#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/intext.h>

#include <errno.h>
#include <iconv.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

/* Tag_some/Val_none are added since OCaml 4.12 */

#if !defined(Tag_some)
#define Tag_some 0
#endif
#if !defined(Val_none)
#define Val_none Val_int(0)
#endif

/* characteristics */

#define MAX_SEQUENCE 8
/* For example: ISO-8859-1 "\xA2" ("¢") is converted to
   ISO-2022-JP "\x1B\x24\x42\x21\x71\x1B\x28\x42". */

/* custom data */

struct mliconv_t {
	iconv_t handle;
	char *tocode;
	char *fromcode;
	char substitute[MAX_SEQUENCE];
	int_least8_t substitute_length;
	int_least8_t min_sequence_in_fromcode;
};

#define WSIZE_32_MLICONV (4 * 6)
#define WSIZE_64_MLICONV (8 * 5)

static inline struct mliconv_t *mliconv_val(value v)
{
	return (struct mliconv_t *)(Data_custom_val(v));
}

static void get_substitute(
	struct mliconv_t *internal,
	char const **substitute,
	size_t *substitute_length);
static bool get_force_substitute(struct mliconv_t *internal);
static void set_force_substitute(struct mliconv_t *internal, bool enabled);

static void mliconv_finalize(value v);
#if defined(SUPPORT_COMPARISON)
static int mliconv_compare(value v1, value v2);
static long mliconv_hash(value v);
#endif
#if defined(SUPPORT_SERIALIZATION)
static void mliconv_serialize(
	value v, unsigned long *wsize_32, unsigned long *wsize_64);
static unsigned long mliconv_deserialize(void *dst);
#endif

static struct custom_operations iconv_ops = {
	.identifier = "jp.halfmoon.panathenaia.iconv",
	.finalize = mliconv_finalize,
#if defined(SUPPORT_COMPARISON)
	.compare = mliconv_compare,
	.hash = mliconv_hash,
#else
	.compare = custom_compare_default,
	.hash = custom_hash_default,
#endif
#if defined(SUPPORT_SERIALIZATION)
	.serialize = mliconv_serialize,
	.deserialize = mliconv_deserialize};
#else
	.serialize = custom_serialize_default,
	.deserialize = custom_deserialize_default};
#endif

static void mliconv_finalize(value v)
{
	CAMLparam1(v);
	struct mliconv_t *internal = mliconv_val(v);
	iconv_close(internal->handle);
	caml_stat_free(internal->tocode);
	caml_stat_free(internal->fromcode);
	CAMLreturn0;
}

#if defined(SUPPORT_COMPARISON)

static int mliconv_compare(value v1, value v2)
{
	CAMLparam2(v1, v2);
	struct mliconv_t *left_internal = mliconv_val(v1);
	struct mliconv_t *right_internal = mliconv_val(v2);
	int result = strcmp(left_internal->tocode, right_internal->tocode);
	if(result == 0){
		result = strcmp(left_internal->fromcode, right_internal->fromcode);
		if(result == 0){
			char const *left_substitute;
			size_t left_substitute_length;
			char const *right_substitute;
			size_t right_substitute_length;
			get_substitute(left_internal, &left_substitute, &left_substitute_length);
			get_substitute(right_internal, &right_substitute, &right_substitute_length);
			size_t min_substitute_length =
				(left_substitute_length < right_substitute_length) ? left_substitute_length :
				right_substitute_length;
			result = memcmp(left_substitute, right_substitute, min_substitute_length);
			if(result == 0){
				result = right_substitute_length - left_substitute_length;
				if(result == 0){
					bool left_force_substitute = get_force_substitute(left_internal);
					bool right_force_substitute = get_force_substitute(right_internal);
					result = right_force_substitute - left_force_substitute;
				}
			}
		}
	}
	CAMLreturnT(int, result);
}

static long mliconv_hash(value v)
{
	CAMLparam1(v);
	struct mliconv_t *internal = mliconv_val(v);
	long result = (strlen(internal->tocode) << 4) + strlen(internal->fromcode);
	CAMLreturnT(long, result);
}

#endif

#if defined(SUPPORT_SERIALIZATION)

static void mliconv_serialize(
	value v, unsigned long *wsize_32, unsigned long *wsize_64)
{
	CAMLparam1(v);
	*wsize_32 = WSIZE_32_MLICONV;
	*wsize_64 = WSIZE_64_MLICONV;
	struct mliconv_t *internal = mliconv_val(v);
	size_t to_len = strlen(internal->tocode);
	caml_serialize_int_4(to_len);
	caml_serialize_block_1(internal->tocode, to_len);
	size_t from_len = strlen(internal->fromcode);
	caml_serialize_int_4(from_len);
	caml_serialize_block_1(internal->fromcode, from_len);
	caml_serialize_int_1(internal->substitute_length);
	if(internal->substitute_length > 0){
		caml_serialize_block_1(internal->substitute, internal->substitute_length);
	}
	caml_serialize_int_1(get_force_substitute(internal));
	CAMLreturn0;
}

static unsigned long mliconv_deserialize(void *dst)
{
	CAMLparam0();
	size_t to_len = caml_deserialize_uint_4();
	char *tocode = caml_stat_alloc(to_len + 1);
	caml_deserialize_block_1(tocode, to_len);
	tocode[to_len] = '\0';
	size_t from_len = caml_deserialize_uint_4();
	char *fromcode = caml_stat_alloc_noexc(from_len + 1);
	if(fromcode == NULL){
		caml_stat_free(tocode);
		caml_raise_out_of_memory();
	}
	caml_deserialize_block_1(fromcode, from_len);
	fromcode[from_len] = '\0';
	iconv_t handle = iconv_open(tocode, fromcode);
	if(handle == (iconv_t)-1){
		char message[to_len + from_len + 128];
		strcat(
			strcat(
				strcat(strcat(strcpy(message, __func__), ": failed iconv_open to "), tocode),
				" from "),
			fromcode);
		caml_stat_free(tocode);
		caml_stat_free(fromcode);
		caml_failwith(message);
	}
	struct mliconv_t *internal = (struct mliconv_t *)dst;
	internal->handle = handle;
	internal->tocode = tocode;
	internal->fromcode = fromcode;
	internal->substitute_length = caml_deserialize_sint_1();
	if(internal->substitute_length > 0){
		caml_deserialize_block_1(internal->substitute, internal->substitute_length);
	}
	internal->min_sequence_in_fromcode = -1;
	set_force_substitute(internal, caml_deserialize_uint_1());
	CAMLreturnT(unsigned long, sizeof(struct mliconv_t));
}

/* setup */

__attribute__((constructor)) static void mliconv_register(void)
{
	caml_register_custom_operations(&iconv_ops);
}

#endif

static int convert_one_sequence(
	char const *tocode,
	char const *fromcode,
	char in,
	char **outbuf,
	size_t *outbytesleft)
{
	int result;
	iconv_t handle = iconv_open(tocode, fromcode);
	if(handle == (iconv_t)-1){
		result = -1; /* error */
	}else{
		char inbuffer[2] = {in, '\0'};
		char *inbuf = inbuffer;
		size_t inbytesleft = 1;
		result = 0;
		while(inbytesleft > 0){
			if(iconv(handle, &inbuf, &inbytesleft, outbuf, outbytesleft) == (size_t)-1){
				result = -1; /* error */
				break;
			}
		}
		iconv_close(handle);
	}
	return result;
}

static char latin1[] = "ISO-8859-1";

static void get_substitute(
	struct mliconv_t *internal,
	char const **substitute,
	size_t *substitute_length)
{
	int_least8_t result_substitute_length = internal->substitute_length;
	if(result_substitute_length < 0){
		char *d = internal->substitute;
		size_t d_len = MAX_SEQUENCE;
		if(convert_one_sequence(internal->tocode, latin1, '?', &d, &d_len) < 0){
			/* error case */
			internal->substitute[0] = '?';
			result_substitute_length = 1;
		}else{
			result_substitute_length = MAX_SEQUENCE - d_len;
		}
		internal->substitute_length = result_substitute_length;
	}
	*substitute = internal->substitute;
	*substitute_length = result_substitute_length;
}

static bool get_force_substitute(
	__attribute__((unused)) struct mliconv_t *internal)
{
	bool result;
#if !defined(_LIBICONV_VERSION) && defined(__FreeBSD__) && __FreeBSD__ >= 10
	int arg;
	if(iconvctl(internal->handle, ICONV_GET_ILSEQ_INVALID, &arg) < 0){
		caml_failwith(__func__);
	}else{
		result = arg;
	}
#else
	result = true;
#endif
	return result;
}

static void set_force_substitute(
	__attribute__((unused)) struct mliconv_t *internal,
	__attribute__((unused)) bool enabled)
{
#if !defined(_LIBICONV_VERSION) && defined(__FreeBSD__) && __FreeBSD__ >= 10
	int arg = enabled;
	if(iconvctl(internal->handle, ICONV_SET_ILSEQ_INVALID, &arg) < 0){
		caml_failwith(__func__);
	}
#endif
}

static int put_substitute(
	struct mliconv_t *internal, char **outbuf, size_t *outbytesleft)
{
	int result;
	char const *substitute;
	size_t substitute_length;
	get_substitute(internal, &substitute, &substitute_length);
	if(substitute_length == 0){
		result = 0;
	}else{
		char *ob2 = *outbuf;
		size_t obl2 = *outbytesleft;
		if(iconv(internal->handle, NULL, NULL, &ob2, &obl2) == (size_t)-1){
			result = -1; /* error */
		}else if(obl2 < substitute_length){
			errno = E2BIG;
			result = -1; /* error */
		}else{
			memcpy(ob2, substitute, substitute_length);
			ob2 += substitute_length;
			obl2 -= substitute_length;
			*outbuf = ob2;
			*outbytesleft = obl2;
			result = 0;
		}
	}
	return result;
}

static size_t get_min_sequence_in_fromcode(struct mliconv_t *internal)
{
	int_least8_t min_sequence_in_fromcode = internal->min_sequence_in_fromcode;
	if(min_sequence_in_fromcode < 0){
		char outbuffer[MAX_SEQUENCE];
		char *d = outbuffer;
		size_t d_len = MAX_SEQUENCE;
		if(convert_one_sequence(
			internal->fromcode,
			latin1,
			'\0', /* assume '\0' is minimal in all encodings */
			&d,
			&d_len) < 0)
		{
			/* error case */
			min_sequence_in_fromcode = 1;
		}else{
			size_t used = MAX_SEQUENCE - d_len;
			min_sequence_in_fromcode = (used > 0) ? used : 1;
		}
		internal->min_sequence_in_fromcode = min_sequence_in_fromcode;
	}
	return min_sequence_in_fromcode;
}

static void skip_min_sequence(
	struct mliconv_t *internal, char **inbuf, size_t *inbytesleft)
{
	size_t min_sequence_in_fromcode = get_min_sequence_in_fromcode(internal);
	if(*inbytesleft < min_sequence_in_fromcode){
		*inbuf += *inbytesleft;
		*inbytesleft = 0;
	}else{
		*inbuf += min_sequence_in_fromcode;
		*inbytesleft -= min_sequence_in_fromcode;
	}
}

/* version functions */

CAMLprim value mliconv_get_version_opt(value val_unit)
{
	CAMLparam1(val_unit);
#if defined(_LIBICONV_VERSION)
	CAMLlocal2(val_result, val_tuple);
	int version = _libiconv_version;
	val_tuple = caml_alloc_tuple(2);
	Store_field(val_tuple, 0, Val_int(version >> 8)); /* major */
	Store_field(val_tuple, 1, Val_int(version & 0xFF)); /* minor */
	val_result = caml_alloc_small(1, Tag_some);
	Store_field(val_result, 0, val_tuple);
#else
	CAMLlocal1(val_result);
	val_result = Val_none;
#endif
	CAMLreturn(val_result);
}

/* open and setting functions */

CAMLprim value mliconv_open(value val_tocode, value val_fromcode)
{
	CAMLparam2(val_tocode, val_fromcode);
	CAMLlocal1(val_result);
	const char *tocode = String_val(val_tocode);
	size_t to_len = caml_string_length(val_tocode);
	const char *fromcode = String_val(val_fromcode);
	size_t from_len = caml_string_length(val_fromcode);
	iconv_t handle = iconv_open(tocode, fromcode);
	if(handle == (iconv_t)-1){
		char message[to_len + from_len + 128];
		strcat(
			strcat(
				strcat(strcat(strcpy(message, __func__), ": failed iconv_open to "), tocode),
				" from "),
			fromcode);
		caml_failwith(message);
	}
	val_result = caml_alloc_custom(&iconv_ops, sizeof(struct mliconv_t), 0, 1);
	struct mliconv_t *internal = mliconv_val(val_result);
	internal->handle = handle;
	internal->tocode = NULL; /* for the case that caml_stat_strdup fails */
	internal->fromcode = NULL; /* same as above */
	/* Recall String_val because caml_alloc_custom can occur heap compaction. */
	tocode = String_val(val_tocode);
	fromcode = String_val(val_fromcode);
#if !defined(__GNU_LIBRARY__) || defined(_LIBICONV_VERSION)
	tocode = iconv_canonicalize(tocode);
	fromcode = iconv_canonicalize(fromcode);
#endif
	internal->tocode = caml_stat_strdup(tocode);
	internal->fromcode = caml_stat_strdup(fromcode);
	internal->substitute_length = -1;
	internal->min_sequence_in_fromcode = -1;
	CAMLreturn(val_result);
}

CAMLprim value mliconv_substitute(value val_conv)
{
	CAMLparam1(val_conv);
	CAMLlocal1(val_result);
	struct mliconv_t *internal = mliconv_val(val_conv);
	char const *substitute;
	size_t substitute_length;
	get_substitute(internal, &substitute, &substitute_length);
	val_result = caml_alloc_initialized_string(substitute_length, substitute);
	CAMLreturn(val_result);
}

CAMLprim value mliconv_set_substitute(value val_conv, value val_substitute)
{
	CAMLparam2(val_conv, val_substitute);
	size_t substitute_length = caml_string_length(val_substitute);
	if(substitute_length > MAX_SEQUENCE){
		caml_invalid_argument(__func__); /* too long */
	}
	struct mliconv_t *internal = mliconv_val(val_conv);
	char const *substitute = (char *)String_val(val_substitute);
	internal->substitute_length = substitute_length;
	memcpy(internal->substitute, substitute, substitute_length);
	CAMLreturn(Val_unit);
}

CAMLprim value mliconv_force_substitute(value val_conv)
{
	CAMLparam1(val_conv);
	struct mliconv_t *internal = mliconv_val(val_conv);
	bool result = get_force_substitute(internal);
	CAMLreturn(Val_bool(result));
}

CAMLprim value mliconv_set_force_substitute(value val_conv, value val_enabled)
{
	CAMLparam2(val_conv, val_enabled);
	struct mliconv_t *internal = mliconv_val(val_conv);
	bool enabled = Bool_val(val_enabled);
	set_force_substitute(internal, enabled);
	CAMLreturn(Val_unit);
}

CAMLprim value mliconv_min_sequence_in_fromcode(value val_conv)
{
	CAMLparam1(val_conv);
	struct mliconv_t *internal = mliconv_val(val_conv);
	size_t result = get_min_sequence_in_fromcode(internal);
	CAMLreturn(Val_long((long)result));
}

/* converting functions */

CAMLprim value mliconv_iconv_substitute(
	value val_conv, value val_fields, value val_finish)
{
	CAMLparam3(val_conv, val_fields, val_finish);
	bool result = true;
	struct mliconv_t *internal = mliconv_val(val_conv);
	char *inbuf_start = (char *)String_val(Field(val_fields, 0));
	char *inbuf = inbuf_start + Long_val(Field(val_fields, 1));
	size_t inbytesleft = Long_val(Field(val_fields, 2));
	char *outbuf_start = (char *)Bytes_val(Field(val_fields, 3));
	char *outbuf = outbuf_start + Long_val(Field(val_fields, 4));
	size_t outbytesleft = Long_val(Field(val_fields, 5));
	while(inbytesleft > 0){
		if(iconv(internal->handle, &inbuf, &inbytesleft, &outbuf, &outbytesleft)
			== (size_t)-1)
		{
			int e = errno;
			if(e == E2BIG){
				result = false;
				break;
			}else if(e == EINVAL && !Bool_val(val_finish)){ /* truncated */
				break;
			}else if(e == EILSEQ || e == EINVAL){
				if(put_substitute(internal, &outbuf, &outbytesleft) < 0){
					e = errno;
					if(e == E2BIG){
						result = false;
						break;
					}else{
						caml_failwith(__func__);
					}
				}
				skip_min_sequence(internal, &inbuf, &inbytesleft);
			}else{
				caml_failwith(__func__);
			}
		}
	}
	Store_field(val_fields, 1, Val_long(inbuf - inbuf_start));
	Store_field(val_fields, 2, Val_long(inbytesleft));
	Store_field(val_fields, 4, Val_long(outbuf - outbuf_start));
	Store_field(val_fields, 5, Val_long(outbytesleft));
	CAMLreturn(Val_bool(result));
}

CAMLprim value mliconv_iconv_end(value val_conv, value val_fields)
{
	CAMLparam2(val_conv, val_fields);
	bool result = true;
	struct mliconv_t *internal = mliconv_val(val_conv);
	char *outbuf_start = (char *)Bytes_val(Field(val_fields, 3));
	char *outbuf = outbuf_start + Long_val(Field(val_fields, 4));
	size_t outbytesleft = Long_val(Field(val_fields, 5));
	if(iconv(internal->handle, NULL, NULL, &outbuf, &outbytesleft) == (size_t)-1){
		int e = errno;
		if(e == E2BIG){
			result = false;
		}else{
			caml_failwith(__func__);
		}
	}
	Store_field(val_fields, 4, Val_long(outbuf - outbuf_start));
	Store_field(val_fields, 5, Val_long(outbytesleft));
	CAMLreturn(Val_bool(result));
}

CAMLprim value mliconv_iconv_reset(value val_conv)
{
	CAMLparam1(val_conv);
	struct mliconv_t *internal = mliconv_val(val_conv);
	if(iconv(internal->handle, NULL, NULL, NULL, NULL) == (size_t)-1){
		caml_failwith(__func__);
	}
	CAMLreturn(Val_unit);
}

CAMLprim value mliconv_unsafe_iconv_substring(
	value val_conv, value val_source, value val_pos, value val_len)
{
	CAMLparam2(val_conv, val_source);
	CAMLlocal2(val_result, val_d);
	size_t s_len = Long_val(val_len);
	size_t d_len = s_len * MAX_SEQUENCE;
	val_d = caml_alloc_string(d_len);
	struct mliconv_t *internal = mliconv_val(val_conv);
	/* const */ char *s = (char *)String_val(val_source) + Long_val(val_pos);
	char *d = (char *)Bytes_val(val_d);
	char *d_current = d;
	bool failed = false;
	while(s_len > 0){
		if(iconv(internal->handle, &s, &s_len, &d_current, &d_len) == (size_t)-1){
			int e = errno;
			if(e == EILSEQ || e == EINVAL){
				if(put_substitute(internal, &d_current, &d_len) < 0){
					/* like E2BIG */
					failed = true;
					break;
				}
				skip_min_sequence(internal, &s, &s_len);
			}else{
				failed = true;
				break;
			}
		}
	}
	if(!failed
		&& iconv(internal->handle, NULL, NULL, &d_current, &d_len) == (size_t)-1)
	{
		failed = true;
	}
	if(failed){
		iconv(internal->handle, NULL, NULL, NULL, NULL);
		caml_failwith(__func__);
	}
	size_t result_len = d_current - d;
	val_result = caml_alloc_initialized_string(result_len, d);
	CAMLreturn(val_result);
}

/* for pretty printer */

CAMLprim value mliconv_tocode(value val_conv)
{
	CAMLparam1(val_conv);
	CAMLlocal1(val_result);
	struct mliconv_t *internal = mliconv_val(val_conv);
	val_result = caml_copy_string(internal->tocode);
	CAMLreturn(val_result);
}

CAMLprim value mliconv_fromcode(value val_conv)
{
	CAMLparam1(val_conv);
	CAMLlocal1(val_result);
	struct mliconv_t *internal = mliconv_val(val_conv);
	val_result = caml_copy_string(internal->fromcode);
	CAMLreturn(val_result);
}
