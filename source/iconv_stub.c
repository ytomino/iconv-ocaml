#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/signals.h>

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

/* polymorphic variants */

enum {
	Val_auto = -0x7f124121, /* 0x80edbedf */
	Val_illegal_sequence = -0x699b4d2b, /* 0x9664b2d5 */
	Val_ok = 0x0000c239,
	Val_overflow = -0x7d88397b /* 0x8277c685 */
};

/* fields */

struct iconv_field_s {
	char *buf;
	size_t bytesleft;
};

static void set_buf(
	struct iconv_field_s *field, value val_fields, int field_offset,
	ptrdiff_t buf_offset)
{
	field->buf = (char *)Bytes_val(Field(val_fields, field_offset)) + buf_offset;
}

static void set_fields(
	struct iconv_field_s *field, value val_fields, int field_offset)
{
	ptrdiff_t buf_offset = Long_val(Field(val_fields, field_offset + 1));
	set_buf(field, val_fields, field_offset, buf_offset);
	field->bytesleft = Long_val(Field(val_fields, field_offset + 2));
}

static ptrdiff_t get_buf_offset(
	value val_fields, int field_offset, struct iconv_field_s const *field)
{
	return field->buf - (char *)Bytes_val(Field(val_fields, field_offset));
}

static void get_fields(
	value val_fields, int field_offset, struct iconv_field_s const *field)
{
	ptrdiff_t buf_offset = get_buf_offset(val_fields, field_offset, field);
	Store_field(val_fields, field_offset + 1, Val_long(buf_offset));
	Store_field(val_fields, field_offset + 2, Val_long(field->bytesleft));
}

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
	char const *tocode, char *substitute, int_least8_t *substitute_length);
static bool get_unexist(struct mliconv_t *internal);
static void set_unexist(struct mliconv_t *internal, bool ilseq);

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
	if(internal->handle != NULL){
		iconv_close(internal->handle);
	}
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
			if(left_internal->substitute_length < 0){
				get_substitute(
					left_internal->tocode, left_internal->substitute,
					&left_internal->substitute_length);
			}
			if(right_internal->substitute_length < 0){
				get_substitute(
					right_internal->tocode, right_internal->substitute,
					&right_internal->substitute_length);
			}
			int_least8_t min_substitute_length =
				(left_internal->substitute_length < right_internal->substitute_length) ?
				left_internal->substitute_length :
				right_internal->substitute_length;
			result = memcmp(
				left_internal->substitute, right_internal->substitute,
				(size_t)min_substitute_length);
			if(result == 0){
				result = right_internal->substitute_length - left_internal->substitute_length;
				if(result == 0){
					bool left_unexist = get_unexist(left_internal);
					bool right_unexist = get_unexist(right_internal);
					result = right_unexist - left_unexist;
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
	caml_serialize_int_1(get_unexist(internal));
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
	set_unexist(internal, caml_deserialize_uint_1());
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
	char const *tocode, char *substitute, int_least8_t *substitute_length)
{
	char *d = substitute;
	size_t d_len = MAX_SEQUENCE;
	if(convert_one_sequence(tocode, latin1, '?', &d, &d_len) < 0){
		/* error case */
		substitute[0] = '?';
		*substitute_length = 1;
	}else{
		*substitute_length = MAX_SEQUENCE - d_len;
	}
}

static bool get_unexist(
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

static void set_unexist(
	__attribute__((unused)) struct mliconv_t *internal,
	__attribute__((unused)) bool ilseq)
{
#if !defined(_LIBICONV_VERSION) && defined(__FreeBSD__) && __FreeBSD__ >= 10
	int arg = ilseq;
	if(iconvctl(internal->handle, ICONV_SET_ILSEQ_INVALID, &arg) < 0){
		caml_failwith(__func__);
	}
#endif
}

static int put_substitute(
	iconv_t handle, char const *substitute, int_least8_t substitute_length,
	char **outbuf, size_t *outbytesleft)
{
	int result;
	if(substitute_length == 0){
		result = 0;
	}else{
		char *ob2 = *outbuf;
		size_t obl2 = *outbytesleft;
		if(iconv(handle, NULL, NULL, &ob2, &obl2) == (size_t)-1){
			result = -1; /* error */
		}else if(obl2 < (size_t)substitute_length){
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

static int_least8_t get_min_sequence_in_fromcode(char const *fromcode)
{
	int_least8_t min_sequence_in_fromcode;
	char outbuffer[MAX_SEQUENCE];
	char *d = outbuffer;
	size_t d_len = MAX_SEQUENCE;
	if(convert_one_sequence(
		fromcode,
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
	return min_sequence_in_fromcode;
}

static void skip_min_sequence(
	int_least8_t min_sequence_in_fromcode, char **inbuf, size_t *inbytesleft)
{
	if(*inbytesleft < (size_t)min_sequence_in_fromcode){
		*inbuf += *inbytesleft;
		*inbytesleft = 0;
	}else{
		*inbuf += (ptrdiff_t)min_sequence_in_fromcode;
		*inbytesleft -= (ptrdiff_t)min_sequence_in_fromcode;
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
	/* Do caml_alloc_custom at first because _noexc-version does not exist. */
	val_result = caml_alloc_custom(&iconv_ops, sizeof(struct mliconv_t), 0, 1);
	struct mliconv_t *internal = mliconv_val(val_result);
	internal->handle = NULL;
	internal->tocode = NULL;
	internal->fromcode = NULL;
	const char *tocode = String_val(val_tocode);
	size_t to_len = caml_string_length(val_tocode);
	const char *fromcode = String_val(val_fromcode);
	size_t from_len = caml_string_length(val_fromcode);
#if !defined(__GNU_LIBRARY__) || defined(_LIBICONV_VERSION)
	tocode = iconv_canonicalize(tocode);
	fromcode = iconv_canonicalize(fromcode);
#endif
	char *stat_tocode = caml_stat_strdup(tocode);
	internal->tocode = stat_tocode;
	char *stat_fromcode = caml_stat_strdup(fromcode);
	internal->fromcode = stat_fromcode;
	caml_enter_blocking_section();
	iconv_t handle = iconv_open(stat_tocode, stat_fromcode);
	caml_leave_blocking_section();
	if(handle == (iconv_t)-1){
		char message[to_len + from_len + 128];
		strcat(
			strcat(
				strcat(
					strcat(strcpy(message, __func__), ": failed iconv_open to "), stat_tocode),
				" from "),
			stat_fromcode);
		caml_stat_free(stat_tocode);
		caml_stat_free(stat_fromcode);
		caml_failwith(message);
	}
	/* The pointer to OCaml heap cannot be kept across blocking sections. */
	internal = mliconv_val(val_result);
	internal->handle = handle;
	internal->tocode = stat_tocode;
	internal->fromcode = stat_fromcode;
	internal->substitute_length = -1;
	internal->min_sequence_in_fromcode = -1;
	CAMLreturn(val_result);
}

CAMLprim value mliconv_substitute(value val_conv)
{
	CAMLparam1(val_conv);
	CAMLlocal1(val_result);
	struct mliconv_t *internal = mliconv_val(val_conv);
	int_least8_t substitute_length = internal->substitute_length;
	if(substitute_length < 0){
		char *tocode = internal->tocode;
		char substitute[MAX_SEQUENCE];
		caml_enter_blocking_section();
		get_substitute(tocode, substitute, &substitute_length);
		caml_leave_blocking_section();
		/* The pointer to OCaml heap cannot be kept across blocking sections. */
		internal = mliconv_val(val_conv);
		memcpy(internal->substitute, substitute, substitute_length);
		internal->substitute_length = substitute_length;
	}
	val_result = caml_alloc_initialized_string(
		substitute_length, internal->substitute);
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

CAMLprim value mliconv_unexist(value val_conv)
{
	CAMLparam1(val_conv);
	CAMLlocal1(val_result);
	struct mliconv_t *internal = mliconv_val(val_conv);
	val_result = get_unexist(internal) ? Val_illegal_sequence : Val_auto;
	CAMLreturn(val_result);
}

CAMLprim value mliconv_set_unexist(value val_conv, value val_x)
{
	CAMLparam2(val_conv, val_x);
	struct mliconv_t *internal = mliconv_val(val_conv);
	set_unexist(internal, val_x == Val_illegal_sequence);
	CAMLreturn(Val_unit);
}

CAMLprim value mliconv_min_sequence_in_fromcode(value val_conv)
{
	CAMLparam1(val_conv);
	struct mliconv_t *internal = mliconv_val(val_conv);
	int_least8_t result = internal->min_sequence_in_fromcode;
	if(result < 0){
		char *fromcode = internal->fromcode;
		caml_enter_blocking_section();
		result = get_min_sequence_in_fromcode(fromcode);
		caml_leave_blocking_section();
		/* The pointer to OCaml heap cannot be kept across blocking sections. */
		internal = mliconv_val(val_conv);
		internal->min_sequence_in_fromcode = result;
	}
	CAMLreturn(Val_long((long)result));
}

/* converting functions */

CAMLprim value mliconv_unsafe_iconv(
	value val_conv, value val_fields, value val_finish)
{
	CAMLparam3(val_conv, val_fields, val_finish);
	CAMLlocal1(val_result);
	val_result = Val_ok;
	struct mliconv_t *internal = mliconv_val(val_conv);
	struct iconv_field_s in, out;
	set_fields(&in, val_fields, 0);
	set_fields(&out, val_fields, 3);
	while(in.bytesleft > 0){
		if(iconv(internal->handle, &in.buf, &in.bytesleft, &out.buf, &out.bytesleft)
			== (size_t)-1)
		{
			int e = errno;
			if(e == E2BIG){
				val_result = Val_overflow;
				break;
			}else if(e == EINVAL && !Bool_val(val_finish)){ /* truncated */
				break;
			}else if(e == EILSEQ || e == EINVAL){
				val_result = Val_illegal_sequence;
				break;
			}else{
				caml_failwith(__func__);
			}
		}
	}
	get_fields(val_fields, 0, &in);
	get_fields(val_fields, 3, &out);
	CAMLreturn(val_result);
}

CAMLprim value mliconv_unsafe_iconv_substitute(
	value val_conv, value val_fields, value val_finish)
{
	CAMLparam3(val_conv, val_fields, val_finish);
	CAMLlocal1(val_result);
	val_result = Val_ok;
	struct mliconv_t *internal = mliconv_val(val_conv);
	struct iconv_field_s in, out;
	set_fields(&in, val_fields, 0);
	set_fields(&out, val_fields, 3);
	while(in.bytesleft > 0){
		if(iconv(internal->handle, &in.buf, &in.bytesleft, &out.buf, &out.bytesleft)
			== (size_t)-1)
		{
			int e = errno;
			if(e == E2BIG){
				val_result = Val_overflow;
				break;
			}else if(e == EINVAL && !Bool_val(val_finish)){ /* truncated */
				break;
			}else if(e == EILSEQ || e == EINVAL){
				int_least8_t substitute_length = internal->substitute_length;
				int_least8_t min_sequence_in_fromcode = internal->min_sequence_in_fromcode;
				if(substitute_length < 0 || min_sequence_in_fromcode < 0){
					intptr_t inbuf_offset = get_buf_offset(val_fields, 0, &in);
					intptr_t outbuf_offset = get_buf_offset(val_fields, 3, &out);
					char *tocode = internal->tocode;
					char *fromcode = internal->fromcode;
					char substitute[MAX_SEQUENCE];
					caml_enter_blocking_section();
					if(substitute_length < 0){
						get_substitute(tocode, substitute, &substitute_length);
					}
					if(min_sequence_in_fromcode < 0){
						min_sequence_in_fromcode = get_min_sequence_in_fromcode(fromcode);
					}
					caml_leave_blocking_section();
					/* The pointer to OCaml heap cannot be kept across blocking sections. */
					internal = mliconv_val(val_conv);
					if(internal->substitute_length < 0){
						internal->substitute_length = substitute_length;
						memcpy(internal->substitute, substitute, substitute_length);
					}
					if(internal->min_sequence_in_fromcode < 0){
						internal->min_sequence_in_fromcode = min_sequence_in_fromcode;
					}
					set_buf(&in, val_fields, 0, inbuf_offset);
					set_buf(&out, val_fields, 3, outbuf_offset);
				}
				if(
					put_substitute(
						internal->handle, internal->substitute, substitute_length, &out.buf,
						&out.bytesleft)
					< 0)
				{
					e = errno;
					if(e == E2BIG){
						val_result = Val_overflow;
						break;
					}else{
						caml_failwith(__func__);
					}
				}
				skip_min_sequence(min_sequence_in_fromcode, &in.buf, &in.bytesleft);
			}else{
				caml_failwith(__func__);
			}
		}
	}
	get_fields(val_fields, 0, &in);
	get_fields(val_fields, 3, &out);
	CAMLreturn(val_result);
}

CAMLprim value mliconv_unsafe_iconv_end(value val_conv, value val_fields)
{
	CAMLparam2(val_conv, val_fields);
	CAMLlocal1(val_result);
	val_result = Val_ok;
	struct mliconv_t *internal = mliconv_val(val_conv);
	struct iconv_field_s out;
	set_fields(&out, val_fields, 3);
	if(iconv(internal->handle, NULL, NULL, &out.buf, &out.bytesleft)
		== (size_t)-1)
	{
		int e = errno;
		if(e == E2BIG){
			val_result = Val_overflow;
		}else{
			caml_failwith(__func__);
		}
	}
	get_fields(val_fields, 3, &out);
	CAMLreturn(val_result);
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
	/* const */ char *s = (char *)String_val(val_source);
	/* const */ char *s_current = s + Long_val(val_pos);
	char *d = (char *)Bytes_val(val_d);
	char *d_current = d;
	bool failed = false;
	while(s_len > 0){
		if(iconv(internal->handle, &s_current, &s_len, &d_current, &d_len)
			== (size_t)-1)
		{
			int e = errno;
			if(e == EILSEQ || e == EINVAL){
				int_least8_t substitute_length = internal->substitute_length;
				int_least8_t min_sequence_in_fromcode = internal->min_sequence_in_fromcode;
				if(substitute_length < 0 || min_sequence_in_fromcode < 0){
					intptr_t inbuf_offset = s_current - s;
					intptr_t outbuf_offset = d_current - d;
					char *tocode = internal->tocode;
					char *fromcode = internal->fromcode;
					char substitute[MAX_SEQUENCE];
					caml_enter_blocking_section();
					if(substitute_length < 0){
						get_substitute(tocode, substitute, &substitute_length);
					}
					if(min_sequence_in_fromcode < 0){
						min_sequence_in_fromcode = get_min_sequence_in_fromcode(fromcode);
					}
					caml_leave_blocking_section();
					/* The pointer to OCaml heap cannot be kept across blocking sections. */
					internal = mliconv_val(val_conv);
					if(internal->substitute_length < 0){
						internal->substitute_length = substitute_length;
						memcpy(internal->substitute, substitute, substitute_length);
					}
					if(internal->min_sequence_in_fromcode < 0){
						internal->min_sequence_in_fromcode = min_sequence_in_fromcode;
					}
					s = (char *)String_val(val_source);
					d = (char *)Bytes_val(val_d);
					s_current = s + inbuf_offset;
					d_current = d + outbuf_offset;
				}
				if(
					put_substitute(
						internal->handle, internal->substitute, substitute_length, &d_current, &d_len)
					< 0)
				{
					/* like E2BIG */
					failed = true;
					break;
				}
				skip_min_sequence(min_sequence_in_fromcode, &s_current, &s_len);
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
