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

#define MAX_SEQUENCE 6

/* custom data */

struct mliconv_t {
	iconv_t handle;
	char *tocode;
	char *fromcode;
	char substitute[MAX_SEQUENCE];
	int_least8_t substitute_length;
	int_least8_t min_sequence_in_fromcode;
};

#define WSIZE_32_MLICONV (4 * 5)
#define WSIZE_64_MLICONV (8 * 4)

static inline struct mliconv_t *mliconv_val(value data)
{
	return (struct mliconv_t *)(Data_custom_val(data));
}

static void get_substitute(
	struct mliconv_t *internal,
	char const **substitute,
	size_t *substitute_length);

static void mliconv_finalize(value r);
#if defined(SUPPORT_COMPARISON)
static int mliconv_compare(value left, value right);
static long mliconv_hash(value data);
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

static void mliconv_finalize(value data)
{
	CAMLparam1(data);
	struct mliconv_t *internal = mliconv_val(data);
	iconv_close(internal->handle);
	caml_stat_free(internal->tocode);
	caml_stat_free(internal->fromcode);
	CAMLreturn0;
}

#if defined(SUPPORT_COMPARISON)

static int mliconv_compare(value left, value right)
{
	CAMLparam2(left, right);
	struct mliconv_t *left_internal = mliconv_val(left);
	struct mliconv_t *right_internal = mliconv_val(right);
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
			}
		}
	}
	CAMLreturnT(int, result);
}

static long mliconv_hash(value data)
{
	CAMLparam1(data);
	struct mliconv_t *internal = mliconv_val(data);
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
			strcat(strcat(strcpy(message, "failed iconv_open to "), tocode), " from "),
			fromcode);
		caml_stat_free(tocode);
		caml_stat_free(fromcode);
		caml_failwith(message);
	}
	struct mliconv_t *internal = (struct mliconv_t *)dst;
	internal->handle = handle;
	internal->tocode = tocode;
	internal->fromcode = fromcode;
	internal->substitute_length = -1;
	internal->min_sequence_in_fromcode = -1;
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

/* version functions */

CAMLprim value mliconv_get_version_opt(void)
{
	CAMLparam0();
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

/* converting functions */

CAMLprim value mliconv_open(value tocodev, value fromcodev)
{
	CAMLparam2(tocodev, fromcodev);
	CAMLlocal1(result);
	const char *tocode = String_val(tocodev);
	size_t to_len = caml_string_length(tocodev);
	const char *fromcode = String_val(fromcodev);
	size_t from_len = caml_string_length(fromcodev);
	iconv_t handle = iconv_open(tocode, fromcode);
	if(handle == (iconv_t)-1){
		char message[to_len + from_len + 128];
		strcat(
			strcat(strcat(strcpy(message, "failed iconv_open to "), tocode), " from "),
			fromcode);
		caml_failwith(message);
	}
	result = caml_alloc_custom(&iconv_ops, sizeof(struct mliconv_t), 0, 1);
	struct mliconv_t *internal = mliconv_val(result);
	internal->handle = handle;
	internal->tocode = NULL; /* for the case that caml_stat_strdup fails */
	internal->fromcode = NULL; /* same as above */
#if !defined(__GNU_LIBRARY__) || defined(_LIBICONV_VERSION)
	tocode = iconv_canonicalize(tocode);
	fromcode = iconv_canonicalize(fromcode);
#endif
	internal->tocode = caml_stat_strdup(tocode);
	internal->fromcode = caml_stat_strdup(fromcode);
	internal->substitute_length = -1;
	internal->min_sequence_in_fromcode = -1;
	CAMLreturn(result);
}

CAMLprim value mliconv_convert(value conv, value source)
{
	CAMLparam2(conv, source);
	CAMLlocal1(result);
	struct mliconv_t *internal = mliconv_val(conv);
	/* const */ char *s = (char *)String_val(source);
	size_t s_len = caml_string_length(source);
	size_t d_len = s_len * MAX_SEQUENCE;
	char *d = malloc(d_len);
	char *d_current = d;
	while(s_len > 0){
		if(iconv(internal->handle, &s, &s_len, &d_current, &d_len) == (size_t)-1){
			int e = errno;
			if(e == EILSEQ || e == EINVAL){
				char const *substitute;
				size_t substitute_length;
				get_substitute(internal, &substitute, &substitute_length);
				if(d_len < substitute_length){
					/* like E2BIG */
					free(d);
					caml_failwith("failed iconv");
				}else{
					memcpy(d_current, substitute, substitute_length);
					d_current += substitute_length;
					d_len -= substitute_length;
				}
				size_t min_sequence_in_fromcode = get_min_sequence_in_fromcode(internal);
				if(s_len < min_sequence_in_fromcode){
					s += s_len;
					s_len = 0;
				}else{
					s += min_sequence_in_fromcode;
					s_len -= min_sequence_in_fromcode;
				}
			}else{
				free(d);
				caml_failwith("failed iconv");
			}
		}
	}
	size_t result_len = d_current - d;
	result = caml_alloc_initialized_string(result_len, d);
	free(d);
	CAMLreturn(result);
}

CAMLprim value mliconv_tocode(value conv)
{
	CAMLparam1(conv);
	CAMLlocal1(result);
	struct mliconv_t *internal = mliconv_val(conv);
	result = caml_copy_string(internal->tocode);
	CAMLreturn(result);
}

CAMLprim value mliconv_fromcode(value conv)
{
	CAMLparam1(conv);
	CAMLlocal1(result);
	struct mliconv_t *internal = mliconv_val(conv);
	result = caml_copy_string(internal->fromcode);
	CAMLreturn(result);
}

CAMLprim value mliconv_min_sequence_in_fromcode(value val_conv)
{
	CAMLparam1(val_conv);
	struct mliconv_t *internal = mliconv_val(val_conv);
	size_t result = get_min_sequence_in_fromcode(internal);
	CAMLreturn(Val_long((long)result));
}
