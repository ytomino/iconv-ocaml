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
#include <string.h>

/* Tag_some/Val_none are added since OCaml 4.12 */

#if !defined(Tag_some)
#define Tag_some 0
#endif
#if !defined(Val_none)
#define Val_none Val_int(0)
#endif

/* custom data */

struct mliconv_t {
	iconv_t handle;
	char *tocode;
	char *fromcode;
};

static inline struct mliconv_t *mliconv_val(value data)
{
	return (struct mliconv_t *)(Data_custom_val(data));
}

static void mliconv_finalize(value r);
static int mliconv_compare(value left, value right);
static long mliconv_hash(value data);
#if defined(SUPPORT_SERIALIZATION)
static void mliconv_serialize(
	value v, unsigned long *wsize_32, unsigned long *wsize_64);
static unsigned long mliconv_deserialize(void *dst);
#endif

static struct custom_operations iconv_ops = {
	.identifier = "jp.halfmoon.panathenaia.iconv",
	.finalize = mliconv_finalize,
	.compare = mliconv_compare,
	.hash = mliconv_hash,
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
	free(internal->tocode);
	free(internal->fromcode);
	CAMLreturn0;
}

static int mliconv_compare(value left, value right)
{
	CAMLparam2(left, right);
	struct mliconv_t *left_internal = mliconv_val(left);
	struct mliconv_t *right_internal = mliconv_val(right);
	int result = strcmp(left_internal->tocode, right_internal->tocode);
	if(result == 0){
		result = strcmp(left_internal->fromcode, right_internal->fromcode);
	}
	CAMLreturn(result);
}

static long mliconv_hash(value data)
{
	CAMLparam1(data);
	struct mliconv_t *internal = mliconv_val(data);
	long result = (strlen(internal->tocode) << 4) + strlen(internal->fromcode);
	CAMLreturn(result);
}

#if defined(SUPPORT_SERIALIZATION)

static void mliconv_serialize(
	value v, unsigned long *wsize_32, unsigned long *wsize_64)
{
	CAMLparam1(v);
	*wsize_32 = 4 * 3;
	*wsize_64 = 8 * 3;
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
	char *tocode = malloc(to_len + 1);
	caml_deserialize_block_1(tocode, to_len);
	tocode[to_len] = '\0';
	size_t from_len = caml_deserialize_uint_4();
	char *fromcode = malloc(from_len + 1);
	caml_deserialize_block_1(fromcode, from_len);
	fromcode[from_len] = '\0';
	iconv_t handle = iconv_open(tocode, fromcode);
	if(handle == (iconv_t)-1){
		char message[to_len + from_len + 128];
		strcat(
			strcat(strcat(strcpy(message, "failed iconv_open to "), tocode), " from "),
			fromcode);
		free(tocode);
		free(fromcode);
		caml_failwith(message);
	}
	struct mliconv_t *internal = (struct mliconv_t *)dst;
	internal->handle = handle;
	internal->tocode = tocode;
	internal->fromcode = fromcode;
	CAMLreturn(sizeof(struct mliconv_t));
}

/* setup */

__attribute__((constructor)) static void mliconv_register(void)
{
	caml_register_custom_operations(&iconv_ops);
}

#endif

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
#if defined(__GNU_LIBRARY__) && !defined(_LIBICONV_VERSION)
	internal->tocode = strdup(tocode);
	internal->fromcode = strdup(fromcode);
#else
	internal->tocode = strdup(iconv_canonicalize(tocode));
	internal->fromcode = strdup(iconv_canonicalize(fromcode));
#endif
	CAMLreturn(result);
}

CAMLprim value mliconv_convert(value conv, value source)
{
	CAMLparam2(conv, source);
	CAMLlocal1(result);
	struct mliconv_t *internal = mliconv_val(conv);
	/* const */ char *s = (char *)String_val(source);
	size_t s_len = caml_string_length(source);
	size_t d_len = s_len * 6;
	char *d = malloc(d_len);
	char *d_current = d;
	while(s_len > 0){
		if(iconv(internal->handle, &s, &s_len, &d_current, &d_len) == (size_t)-1){
			int e = errno;
			if(e == EILSEQ || e == EINVAL){
				*d_current = '?';
				++ d_current;
				-- d_len;
				++ s;
				-- s_len;
			}else{
				free(d);
				caml_failwith("failed iconv");
			}
		}
	}
	size_t result_len = d_current - d;
	result = caml_alloc_string(result_len);
	memcpy(Bytes_val(result), d, result_len);
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
