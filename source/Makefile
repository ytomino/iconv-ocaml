include Makefile.variables

MLI=iconv.mli
MLSRC=iconv.ml iconv_pp.ml
MLINIT=iconv_pp_install.ml
CSRC=iconv_stub.c

LIBNAME=iconv
STUBLIBNAME=ml$(LIBNAME)

WIFLAGS=
WLFLAGS=
ifneq ($(WITH_ICONV),)
 WIFLAGS+=-I$(WITH_ICONV)/include
 WLFLAGS+=-L$(WITH_ICONV)/lib
endif

OCAMLCFLAGS=-w +a
CFLAGS=$(WIFLAGS) -Wall -Wextra -Wno-unused-result

LDFLAGS=$(WLFLAGS)
LDLIB=
ifeq ($(findstring linux-gnu,$(TARGET)),)
 LDLIB+=-liconv
endif
CCLIB=$(addprefix -cclib ,$(LDFLAGS) $(LDLIB))

SUPPORT_SERIALIZATION=
ifneq ($(SUPPORT_SERIALIZATION),)
	CFLAGS+=-DSUPPORT_SERIALIZATION
endif

include Makefile.rules

$(BUILDDIR)/iconv_pp.cmo: $(BUILDDIR)/iconv.cmi
$(BUILDDIR)/iconv_pp.cmx: $(BUILDDIR)/iconv.cmx
$(BUILDDIR)/iconv_pp_install.cmo $(BUILDDIR)/iconv_pp_install.cmi: \
	private override OCAMLCFLAGS+=-I $(OCAMLLIBDIR)/compiler-libs
$(BUILDDIR)/iconv_pp_install.cmo: $(BUILDDIR)/iconv_pp.cmi
