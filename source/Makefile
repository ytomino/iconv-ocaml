include Makefile.variables

MLI=iconv.mli
MLSRC=iconv.ml iconv_pp.ml
MLINIT=iconv_pp_install.ml
CSRC=iconv_stub.c

LIBNAME=iconv
STUBLIBNAME=ml$(LIBNAME)

WITH_ICONV?=
WIFLAGS=$(and $(WITH_ICONV),-I$(WITH_ICONV)/include)
WLFLAGS=$(and $(WITH_ICONV),-L$(WITH_ICONV)/lib)

OCAMLCFLAGS=-w +a
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
CFLAGS=$(WIFLAGS) -Wall -Wextra -Wno-unused-result \
       $(and $(SUPPORT_COMPARISON),-DSUPPORT_COMPARISON) \
       $(and $(SUPPORT_SERIALIZATION),-DSUPPORT_SERIALIZATION)

LDFLAGS=$(WLFLAGS)
LDLIB=$(if $(findstring linux-gnu,$(TARGET)),,-liconv)
CCLIB=$(addprefix -cclib ,$(WLFLAGS) $(LDLIB))

SUPPORT_COMPARISON=
SUPPORT_SERIALIZATION=

include Makefile.rules

$(BUILDDIR)/iconv.cmo $(BUILDDIR)/iconv.cmx: \
	private override OCAMLCFLAGS+=-w -69
$(BUILDDIR)/iconv_pp.cmo: $(BUILDDIR)/iconv.cmi
$(BUILDDIR)/iconv_pp.cmx: $(BUILDDIR)/iconv.cmx
$(BUILDDIR)/iconv_pp_install.cmo: \
	private override OCAMLCFLAGS+=-I $(OCAMLLIBDIR)/compiler-libs
$(BUILDDIR)/iconv_pp_install.cmo: $(BUILDDIR)/iconv_pp.cmo
