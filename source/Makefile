include Makefile.variables

MLI=iconv.mli iconv__Out_iconv.mli
MLSRC=$(MLI:.mli=.ml) iconv_pp.ml
MLINIT=iconv_pp_install.ml
CSRC=iconv_stub.c

LIBNAME=iconv
STUBLIBNAME=ml$(LIBNAME)

DEBUG=
ASSERT=

WITH_ICONV?=

OCAMLCFLAGS=$(and $(filter-out 0,$(DEBUG)),-g) $(if $(ASSERT),,-noassert) -w +a
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
OCAML_INCLUDE_FLAGS=
CFLAGS=$(and $(filter-out 0,$(DEBUG)),-g$(DEBUG)) \
       $(and $(ASSERT),-DDEBUG -DNDEBUG) \
       $(and $(SUPPORT_COMPARISON),-DSUPPORT_COMPARISON) \
       $(and $(SUPPORT_SERIALIZATION),-DSUPPORT_SERIALIZATION) \
       -Wall -Wextra -Wno-unused-result
C_INCLUDE_FLAGS=$(and $(WITH_ICONV),-I$(WITH_ICONV)/include)

LD_ICONV=$(or \
           $(and $(WITH_ICONV),-L$(WITH_ICONV)/lib -liconv), \
           $(if $(findstring linux-gnu,$(TARGET)),,-liconv))

CCLIB=$(addprefix -cclib ,$(LD_ICONV))
LDLIB=$(LD_ICONV)

SUPPORT_COMPARISON=
SUPPORT_SERIALIZATION=

include Makefile.rules

$(BUILDDIR)/iconv.cmi $(BUILDDIR)/iconv.cmo $(BUILDDIR)/iconv.cmx: \
        private override OCAMLCFLAGS+=-no-alias-deps -w -49
$(BUILDDIR)/iconv__Out_iconv.cmi $(BUILDDIR)/iconv_pp.cmo: \
	$(BUILDDIR)/iconv.cmi
$(BUILDDIR)/iconv__Out_iconv.cmx $(BUILDDIR)/iconv_pp.cmx: \
	$(BUILDDIR)/iconv.cmx
$(BUILDDIR)/iconv_pp_install.cmo: \
	private override OCAMLCFLAGS+=-I $(OCAMLLIBDIR)/compiler-libs
$(BUILDDIR)/iconv_pp_install.cmo: $(BUILDDIR)/iconv_pp.cmo
