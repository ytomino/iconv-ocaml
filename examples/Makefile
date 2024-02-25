BUILD:=$(shell $(CC) -dumpmachine)
HOST=$(BUILD)
TARGET=$(HOST)

PROGRAMPREFIX=$(addsuffix -,$(filter-out $(BUILD),$(TARGET)))

OCAMLC=$(PROGRAMPREFIX)ocamlc
OCAMLOPT=$(or $(filter-out $(OCAMLC),$(OCAMLC:c=opt)),$(PROGRAMPREFIX)ocamlopt)
RLWRAP?=

DEBUG=

OCAMLCFLAGS=$(and $(filter-out 0,$(DEBUG)),-g)
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
OCAMLCFLAGS_EXE=$(and $(filter-out 0,$(DEBUG)),-g)
OCAMLOPTFLAGS_EXE=$(OCAMLCFLAGS_EXE) $(addprefix -ccopt ,$(LDFLAGS))
LDFLAGS?=

SUPPORT_COMPARISON=1
SUPPORT_SERIALIZATION=1

BUILDSUFFIX=.noindex
BUILDDIR=$(TARGET)$(BUILDSUFFIX)
ICONV_BUILDDIR=$(BUILDDIR)/iconv$(BUILDSUFFIX)

BINLN=$(and $(filter $(BUILD),$(TARGET)), \
        $(if $(findstring mingw,$(BUILD))$(findstring msys,$(BUILD)),,bin))

EXAMPLES=$(basename $(wildcard *.ml))
TESTS=$(filter test_%,$(EXAMPLES))

.PHONY: all check interacitve clean $(TESTS)

all: \
	$(and $(OCAMLC),$(patsubst %,$(BUILDDIR)/%.byte.exe,$(EXAMPLES))) \
	$(and $(OCAMLOPT),$(patsubst %,$(BUILDDIR)/%.opt.exe,$(EXAMPLES))) \
	$(BINLN)

$(BUILDDIR)/%.byte.exe: %.ml $(BUILDDIR)/iconv.cma
	$(OCAMLC) -c $(OCAMLCFLAGS) -o $(BUILDDIR)/$*.cmo -I $(BUILDDIR) $<
	$(OCAMLC) $(strip \
		$(OCAMLCFLAGS_EXE) \
		-o $@ -I $(BUILDDIR) iconv.cma $(BUILDDIR)/$*.cmo)

$(BUILDDIR)/%.opt.exe: %.ml $(BUILDDIR)/iconv.cmxa
	$(OCAMLOPT) -c $(OCAMLOPTFLAGS) -o $(BUILDDIR)/$*.cmx -I $(BUILDDIR) $<
	$(OCAMLOPT) $(strip \
		$(OCAMLOPTFLAGS_EXE) \
		-o $@ -I $(BUILDDIR) iconv.cmxa $(BUILDDIR)/$*.cmx)

check: all $(TESTS)

$(TESTS): %: \
		$(and $(OCAMLC),$(BUILDDIR)/%.byte.exe) \
		$(and $(OCAMLOPT),$(BUILDDIR)/%.opt.exe)
	$(and $(OCAMLC),$<)
	$(and $(OCAMLOPT),$(BUILDDIR)/$*.opt.exe)

interactive: $(BUILDDIR)/iconv.cma
	$(RLWRAP) ocaml $(OCAMLCFLAGS) -I $(BUILDDIR) iconv.cma

$(and $(OCAMLC),$(BUILDDIR)/iconv.cma) \
$(and $(OCAMLOPT),$(BUILDDIR)/iconv.cmxa)&: \
		../source/iconv*
	$(MAKE) $(strip \
		-C ../source install \
		BUILDDIR=$(abspath $(ICONV_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)) \
		SUPPORT_COMPARISON=$(SUPPORT_COMPARISON) \
		SUPPORT_SERIALIZATION=$(SUPPORT_SERIALIZATION))

$(BINLN):
	ln -s $(BUILDDIR) $@

clean:
	-$(and $(BINLN),[ -h "$(BINLN)" ] && rm "$(BINLN)")
	-$(MAKE) $(strip \
		-C ../source clean uninstall \
		BUILDDIR=$(abspath $(ICONV_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)))
	-rm $(BUILDDIR)/*.exe $(BUILDDIR)/*.cm? $(BUILDDIR)/*.o
	-rmdir $(BUILDDIR)/stublibs
	-rmdir $(BUILDDIR)
