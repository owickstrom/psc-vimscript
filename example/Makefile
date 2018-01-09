SRCS = $(shell find ../lib/purescript-*/src -name '*.purs') $(shell find src -name '*.purs')
FOREIGNS = $(foreach SRC,$(SRCS),$(wildcard $(SRC:%.purs=%.vim)))

to_module_name = $(shell echo $(SRC) | sed -rn 's/.*src\/(.*)\.purs/\1/p'| sed 's/\//\./g')
MODULES = $(foreach SRC,$(SRCS),$(to_module_name))
COREFNS = $(MODULES:%=output/%/corefn.json)

VIM_PACKAGES = $(foreach MODULE,$(MODULES),$(shell echo "vim-output/pack/purs/opt/$(MODULE)/plugin/vim-output/$(MODULE).vim"))

# This is a hack to only run the psc-vimscript backend once for all the
# corefn.json files.
RECOMPILED_FLAG = output/.recompiled

all: compile
	@echo $(SRCS)

.PHONY: compile
compile: $(RECOMPILED_FLAG) $(FOREIGNS)
	stack exec psc-vimscript -- $(COREFNS)

$(RECOMPILED_FLAG): $(SRCS)
	stack exec purs -- compile $(SRCS) --dump-corefn
	touch output/.recompiled

.PHONY: run
run:
	vim -u run.vim -c 'call g:Main#main()'

clean:
	rm -rf output vim-output
