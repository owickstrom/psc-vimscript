.PHONY: compile
compile:
	stack exec psc-vimscript -- $(shell find output -name 'corefn.json')

.PHONY: purs
purs:
	stack exec purs -- compile 'lib/**/src/**/*.purs' 'test/purs-src/**/*.purs' --dump-corefn

clean:
	rm -rf output vim-output
