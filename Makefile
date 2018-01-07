.PHONY: compile
compile:
	stack exec purs -- compile 'lib/**/src/**/*.purs' 'test/purs-src/**/*.purs' --dump-corefn
	stack exec psc-vimscript -- $(shell find output -name 'corefn.json')

clean:
	rm -rf output vim-output
