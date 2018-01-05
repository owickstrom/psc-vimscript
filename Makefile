compile: vim-output/Array.vim

output/Array/corefn.json: test/purs-src/Array.purs
	stack exec purs -- compile 'test/purs-src/**/*.purs' --dump-corefn

vim-output/Array.vim: output/Array/corefn.json
	stack exec psc-vimscript -- $<

clean:
	rm -rf output vim-output
