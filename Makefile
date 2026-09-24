.PHONY: test clean-test

test:
	dune exec ./test_codegen.exe > out.ll
	clang out.ll runtime.c -o out -Wno-override-module
	./out; echo "exit code: $$?"

clean-test:
	rm -f out.ll out