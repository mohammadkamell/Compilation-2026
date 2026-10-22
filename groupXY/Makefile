.PHONY: test clean-test

test:
	dune exec ./test/test_codegen.exe > out.ll
	clang out.ll src/runtime/runtime.c -o out -Wno-override-module
	./out; echo "exit code: $$?"

clean-test:
	rm -f out.ll out