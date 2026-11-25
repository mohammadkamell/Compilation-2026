.PHONY: test clean-test semanttest tests

MODE ?= compile

tests:
	MODE=$(MODE) bash run_tests.sh; r1=$$?; \
	MODE=$(MODE) bash run_examples.sh; r2=$$?; \
	exit $$((r1 + r2))

test:
	dune exec ./test/test_codegen.exe > out.ll
	clang out.ll src/runtime/runtime.c src/runtime/stdlib.c -o out -Wno-override-module
	./out; echo "exit code: $$?"

clean-test:
	rm -f out.ll out

semanttest:
	dune exec ./test/semantic/test_semant.exe