SHELL := /bin/bash
.PHONY: all test edoc dialyzer clean

all:
	(cd src;$(MAKE))

unit_test: clean
	(cd src;$(MAKE) TEST=true EUNIT=true)
	(erl -pa ./ebin -noshell -eval "eunit:test(\"./ebin\", []), init:stop()")

comm_test: clean
	(mkdir -p ./test/log)
	(cd src;$(MAKE) TEST=true)
	(erl -noshell -s ct_run script_start -DTEST -logdir `pwd`/test/log -include `pwd`/include -pa `pwd`/ebin -cover test/test.coverspec -dir . -s init stop)

edoc: 
	(mkdir -p ./edoc)
	(cd src; $(MAKE) edoc)

tags :
	(ctags -R .)

clean:
	(cd src;$(MAKE) clean)
	rm -rf ./test/log
	rm -rf ./test/*.beam
