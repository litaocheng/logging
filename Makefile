SHELL := /bin/bash
include vsn.mk
.PHONY: all test edoc dialyzer clean
ERL_LIB := $(shell erl -noshell -eval 'io:format("~s",[code:lib_dir()]),erlang:halt()' \
		2> /dev/null)
APP_FULLNAME := $(APP_NAME)-$(APP_VSN)

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

install: all
ifeq ($(ERL_LIB), )
	@echo "please install Erlang/OTP"
else
	@echo "install..."
	(mkdir -p $(ERL_LIB)/$(APP_FULLNAME) && \
		cp -rf ./ebin ./include ./src $(ERL_LIB)/$(APP_FULLNAME))
endif

uninstall:
	@echo "uninstall the lib..."
	(rm -rf $(ERL_LIB)/$(APP_FULLNAME))
