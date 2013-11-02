# See LICENSE for licensing information.

PROJECT = everrank

# Options.

#COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol
CT_SUITES = eunit http spdy ws
PLT_APPS = crypto public_key ssl

# Dependencies.

DEPS = cowboy 
dep_cowboy = pkg://cowboy

TEST_DEPS = ct_helper gun
dep_ct_helper = https://github.com/extend/ct_helper.git master
dep_gun = pkg://gun master

# Standard targets.

include erlang.mk

# Extra targets.

.PHONY: autobahn

autobahn: clean clean-deps deps app build-tests
	@mkdir -p logs/
	@$(CT_RUN) -suite autobahn_SUITE
