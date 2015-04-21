DEPS_PLT=$(CURDIR)/plt/dialyzer_plt

DEPS=erts kernel stdlib crypto mnesia deps/webmachine/ebin  deps/lager/ebin  deps/jsx/ebin  deps/mochiweb/ebin deps/sync/ebin  inets deps/hackney/ebin deps/proper/ebin deps/triq/ebin 

REBAR = rebar --force
.PHONY: deps

all: deps compile

compile: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

distclean: clean devclean relclean
	$(REBAR) delete-deps

release: rel
rel: relclean all
	$(REBAR) generate



relclean:
	rm -rf rel/riak_sets

xref: all
	$(REBAR) skip_deps=true xref

eunit: fcompile
	@if [ -n "$(REBAR)" ] ; then \
	ERL_LIBS=~/eqcmini $(REBAR) eunit skip_deps=true ; \
	fi
fcompile: 
	$(REBAR) compile skip_deps=true

test: compile eunit dialyzer 

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt \
	   --apps $(DEPS)
plt: $(DEPS_PLT)

dialyzer: fcompile plt
	dialyzer \
	--fullpath \
	--plt $(DEPS_PLT) \
	-Wrace_conditions -r  ebin


typer:
	typer --plt $(DEPS_PLT) -r ./src


stage : rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/riak_sets/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/riak_sets/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/riak_sets/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/riak_sets/lib;)


##
## Developer targets
##
##  devN - Make a dev build for node N
##  stagedevN - Make a stage dev build for node N (symlink libraries)
##  devrel - Make a dev build for 1..$DEVNODES
##  stagedevrel Make a stagedev build for 1..$DEVNODES
##
##  Example, make a 68 node devrel cluster
##    make stagedevrel DEVNODES=68

.PHONY : stagedevrel devrel
DEVNODES ?= 4

# 'seq' is not available on all *BSD, so using an alternate in awk
SEQ = $(shell awk 'BEGIN { for (i = 1; i < '$(DEVNODES)'; i++) printf("%i ", i); print i ;exit(0);}')

$(eval stagedevrel : $(foreach n,$(SEQ),stagedev$(n)))
$(eval devrel : $(foreach n,$(SEQ),dev$(n)))

dev% : all
	mkdir -p dev
	rel/gen_dev $@ rel/vars/dev_vars.config.src rel/vars/$@_vars.config
	(cd rel && $(REBAR) generate target_dir=../dev/$@ overlay_vars=vars/$@_vars.config)

stagedev% : dev%
	  $(foreach dep,$(wildcard deps/*), rm -rf dev/$^/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) dev/$^/lib;)
	  $(foreach app,$(wildcard apps/*), rm -rf dev/$^/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) dev/$^/lib;)

devclean: clean
	rm -rf dev
