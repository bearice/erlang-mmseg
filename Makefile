REBAR=./rebar

all:
	@$(REBAR) compile

clean:
	@$(REBAR) clean 

build_plt:
	@$(REBAR) build-plt

dialyzer:
	@$(REBAR) dialyze

