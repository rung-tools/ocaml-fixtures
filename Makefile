build:
	jbuilder build

clean:
	jbuilder clean

doc:
	jbuilder build @doc

test:
	jbuilder runtest --force

deps:
	opam install jbuilder.1.0+beta20
	opam install core.v0.10.0
	opam install core_extended.v0.10.0
