
hs_files = $(shell find Data bench -name \*.hs)

bench.html : dist/build/bench/bench
	dist/build/bench/bench --no-gc -o bench.html --template=Criterion/report.tpl


dist/build/bench/bench : $(hs_files)
	cabal build
