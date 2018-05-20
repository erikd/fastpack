
hs_files = $(shell find src bench -name \*.hs)

fastpack-bench.html : dist/build/bench/bench
	dist/build/bench/bench -o $@ --template=Criterion/report.tpl
