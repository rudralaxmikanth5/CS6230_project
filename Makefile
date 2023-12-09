MODULE = Pipelined_module.bsv
TESTBENCH = mkTb

.PHONY: verilog compile run clean all

all: verilog compile run

verilog:
	bsc -verilog $(MODULE)

compile:
	bsc -e $(TESTBENCH) $(TESTBENCH).v

run:
	./a.out

clean:
	rm -f *.v *.out *.bo
