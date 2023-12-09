import FloatingPoint     ::*;
import ConfigReg         ::*;

typedef FloatingPoint#(8,23) Float;

interface Ifc_factor_s1;
    method Action get(Bit#(2) op, Float x);
    method Float factor_result; 
    method Action pass_in(Float x);
    method Float pass_out; 
endinterface

module mkfactor_s1(Ifc_factor_s1);    // factor has a latency of 2 cycles
    Reg#(Float) input_x <- mkReg(0);
    Reg#(Float) factor_res <- mkReg(0);
    Reg#(Float) in <- mkReg(0);
    Reg#(Bit#(2)) opcode <- mkReg(0);
    Reg#(Bool) input_valid_factor <- mkReg(False);
    
    rule tanh ((opcode == 2'b00) && (input_valid_factor));
       factor_res <= 2 * input_x;
    endrule 
    rule signmoid ((opcode == 2'b01) && (input_valid_factor));
       factor_res <= -1 * input_x;
    endrule 
    rule lrelu_or_selu (((opcode == 2'b10) || (opcode == 2'b11))&& (input_valid_factor));
       factor_res <= 1 * input_x;
    endrule 

    method Action get(Bit#(2) op, Float x);
       input_x <= x;
       opcode <= op;
       input_valid_factor <= True;
    endmethod

    method Float factor_result; 
        return factor_res;
    endmethod 
    method Action pass_in(Float x);
        in <= x;
    endmethod 
    method Float pass_out;
        return in;
    endmethod
endmodule 

(*synthesize*)
module mkTb();   
    Reg#(int) cycle <- mkReg(0);

    Ifc_factor_s1 fp_factor <- mkfactor_s1;
    rule stages;
        cycle <= cycle + 1;
    endrule
    rule get1 (cycle == 0);
       fp_factor.get(2'b01, 2);
    endrule

    rule get2 (cycle == 1);
       fp_factor.get(2'b00, 1.2);
    endrule

    rule res;
        let r = fp_factor.factor_result;
        $display("Result = %b : cycle = %d",  r, cycle);
    endrule 
    rule done(cycle == 3);
        $finish(0);
    endrule 
endmodule:mkTb

    