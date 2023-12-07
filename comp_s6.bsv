import FloatingPoint     ::*;
import ConfigReg         ::*;
typedef FloatingPoint#(8,23) Float;

interface Ifc_comp_s6;
    method Action get(Bit#(2) op, Float v,Float x, Float z);
    method Float comp_result; 
endinterface

module mk_comp_s6(Ifc_comp_s6);
    Reg#(Float) input_x <- mkReg(0);
    Reg#(Float) sub_Res <- mkReg(0);
    Reg#(Float) div_Res <- mkReg(0);
    Reg#(Float) comp_res <- mkReg(0);
    Reg#(Float) y <- mkReg(0);

    Reg#(Float) alpha <- mkReg(1.67);
    Reg#(Float) lambda <- mkReg(1.05);
    Reg#(Float) alpha_lambda <- mkReg(1.75);

    Reg#(Bit#(2)) opcode <- mkReg(0);

    Reg#(Bool) input_valid_comp <- mkReg(False);

    rule tanh_or_sigmoid (((opcode == 2'b00) || (opcode == 2'b01)) && input_valid_comp); 
        comp_res <= div_Res;
    endrule 

    rule lrelu ((opcode == 2'b10) && input_valid_comp);
        if (input_x.sign == False) begin 
            comp_res <= input_x;
        end
        if (input_x.sign == True) begin
            comp_res <= alpha*input_x;
        end
    endrule
    rule selu ((opcode == 2'b11) && input_valid_comp);
        if (input_x.sign == False) begin
            comp_res <= lambda*input_x;
        end
        if (input_x.sign == True) begin 
            comp_res <= alpha_lambda*sub_Res;
        end
    endrule 
    method Action get(Bit#(2) op,Float v,Float x, Float z);
        input_x <= v;
        sub_Res <= x;
        div_Res <= z;
        opcode <= op;
        input_valid_comp <= True;
    endmethod

    method Float comp_result;
        return comp_res ;
    endmethod
endmodule 

(*synthesize*)
module mkTb();
    Reg#(int) cycle <- mkReg(0);

    Ifc_comp_s6 fp_comp <- mk_comp_s6;
    rule stages;
        cycle <= cycle + 1;
    endrule
    rule get1 (cycle == 0);
       fp_comp.get(2'b11, -1, 2, 3);
    endrule

    // rule get2 (cycle == 1);
    //    fp_comp.get(2'b00, 5, 2.2, 1.2);
    // endrule

    rule res;
        let r = fp_comp.comp_result;
        $display("Result = %b : cycle = %d", r, cycle);
    endrule 
    rule done(cycle == 3);
        $finish(0);
    endrule 
endmodule:mkTb