import FloatingPoint     ::*;
import ConfigReg         ::*;
interface Ifc_div_op;
    method Action get(Float a,Float b);
    method Float res_op; 
endinterface

module mk_div_op(Ifc_div_op);
    Reg#(Float) x1 <- mkReg(0);
    Reg#(Float) x2 <- mkReg(0);
    Reg#(Float) y <- mkReg(0);

    rule res;
        y <= x1/x2;
    endrule 
    method Action get(Float a, Float b);
        x1 <= a;
        x2 <= b;
    endmethod

    method Float res_op;
        return y;
    endmethod
endmodule 

interface Ifc_div_s5;
    method Action get(Bit#(2) op, Float x, Float z);
    method Float div_result; 
    method Action pass_in(Float x, Float div_r);
    method Float pass_out; 
    method Float sub_res_out;
endinterface 

module mkdiv_s5(Ifc_div_s5); 
    Reg#(Float) input_x <- mkReg(0); 
    Reg#(Float) input_z <- mkReg(0);
    Reg#(Float) div_res <- mkReg(0);
    Reg#(Float) sub_pass <- mkReg(0);
    Reg#(Float) in <- mkReg(0);

    Reg#(Bit#(2)) opcode <- mkReg(0);
    Reg#(Bool) input_valid_div <- mkReg(False);

    Reg#(Bool) div1_valid <- mkReg(False);
    Reg#(Bool) div2_valid <- mkReg(False);

    Ifc_div_op ifc_divop <- mk_div_op;

    rule div_1 (((opcode == 2'b00)) && (input_valid_div));
        ifc_divop.get(input_x, input_z);
        div1_valid <= True;
    endrule 

    rule div_1res (((opcode == 2'b00)) && (div1_valid));
        let d = ifc_divop.res_op;
        div_res <= d;
    endrule 

    rule div_2 (((opcode == 2'b01)) && (input_valid_div));
        ifc_divop.get(1, input_z);
        div2_valid <= True;
    endrule 

    rule div_2res (((opcode == 2'b01)) && (div2_valid));
        let d = ifc_divop.res_op;
        div_res <= d;
    endrule 

    rule div2_res (((opcode == 2'b10)||(opcode == 2'b11)) && input_valid_div );
        div_res <= input_x;
    endrule

    method Action get(Bit#(2) op, Float x, Float z);
        input_x <= x;
        input_z <= z;
        opcode <= op;
        input_valid_div <= True;
     endmethod
 
     method Float div_result; 
        return div_res;
     endmethod 
 
     method Action pass_in(Float x, Float sub_r);
        in <= x;
        sub_pass <= sub_r;
     endmethod 
     method Float pass_out;
        return in;
     endmethod
     method Float sub_res_out;
        return sub_pass;
     endmethod
endmodule

(*synthesize*)
module mkTb();   
    Reg#(int) cycle <- mkReg(0);

    Ifc_div_s5 fp_div <- mkdiv_s5;
    rule stages;
        cycle <= cycle + 1;
    endrule
    rule get1 (cycle == 0);
       fp_div.get(2'b00, 2, 1.2);
    endrule

    rule get2 (cycle == 1);
       fp_div.get(2'b01, 1.2, 3);
    endrule

    rule res;
        let div_r = fp_div.div_result;
        $display("Result = %b : cycle = %d", div_r, cycle);
    endrule 
    rule done(cycle == 15);
        $finish(0);
    endrule 
endmodule:mkTb 
        