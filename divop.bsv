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

(*synthesize*)
module mkTb();
    Reg#(int) cycle <- mkReg(0);

    Ifc_div_op fp_div_op <- mk_div_op;
    rule stages;
        cycle <= cycle + 1;
    endrule
    rule get (cycle == 0);
       fp_div_op.get(3.2, 1.2);
    endrule
    rule res;
        let r = fp_div_op.res_op;
        $display("Result = %b : cycle = %d", r, cycle);
    endrule 
    rule done(cycle == 3);
        $finish(0);
    endrule 
endmodule:mkTb