import FloatingPoint     ::*;
import ConfigReg         ::*;

typedef FloatingPoint#(8,23) Float;

function Float generate_2_x (Bit#(8) x);

    if (x[7:3] > 0) // doing for x greater than 5
        return 32; //32
	else if (x[2] == 1 && x[1] == 0 && x[0] == 0)
		return 16; //16
	else if (x[1] == 1 && x[0] == 1)
	    return 8; //8
	else if (x[1] == 1)
		return 4; //4
	else if (x[0] == 1)
		return 2; //2
	else
		return 1; //1

endfunction

function Float generate_2_negx (Bit#(8) x);
    if (x[7:3] > 0) // doing for x greater than 5
        return -32;
	else if (x[2] == 1 && x[1] == 0 && x[0] == 0)
		return -16; //16
	else if (x[1] == 1 && x[0] == 1)
	    return -8; //8
	else if (x[1] == 1)
		return -4; //4
	else if (x[0] == 1)
		return -2; //2
	else 
		return -1; //1
endfunction 

function Float generate_e_2_x (Bit#(8) x);
    
    if (x[7:3] > 0) // doing for x greater than 4
        return 78701400162304; //e^32             // I replaced the value of e^32 as it exceeds the value
	else if (x[2] == 1 && x[1] == 0 && x[0] == 0)
		return 8871381.143; //e^16
	else if (x[1] == 1 && x[0] == 1)
	    return 2978.486; //e^8
	else if (x[1] == 1)
		return 54.575; //e^4
	else if (x[0] == 1)
		return 7.387; //e^2
	else
		return 2.718; //e

endfunction

function Float generate_e_2_negx (Bit#(8) x);

    if (x[7:3] > 0) // doing for x greater than 4
        return 0.000000000000012706; //e^-32
	else if (x[2] == 1 && x[1] == 0 && x[0] == 0)
		return 0.0000001127; //e^-16
	else if (x[1] == 1 && x[0] == 1)
	    return 0.000335; //e^-8
	else if (x[1] == 1)
		return 0.0183; //e^-4
	else if (x[0] == 1)
		return 0.135; //e^-2
	else
		return 0.368; //e^-1

endfunction 

interface Ifc_exp;
    method Action get_x(Float x);
    method Float res; 
endinterface

module mkExp(Ifc_exp);
	// Stage 0
	PulseWire en_stage1 <- mkPulseWire;
    Reg#(Maybe#(Float)) stage0_x <- mkReg(tagged Invalid);

	// Stage 1
	Reg#(Maybe#(Float)) stage1_x <- mkReg(tagged Invalid);
	Reg#(Float) stage1_int_res <- mkReg(defaultValue);

	// Stage 2
    Reg#(Maybe#(Float)) stage2_x <- mkReg(tagged Invalid);
	Reg#(Float) stage2_int_res <- mkReg(defaultValue);

	// Stage 3
	Reg#(Float) txv_1 <- mkReg(defaultValue);
	Reg#(Float) stage3_int_res <- mkReg(defaultValue);
	Reg#(Maybe#(Float)) tsum_1 <- mkReg(tagged Invalid);

	// Stage 4
    Reg#(Float) txv_2 <- mkReg(defaultValue);
	Reg#(Float) stage4_int_res <- mkReg(defaultValue);
	Reg#(Maybe#(Float)) tsum_2 <- mkReg(tagged Invalid);

	// Stage 5
	Reg#(Float) txv_3 <- mkReg(defaultValue);
	Reg#(Float) stage5_int_res <- mkReg(defaultValue);
	Reg#(Maybe#(Float)) tsum_3 <- mkReg(tagged Invalid);

	// Stage 6
	Reg#(Float) txv_4 <- mkReg(defaultValue);
	Reg#(Float) stage6_int_res <- mkReg(defaultValue);
	Reg#(Maybe#(Float)) tsum_4 <- mkReg(tagged Invalid);

    // Stage 7 
    Reg#(Maybe#(Float)) result <- mkReg(tagged Invalid);

	// Stage 1
	rule stage1_check_if_smol if (isValid(stage0_x));
		Float x = stage0_x.Valid;
		if (x.exp < 127) begin
			stage1_x <= tagged Valid(x);
			stage1_int_res <= 1;
		end
		else begin
			Bit#(8) exp_diff = x.exp - 127;
			if (x.sign == False) begin 
				Float xdiff = x - generate_2_x(exp_diff);
				stage1_x <= tagged Valid(xdiff);
				stage1_int_res <= generate_e_2_x(exp_diff);
			end 
			else begin 
				Float xdiff = x - generate_2_negx(exp_diff);
				stage1_x <= tagged Valid(xdiff);
				stage1_int_res <= generate_e_2_negx(exp_diff);
			end  
	
		end 
	endrule

	rule disable_stage2 (!isValid(stage0_x));
		stage1_x <= tagged Invalid;
	endrule

	// Stage 2
	rule stage2_check_if_smol if (isValid(stage1_x));    // isValid(stage_x) is 1 
		Float x = stage1_x.Valid;   

		if (x.exp < 127) begin
			stage2_x <= tagged Valid(x);
			stage2_int_res <= stage1_int_res;
		end
		else begin
			Bit#(8) exp_diff = x.exp - 127;
			if (x.sign == False) begin 
			Float xdiff = x - generate_2_x(exp_diff);
			stage2_x <= tagged Valid(xdiff);
			stage2_int_res <= stage1_int_res * generate_e_2_x(exp_diff);
			end 
			else begin 
			Float xdiff = x - generate_2_negx(exp_diff);
			stage2_x <= tagged Valid(xdiff);
			stage2_int_res <= stage1_int_res * generate_e_2_negx(exp_diff);

			end 

		end 
	endrule

	rule disable_stage3 (!isValid(stage1_x));
		stage2_x <= tagged Invalid;
	endrule

	// Stage 3 taylor first term 
    rule stage3 if (isValid(stage2_x));
		Float z = FloatingPoint {
            sign:       False,
            exp:      8'b01111111,
            sfd:      23'b00000000000000000000000
        }; 
		txv_1 <= stage2_x.Valid;
        Float tsum = z + stage2_x.Valid; 
		tsum_1 <= tagged Valid(tsum);
		stage3_int_res <= stage2_int_res;
	endrule 

	rule disable_stage4 (!isValid(stage2_x));
		tsum_1 <= tagged Invalid;
	endrule

	// Stage 4 taylor second term 
	rule stage4 if (isValid(tsum_1));
		txv_2 <= txv_1;
		Float tsum_temp = tsum_1.Valid;
		Float tsum = tsum_temp + txv_1*txv_1*0.5;
        tsum_2 <= tagged Valid(tsum);
		stage4_int_res <= stage3_int_res;
	endrule 

	rule disable_stage5 (!isValid(tsum_1));
		tsum_2 <= tagged Invalid;
	endrule

	// Stage 5 taylor third term 
	rule stage5 if (isValid(tsum_2));
		txv_3 <= txv_2;
		Float tsum_temp = tsum_2.Valid;
		Float tsum = tsum_temp + txv_2*txv_2*txv_2*0.167;
		tsum_3 <= tagged Valid (tsum);
		stage5_int_res <= stage4_int_res;
	endrule 

	rule disable_stage6 (!isValid(tsum_2));
		tsum_3 <= tagged Invalid;
	endrule

	// Stage 6 taylor four term
	rule stage6 if (isValid(tsum_3));
		txv_4 <= txv_3;
		Float tsum_temp = tsum_3.Valid;
		Float tsum = (tsum_temp + txv_3*txv_3*txv_3*txv_3*0.0417);
        tsum_4 <= tagged Valid (tsum);
        stage6_int_res <= stage5_int_res;
	endrule 

	rule disable_stage7 (!isValid(tsum_3));
		tsum_4 <= tagged Invalid;
	endrule
    
    // Stage 7 taylor five term
    rule stage7 if (isValid(tsum_4));
        Float tx_5 = txv_4;
        Float tsum_temp = tsum_4.Valid;
		Float tsum = (tsum_temp + tx_5*tx_5*tx_5*tx_5*tx_5*0.00833) * stage6_int_res;
        result <= tagged Valid(tsum);
    endrule 

	rule disable_result (!isValid(tsum_4));
		result <= tagged Invalid;
	endrule

	// Stage 0
	rule disable_stage1 (!en_stage1);
		stage0_x <= tagged Invalid;
	endrule

	method Action get_x(Float x_input);
		stage0_x <= tagged Valid(x_input);
		en_stage1.send();
	endmethod

	// Final stage
    method Float res if (isValid(result));
		return result.Valid;
	endmethod
endmodule

interface Ifc_exp_s2;
    method Action get(Bit#(2) op, Float x);
    method Float exp_result; 
    method Action pass_in(Float x);
    method Float pass_out; 
endinterface 

module mkexp_s2(Ifc_exp_s2); 
    Reg#(Float) input_x <- mkReg(0);
    Reg#(Float) exp_res <- mkReg(0);
    Reg#(Float) in <- mkReg(0);
    Reg#(Bit#(2)) opcode <- mkReg(0);
    Reg#(Bool) input_valid_exp <- mkReg(False);

    Reg#(Bool) exp1_valid <- mkReg(False);
	Reg#(Bool) exp2_valid <- mkReg(False);
	Reg#(Bool) exp3_valid <- mkReg(False);
	Reg#(Bool) exp4_valid <- mkReg(False);
	Reg#(Bool) exp5_valid <- mkReg(False);
	Reg#(Bool) exp6_valid <- mkReg(False);
    Ifc_exp ifc_exp <- mkExp;

    rule exp_1 (((opcode == 2'b00) || (opcode == 2'b01) || (opcode == 2'b11)) && input_valid_exp);
       ifc_exp.get_x(input_x);
       exp1_valid <= True;
    endrule 

    rule exp_2 ((opcode == 2'b10) && input_valid_exp);
       exp_res <= input_x;
    endrule 
    rule exp1 (exp1_valid);
		exp2_valid <= True;
	endrule
	rule exp2 (exp2_valid);
		exp3_valid <= True;
	endrule
	rule exp3 (exp3_valid);
		exp4_valid <= True;
	endrule
	rule exp4 (exp4_valid);
		exp5_valid <= True;
	endrule
	rule exp5 (exp5_valid);
		exp6_valid <= True;
	endrule

    rule exp1_res (((opcode == 2'b00) || (opcode == 2'b01) || (opcode == 2'b11)) && input_valid_exp && exp6_valid );
        let r = ifc_exp.res;
        exp_res <= r;
    endrule

    method Action get(Bit#(2) op, Float x);
        input_x <= x;
        opcode <= op;
        input_valid_exp <= True;
     endmethod
 
     method Float exp_result; 
        return exp_res;
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

    Ifc_exp_s2 fp_exp <- mkexp_s2;
    rule stages;
        cycle <= cycle + 1;
    endrule
    rule get1 (cycle == 0);
       fp_exp.get(2'b01, 2);
    endrule

    // rule get2 (cycle == 1);
    //    fp_exp.get(2'b00, 1.2);
    // endrule

    rule res;
        let r = fp_exp.exp_result;
        $display("Result = %b : cycle = %d",  r, cycle);
    endrule 
    rule done(cycle == 15);
        $finish(0);
    endrule 
endmodule:mkTb 
