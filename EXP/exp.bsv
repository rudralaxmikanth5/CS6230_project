import FloatingPoint ::*;
import ConfigReg ::*;
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

interface Exp_ifc;
    method Action get_x(Float x);
    method Float res; 
endinterface

module mkExp(Exp_ifc);
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
			//$display("Stage 1: Smol num");
			stage1_x <= tagged Valid(x);
			stage1_int_res <= 1;
		end
		else begin
			//$display("Stage 1: Big num");
			Bit#(8) exp_diff = x.exp - 127;
			// $display("diff_1 = %d", exp_diff); // verified 
			if (x.sign == False) begin 
				Float xdiff = x - generate_2_x(exp_diff);
				stage1_x <= tagged Valid(xdiff);
				stage1_int_res <= generate_e_2_x(exp_diff);
			end 
			else begin 
				Float xdiff = x - generate_2_negx(exp_diff);
				//$display("xdiff1 = %b", xdiff );
				stage1_x <= tagged Valid(xdiff);
				stage1_int_res <= generate_e_2_negx(exp_diff);
				//$display("res = %b", stage1_int_res);
			end  
			//stage1_x <= tagged Valid(xdiff);     // stage1_x adds a valid bit to the MSB of xdiff
			//$display("stage1_x = %b", stage1_x);      
			//stage1_int_res <= generate_e_2_x(exp_diff);
			// $display("res = %b", generate_e_2_x(exp_diff)); // verified 
		end 
	endrule

	rule disable_stage2 (!isValid(stage0_x));
		stage1_x <= tagged Invalid;
	endrule

	// Stage 2
	rule stage2_check_if_smol if (isValid(stage1_x));    // isValid(stage_x) is 1 
		Float x = stage1_x.Valid;   
		//$display("x = %b", x);      // x is xdiff

		if (x.exp < 127) begin
			//$display("Stage 2: Smol num");
			stage2_x <= tagged Valid(x);
			stage2_int_res <= stage1_int_res;
		end
		else begin
			//$display("Stage 2: Big num");
			Bit#(8) exp_diff = x.exp - 127;
			// $display("diff_2 = %d", exp_diff);
			if (x.sign == False) begin 
			Float xdiff = x - generate_2_x(exp_diff);
			//$display("xdiff2 = %b", xdiff);
			stage2_x <= tagged Valid(xdiff);
			stage2_int_res <= stage1_int_res * generate_e_2_x(exp_diff);
			end 
			else begin 
			Float xdiff = x - generate_2_negx(exp_diff);
			//$display("xdiff2 = %b", xdiff);
			stage2_x <= tagged Valid(xdiff);
			stage2_int_res <= stage1_int_res * generate_e_2_negx(exp_diff);
			// $display("res = %b", stage2_int_res);

			end 
			//$display("xdiff = %b", xdiff);       // verified
			//stage2_x <= tagged Valid(xdiff);
			//$display("stage2_x = %b", stage2_x);
			//stage2_int_res <= stage1_int_res * generate_e_2_x(exp_diff);
			// $display("res = %b", stage1_int_res * generate_e_2_x(exp_diff));     // verified 
		end 
	endrule

	rule disable_stage3 (!isValid(stage1_x));
		stage2_x <= tagged Invalid;
	endrule

	// Stage 3 taylor first term 
    rule stage3 if (isValid(stage2_x));
		//$display("v bit = %b", isValid(stage2_x));
		Float z = FloatingPoint {
            sign:       False,
            exp:      8'b01111111,
            sfd:      23'b00000000000000000000000
        }; 
		txv_1 <= stage2_x.Valid;
		// $display("tx_1 = %b", stage2_x.Valid);
        Float tsum = z + stage2_x.Valid; 
		// $display("tsum = %b", tsum);
		tsum_1 <= tagged Valid(tsum);
		stage3_int_res <= stage2_int_res;
	endrule 

	rule disable_stage4 (!isValid(stage2_x));
		tsum_1 <= tagged Invalid;
	endrule

	// Stage 4 taylor second term 
	rule stage4 if (isValid(tsum_1));
		txv_2 <= txv_1;
		// $display("tx_2 = %b", txv_1);
		Float tsum_temp = tsum_1.Valid;
		Float tsum = tsum_temp + txv_1*txv_1*0.5;
		// $display("tsum = %b", tsum);    // verified 
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
		// $display("tsum = %b", tsum);   // verified 
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
		// $display("tsum = %b", tsum);  //verified 
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
		//$display("tsum = %b", tsum);  //verified
        result <= tagged Valid(tsum);
    endrule 

	rule disable_result (!isValid(tsum_4));
		result <= tagged Invalid;
	endrule

	// Stage 0
	rule disable_stage1 (!en_stage1);
		stage0_x <= tagged Invalid;
		// $display($time, " - Blocking inp");
	endrule

	method Action get_x(Float x_input);
		stage0_x <= tagged Valid(x_input);
		en_stage1.send();
		// $display($time, " - Sending inp");
	endmethod

	// Final stage
    method Float res if (isValid(result));
		return result.Valid;
	endmethod
endmodule 

(*synthesize*)
module mkTb(Empty);

    Exp_ifc m <- mkExp;
    Reg#(int) status <- mkReg(0);
    // rule get if (status==0);         
    //     m.get_x(17);
    //     status <= 1;
    // endrule

    // rule check if(status==1);
	// 	let res <- m.res;
	// 	$display($time, " Result = %b", res);
	// 	status <= 0;
    //     $finish;
    // endrule 
    rule rl_cycle;
		status <= status + 1;
		$display("cycle %d:",status);
		if(status>20)
			$finish(0);
	endrule 
	rule rl_get(status == 1);
	    m.get_x(5.5);
	endrule 
	rule rl_get1(status == 2);
	    m.get_x(12.2);
	endrule

	rule result;
	    let res = m.res; // = because data method used
		$display("cycle %d: Result = %b",status, res);
	endrule 
endmodule

