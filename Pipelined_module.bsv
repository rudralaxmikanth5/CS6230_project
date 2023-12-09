import FloatingPoint     ::*;
import Vector            ::*;
import Real              ::*;
import BUtils            ::*;
import DefaultValue      ::*;
import FShow             ::*;
import GetPut            ::*;
import ClientServer      ::*;
import FIFO              ::*;
import FixedPoint        ::*;
import DReg  :: *;
import FloatingPoint ::*;
import ConfigReg ::*;
typedef FloatingPoint#(8,23) Float;

//============================ ADD module =================================

`define STAGES_FADD_SP 4
typedef struct {
   Bit#(1) 	valid;
   FloatingPoint#(e,m)	value;
   Exception  	ex;
} ReturnType#(numeric type e, numeric type m) deriving (Bits,FShow);

typedef struct {
		Maybe#(FloatingPoint#(e,m)) res;
		Exception exc;
		RoundMode rmode;
		} CommonState#(numeric type e, numeric type m) deriving (Bits, Eq);


function Bool isNaNOrInfinity( FloatingPoint#(e,m) din );
   return (din.exp == '1);
endfunction

function Bool isSubNormal( FloatingPoint#(e,m) din );
   return (din.exp == 0);
endfunction

function Integer bias( FloatingPoint#(e,m) din );
   return (2 ** (valueof(e)-1)) - 1;
endfunction

function Bit#(e) unbias( FloatingPoint#(e,m) din );
   return (din.exp - fromInteger(bias(din)));
endfunction

function Bit#(1) getHiddenBit( FloatingPoint#(e,m) din );
   return (isSubNormal(din)) ? 0 : 1;
endfunction

function Integer minexp( FloatingPoint#(e,m) din );
  return 1-bias(din);
endfunction

function Integer minexp_subnormal( FloatingPoint#(e,m) din );
   return minexp(din)-valueof(m);
endfunction

function Integer maxexp( FloatingPoint#(e,m) din );
   return bias(din);
endfunction

function Tuple2#(FloatingPoint#(e,m),Exception) round( RoundMode rmode, FloatingPoint#(e,m) din, Bit#(2) guard )
   provisos(  Add#(m, 1, m1)
	    , Add#(m, 2, m2)
	    );

   FloatingPoint#(e,m) out = defaultValue;
   Exception exc = defaultValue;

   if (isNaNOrInfinity(din)) begin
      out = din;
   end
   else begin
      let din_inc = din;

      Bit#(TAdd#(m,2)) sfd = unpack({1'b0, getHiddenBit(din), din.sfd}) + 1;

      if (msb(sfd) == 1) begin
	 if (din.exp == fromInteger(maxexp(din) + bias(out))) begin
	    din_inc = infinity(din_inc.sign);
	 end
	 else begin
	    din_inc.exp = din_inc.exp + 1;
	    din_inc.sfd = truncate(sfd >> 1);
	 end
      end
      else if ((din.exp == 0) && (truncateLSB(sfd) == 2'b01)) begin
	 din_inc.exp = 1;
	 din_inc.sfd = truncate(sfd);
      end
      else begin
	 din_inc.sfd = truncate(sfd);
      end

      if (guard != 0) begin
	 exc.inexact = True;
      end

      case(rmode)
	 Rnd_Nearest_Even:
	 begin
	    case (guard)
	       'b00: out = din;
	       'b01: out = din;
	       'b10: out = (lsb(din.sfd) == 1) ? din_inc : din;
	       'b11: out = din_inc;
	    endcase
	 end

	 Rnd_Nearest_Away_Zero:
	 begin
	    case (guard)
	       'b00: out = din;
//	       'b01: out = din_inc;
	       'b01: out = din;
	       'b10: out = din_inc;
	       'b11: out = din_inc;
	    endcase
	 end

	 Rnd_Plus_Inf:
	 begin
	    if (guard == 0)
	       out = din;
	    else if (din.sign)
	       out = din;
	    else
	       out = din_inc;
	 end

	 Rnd_Minus_Inf:
	 begin
	    if (guard == 0)
	       out = din;
	    else if (din.sign)
	       out = din_inc;
	    else
	       out = din;
	 end

	 Rnd_Zero:
	 begin
	    out = din;
	 end
      endcase
   end

   if (isInfinity(out)) begin
      exc.overflow = True;
   end

   return tuple2(out,exc);
endfunction

function Tuple3#(FloatingPoint#(e,m),Bit#(2),Exception) normalize( FloatingPoint#(e,m) din, Bit#(x) sfdin )
   provisos(
      Add#(1, a__, x),
      Add#(m, b__, x),
      // per request of bsc
      Add#(c__, TLog#(TAdd#(1, x)), TAdd#(e, 1))
      );

   FloatingPoint#(e,m) out = din;
   Bit#(2) guard = 0;
   Exception exc = defaultValue;
   Int#(TAdd#(e,1)) exp = isSubNormal(out) ? fromInteger(minexp(out)) : signExtend(unpack(unbias(out)));
   let zeros = countZerosMSB(sfdin);

   if ((zeros == 0) && (exp == fromInteger(maxexp(out)))) begin
      out.exp = maxBound - 1;
      out.sfd = maxBound;
      guard = '1;
      exc.overflow = True;
      exc.inexact = True;
   end
   else begin
      if (zeros == 0) begin
	 // carry, no sfd adjust necessary

	 if (out.exp == 0)
	    out.exp = 2;
	 else
	    out.exp = out.exp + 1;

	 // carry bit
	 sfdin = sfdin << 1;
      end
      else if (zeros == 1) begin
	 // already normalized

	 if (out.exp == 0)
	    out.exp = 1;

	 // carry, hidden bits
	 sfdin = sfdin << 2;
      end
      else if (zeros == fromInteger(valueOf(x))) begin
	 // exactly zero
	 out.exp = 0;
      end
      else begin
	 // try to normalize
	 Int#(TAdd#(e,1)) shift = zeroExtend(unpack(pack(zeros - 1)));
	 Int#(TAdd#(e,1)) maxshift = exp - fromInteger(minexp(out));

`ifdef denormal_support
if (shift > maxshift) begin
	    // result will be subnormal

	    sfdin = sfdin << maxshift;
	    out.exp = 0;
	 end
	 else begin
	    // result will be normal

	    sfdin = sfdin << shift;
	    out.exp = out.exp - truncate(pack(shift));
	 end

 	 // carry, hidden bits
	 sfdin = sfdin << 2;
      end
`else
         if (shift <= maxshift) begin
	    // result will be normal

	    sfdin = sfdin << shift;
       out.exp = out.exp - truncate(pack(shift));
       end
	    sfdin = sfdin << 2;
	 

 	 // carry, hidden bits
	
      end
`endif
      
      out.sfd = unpack(truncateLSB(sfdin));
      sfdin = sfdin << fromInteger(valueOf(m));

      guard[1] = unpack(truncateLSB(sfdin));
      sfdin = sfdin << 1;

      guard[0] = |sfdin;
   end

   if ((out.exp == 0) && (guard != 0))
      exc.underflow = True;

   return tuple3(out,guard,exc);
endfunction

function FloatingPoint#(e,m) canonicalize (FloatingPoint#(e,m) in);
	if (in.exp == '1 && in.sfd != 0)
	  return FloatingPoint{sign:False, exp:'1, sfd:1<<(valueof(m)-1)};
	else
	  return in;
endfunction 
function Tuple2#(FloatingPoint#(e,m),Exception) fp_add (Tuple3#(FloatingPoint#(e,m),FloatingPoint#(e,m),RoundMode) operands)

   provisos(
      Add#(a__, TLog#(TAdd#(1, TAdd#(m, 5))), TAdd#(e, 1))
      );

   function Tuple7#(CommonState#(e,m),
		    Bit#(TAdd#(m,5)),
		    Bit#(TAdd#(m,5)),
		    Bool,
		    Bool,
		    Bit#(e),
				Bit#(e)) s1_stage(Tuple3#(
								FloatingPoint#(e,m),
					      FloatingPoint#(e,m),
					      RoundMode) op);

      match {.opA, .opB, .rmode } = op;

      CommonState#(e,m) s = CommonState {
	 res: tagged Invalid,
	 exc: defaultValue,
	 rmode: rmode
	 };

      Int#(TAdd#(e,2)) expA = isSubNormal(opA) ? fromInteger(minexp(opA)) : signExtend(unpack(unbias(opA)));
      Int#(TAdd#(e,2)) expB = isSubNormal(opB) ? fromInteger(minexp(opB)) : signExtend(unpack(unbias(opB)));

      Bit#(TAdd#(m,5)) sfdA = {1'b0, getHiddenBit(opA), opA.sfd, 3'b0};
      Bit#(TAdd#(m,5)) sfdB = {1'b0, getHiddenBit(opB), opB.sfd, 3'b0};

      Bit#(TAdd#(m,5)) x;
      Bit#(TAdd#(m,5)) y;
      Bool sgn;
      Bool sub;
      Bit#(e) exp;
      Bit#(e) expdiff;

      if ((expB > expA) || ((expB == expA) && (sfdB > sfdA))) begin
	 exp = opB.exp;
	 expdiff = truncate(pack(expB - expA));
	 x = sfdB;
	 y = sfdA;
	 sgn = opB.sign;
	 sub = (opB.sign != opA.sign);
      end
      else begin
	 exp = opA.exp;
	 expdiff = truncate(pack(expA - expB));
	 x = sfdA;
	 y = sfdB;
	 sgn = opA.sign;
	 sub = (opA.sign != opB.sign);
      end

      if (isSNaN(opA)) begin
	 s.res = tagged Valid nanQuiet(opA);
	 s.exc.invalid_op = True;
      end
      else if (isSNaN(opB)) begin
	 s.res = tagged Valid nanQuiet(opB);
	 s.exc.invalid_op = True;
      end
      else if (isQNaN(opA)) begin
	 s.res = tagged Valid opA;
      end
      else if (isQNaN(opB)) begin
	 s.res = tagged Valid opB;
      end
      else if (isInfinity(opA) && isInfinity(opB)) begin
	 if (opA.sign == opB.sign)
	    s.res = tagged Valid infinity(opA.sign);
	 else begin
	    s.res = tagged Valid qnan();
	    s.exc.invalid_op = True;
	 end
      end
      else if (isInfinity(opA)) begin
	 s.res = tagged Valid opA;
      end
      else if (isInfinity(opB)) begin
	 s.res = tagged Valid opB;
      end

      return tuple7(s,
		    x,
		    y,
		    sgn,
		    sub,
		    exp,
		    expdiff);
   endfunction

   function Tuple6#(CommonState#(e,m),
		    Bit#(TAdd#(m,5)),
		    Bit#(TAdd#(m,5)),
		    Bool,
		    Bool,
		    Bit#(e)) s2_stage(Tuple7#(CommonState#(e,m),
					      Bit#(TAdd#(m,5)),
					      Bit#(TAdd#(m,5)),
					      Bool,
					      Bool,
					      Bit#(e),
					      Bit#(e)) op);

      match {.s, .opA, .opB, .sign, .subtract, .exp, .diff} = op;

      if (s.res matches tagged Invalid) begin
	 if (diff < fromInteger(valueOf(m) + 5)) begin
	    Bit#(TAdd#(m,5)) guard = opB;

	    guard = opB << (fromInteger(valueOf(m) + 5) - diff);
	    opB = opB >> diff;
	    opB[0] = opB[0] | (|guard);
	 end
	 else if (|opB == 1) begin
	    opB = 1;
	 end
      end

      return tuple6(s,
		    opA,
		    opB,
		    sign,
		    subtract,
		    exp);
   endfunction

   function Tuple6#(CommonState#(e,m),
		    Bit#(TAdd#(m,5)),
		    Bit#(TAdd#(m,5)),
		    Bool,
		    Bool,
		    Bit#(e)) s3_stage(Tuple6#(CommonState#(e,m),
					      Bit#(TAdd#(m,5)),
					      Bit#(TAdd#(m,5)),
					      Bool,
					      Bool,
					      Bit#(e)) op);

      match {.s, .a, .b, .sign, .subtract, .exp} = op;

      let sum = a + b;
      let diff = a - b;

      return tuple6(s,
		    sum,
		    diff,
		    sign,
		    subtract,
		    exp);
   endfunction

   function Tuple4#(CommonState#(e,m),
		    FloatingPoint#(e,m),
		    Bit#(2),
		    Bool) s4_stage(Tuple6#(CommonState#(e,m),
					   Bit#(TAdd#(m,5)),
					   Bit#(TAdd#(m,5)),
					   Bool,
					   Bool,
					   Bit#(e)) op);

      match {.s, .addres, .subres, .sign, .subtract, .exp} = op;

      FloatingPoint#(e,m) out = defaultValue;
      Bit#(2) guard = 0;

      if (s.res matches tagged Invalid) begin
	 Bit#(TAdd#(m,5)) result;

	 if (subtract) begin
	    result = subres;
	 end
	 else begin
            result = addres;
	 end

	 out.sign = sign;
	 out.exp = exp;

	 let y = normalize(out, result);
	 out = tpl_1(y);
	 guard = tpl_2(y);
	 s.exc = s.exc | tpl_3(y);
      end

      return tuple4(s,
		    out,
		    guard,
		    subtract);
   endfunction

   function Tuple2#(FloatingPoint#(e,m),
		    Exception) s5_stage(Tuple4#(CommonState#(e,m),
						FloatingPoint#(e,m),
						Bit#(2),
						Bool) op);

      match {.s, .rnd, .guard, .subtract} = op;

      FloatingPoint#(e,m) out = rnd;

      if (s.res matches tagged Valid .x) begin
	 out = x;
      end
      else begin
	 let y = round(s.rmode, out, guard);
	 out = tpl_1(y);
	 s.exc = s.exc | tpl_2(y);
      end

      // adjust sign for exact zero result
      if (isZero(out) && !s.exc.inexact && subtract) begin
	 out.sign = (s.rmode == Rnd_Minus_Inf);
      end

      return tuple2(canonicalize(out),s.exc);
   endfunction


	 return s5_stage( s4_stage( s3_stage( s2_stage( s1_stage(operands))))); //INTERFACE CHANGES
endfunction

interface Ifc_add#(numeric type e, numeric type m, numeric type nos);
	method Action send(Tuple3#(FloatingPoint#(e,m),
		 FloatingPoint#(e,m),
		 RoundMode) operands);

	method ReturnType#(e,m) receive();
endinterface
module mk_add(Ifc_add#(e,m,nos))
	provisos(
		 Add#(a__, TLog#(TAdd#(1, TAdd#(TAdd#(m, 1), TAdd#(m, 1)))), TAdd#(e, 1)),
		 Add#(b__, TLog#(TAdd#(1, TAdd#(m, 5))), TAdd#(e, 1))
	);


	Vector#(nos,Reg#(Tuple2#(FloatingPoint#(e,m),Exception))) rg_stage_out <- replicateM(mkReg(tuple2(unpack(0),unpack(0))));
	Vector#(nos,Reg#(Bit#(1))) rg_stage_valid <- replicateM(mkDReg(0));

	rule rl_pipeline;
		 for(Integer i = 1 ; i <= valueOf(nos) -1 ; i = i+1)
		 begin
				rg_stage_out[i] <= rg_stage_out[i-1];
				rg_stage_valid[i] <= rg_stage_valid[i-1];
		 end
	endrule
	method Action send(Tuple3#(FloatingPoint#(e,m),
				FloatingPoint#(e,m),
				RoundMode) operands);		
					 
					 rg_stage_out[0] <= fp_add(operands);
					 rg_stage_valid[0] <= 1;

	endmethod

	method ReturnType#(e,m) receive();
		let x = ReturnType{valid:rg_stage_valid[valueOf(nos)-1] ,value:tpl_1(rg_stage_out[valueOf(nos)-1]) ,ex:tpl_2(rg_stage_out[valueOf(nos)-1])};
		return x;
	endmethod 
endmodule

interface Ifc_add_s3;
    method Action get(Bit#(2) op, Float x);
    method Float add_result; 
    method Action pass_in(Float x, Float exp_r);
    method Float pass_out; 
    method Float exp_res_out;
endinterface

module mkadd_s3(Ifc_add_s3); 
    Reg#(Float) input_x <- mkReg(0); 
    Reg#(Float) add_res <- mkReg(0);
    Reg#(Float) exp_pass <- mkReg(0);
    Reg#(Float) in <- mkReg(0);
    Reg#(Bit#(2)) opcode <- mkReg(0);
    Reg#(Bool) input_valid_add <- mkReg(False);

    Reg#(Bool) add1_valid <- mkReg(False);
    Reg#(Bool) add2_valid <- mkReg(False);
    Reg#(Bool) add3_valid <- mkReg(False);
    Reg#(Bool) add4_valid <- mkReg(False);

    Ifc_add#(8,23,4) add <- mk_add;

    rule add_1 (((opcode == 2'b00) || (opcode == 2'b01))&& (input_valid_add));
        RoundMode op4 = Rnd_Nearest_Even;
        add.send(tuple3(input_x,1,op4));
        add1_valid <= True;
    endrule 
    rule add1 (add1_valid);
	    add2_valid <= True;
    endrule 
    rule add2 (add2_valid);
	    add3_valid <= True;
    endrule
    rule add3 (add3_valid);
	    add4_valid <= True;
    endrule

    rule add_1_res (((opcode == 2'b00) || (opcode == 2'b01)) && input_valid_add && add4_valid );
        let r = add.receive();
        add_res <= r.value;
    endrule

    rule add2_res (((opcode == 2'b10)||(opcode == 2'b11)) && input_valid_add );
        add_res <= input_x;
    endrule

    method Action get(Bit#(2) op, Float x);
       input_x <= x;
       opcode <= op;
       input_valid_add <= True;
    endmethod

    method Float add_result; 
       return add_res;
    endmethod 

    method Action pass_in(Float x, Float exp_r);
       in <= x;
       exp_pass <= exp_r;
    endmethod 
    method Float pass_out;
       return in;
    endmethod
    method Float exp_res_out;
       return exp_pass;
    endmethod
endmodule
//============================ End of ADD module ===========================

//============================ SUB module ==================================

function Tuple2#(FloatingPoint#(e,m),Exception) fp_sub (Tuple3#(FloatingPoint#(e,m),FloatingPoint#(e,m),RoundMode) operands)

   provisos(
      // per request of bsc
      Add#(a__, TLog#(TAdd#(1, TAdd#(m, 5))), TAdd#(e, 1))
      );

   function Tuple7#(CommonState#(e,m),
		    Bit#(TAdd#(m,5)),
		    Bit#(TAdd#(m,5)),
		    Bool,
		    Bool,
		    Bit#(e),
				Bit#(e)) s1_stage(Tuple3#(
								FloatingPoint#(e,m),
					      FloatingPoint#(e,m),
					      RoundMode) op);

      match {.opA, .opB, .rmode } = op;

      CommonState#(e,m) s = CommonState {
	 res: tagged Invalid,
	 exc: defaultValue,
	 rmode: rmode
	 };

      Int#(TAdd#(e,2)) expA = isSubNormal(opA) ? fromInteger(minexp(opA)) : signExtend(unpack(unbias(opA)));
      Int#(TAdd#(e,2)) expB = isSubNormal(opB) ? fromInteger(minexp(opB)) : signExtend(unpack(unbias(opB)));

      Bit#(TAdd#(m,5)) sfdA = {1'b0, getHiddenBit(opA), opA.sfd, 3'b0};
      Bit#(TAdd#(m,5)) sfdB = {1'b0, getHiddenBit(opB), opB.sfd, 3'b0};

      Bit#(TAdd#(m,5)) x;
      Bit#(TAdd#(m,5)) y;
      Bool sgn;
      Bool sub;
      Bit#(e) exp;
      Bit#(e) expdiff;

      if ((expB > expA) || ((expB == expA) && (sfdB > sfdA))) begin
	 exp = opB.exp;
	 expdiff = truncate(pack(expB - expA));
	 x = sfdB;
	 y = sfdA;
	 sgn = opB.sign;
	 sub = (opB.sign != opA.sign);
      end
      else begin
	 exp = opA.exp;
	 expdiff = truncate(pack(expA - expB));
	 x = sfdA;
	 y = sfdB;
	 sgn = opA.sign;
	 sub = (opA.sign != opB.sign);
      end

      if (isSNaN(opA)) begin
	 s.res = tagged Valid nanQuiet(opA);
	 s.exc.invalid_op = True;
      end
      else if (isSNaN(opB)) begin
	 s.res = tagged Valid nanQuiet(opB);
	 s.exc.invalid_op = True;
      end
      else if (isQNaN(opA)) begin
	 s.res = tagged Valid opA;
      end
      else if (isQNaN(opB)) begin
	 s.res = tagged Valid opB;
      end
      else if (isInfinity(opA) && isInfinity(opB)) begin
	 if (opA.sign == opB.sign)
	    s.res = tagged Valid infinity(opA.sign);
	 else begin
	    s.res = tagged Valid qnan();
	    s.exc.invalid_op = True;
	 end
      end
      else if (isInfinity(opA)) begin
	 s.res = tagged Valid opA;
      end
      else if (isInfinity(opB)) begin
	 s.res = tagged Valid opB;
      end

      return tuple7(s,
		    x,
		    y,
		    sgn,
		    sub,
		    exp,
		    expdiff);
   endfunction

   function Tuple6#(CommonState#(e,m),
		    Bit#(TAdd#(m,5)),
		    Bit#(TAdd#(m,5)),
		    Bool,
		    Bool,
		    Bit#(e)) s2_stage(Tuple7#(CommonState#(e,m),
					      Bit#(TAdd#(m,5)),
					      Bit#(TAdd#(m,5)),
					      Bool,
					      Bool,
					      Bit#(e),
					      Bit#(e)) op);

      match {.s, .opA, .opB, .sign, .subtract, .exp, .diff} = op;

      if (s.res matches tagged Invalid) begin
	 if (diff < fromInteger(valueOf(m) + 5)) begin
	    Bit#(TAdd#(m,5)) guard = opB;

	    guard = opB << (fromInteger(valueOf(m) + 5) - diff);
	    opB = opB >> diff;
	    opB[0] = opB[0] | (|guard);
	 end
	 else if (|opB == 1) begin
	    opB = 1;
	 end
      end

      return tuple6(s,
		    opA,
		    opB,
		    sign,
		    subtract,
		    exp);
   endfunction

   function Tuple6#(CommonState#(e,m),
		    Bit#(TAdd#(m,5)),
		    Bit#(TAdd#(m,5)),
		    Bool,
		    Bool,
		    Bit#(e)) s3_stage(Tuple6#(CommonState#(e,m),
					      Bit#(TAdd#(m,5)),
					      Bit#(TAdd#(m,5)),
					      Bool,
					      Bool,
					      Bit#(e)) op);

      match {.s, .a, .b, .sign, .subtract, .exp} = op;

      let sum = a + b;
      let diff = a - b;

      return tuple6(s,
		    sum,
		    diff,
		    sign,
		    subtract,
		    exp);
   endfunction

   function Tuple4#(CommonState#(e,m),
		    FloatingPoint#(e,m),
		    Bit#(2),
		    Bool) s4_stage(Tuple6#(CommonState#(e,m),
					   Bit#(TAdd#(m,5)),
					   Bit#(TAdd#(m,5)),
					   Bool,
					   Bool,
					   Bit#(e)) op);

      match {.s, .addres, .subres, .sign, .subtract, .exp} = op;

      FloatingPoint#(e,m) out = defaultValue;
      Bit#(2) guard = 0;

      if (s.res matches tagged Invalid) begin
	 Bit#(TAdd#(m,5)) result;

	 if (subtract) begin
	    result = subres;
	 end
	 else begin
            result = addres;
	 end

	 out.sign = sign;
	 out.exp = exp;

	 let y = normalize(out, result);
	 out = tpl_1(y);
	 guard = tpl_2(y);
	 s.exc = s.exc | tpl_3(y);
      end

      return tuple4(s,
		    out,
		    guard,
		    subtract);
   endfunction

   function Tuple2#(FloatingPoint#(e,m),
		    Exception) s5_stage(Tuple4#(CommonState#(e,m),
						FloatingPoint#(e,m),
						Bit#(2),
						Bool) op);

      match {.s, .rnd, .guard, .subtract} = op;

      FloatingPoint#(e,m) out = rnd;

      if (s.res matches tagged Valid .x) begin
	 out = x;
      end
      else begin
	 let y = round(s.rmode, out, guard);
	 out = tpl_1(y);
	 s.exc = s.exc | tpl_2(y);
      end

      // adjust sign for exact zero result
      if (isZero(out) && !s.exc.inexact && subtract) begin
	 out.sign = (s.rmode == Rnd_Minus_Inf);
      end

      return tuple2(canonicalize(out),s.exc);
   endfunction

	 return s5_stage( s4_stage( s3_stage( s2_stage( s1_stage(operands))))); //INTERFACE CHANGES
endfunction

interface Ifc_sub#(numeric type e, numeric type m, numeric type nos);
	method Action send(Tuple3#(FloatingPoint#(e,m),
		 FloatingPoint#(e,m),
		 RoundMode) operands);
	method ReturnType#(e,m) receive();
endinterface
module mk_sub(Ifc_sub#(e,m,nos))
	provisos(
		 Add#(a__, TLog#(TAdd#(1, TAdd#(TAdd#(m, 1), TAdd#(m, 1)))), TAdd#(e, 1)),
		 Add#(b__, TLog#(TAdd#(1, TAdd#(m, 5))), TAdd#(e, 1))
	);


	Vector#(nos,Reg#(Tuple2#(FloatingPoint#(e,m),Exception))) rg_stage_out <- replicateM(mkReg(tuple2(unpack(0),unpack(0))));
	Vector#(nos,Reg#(Bit#(1))) rg_stage_valid <- replicateM(mkDReg(0));

	rule rl_pipeline;
		 for(Integer i = 1 ; i <= valueOf(nos) -1 ; i = i+1)
		 begin
				rg_stage_out[i] <= rg_stage_out[i-1];
				rg_stage_valid[i] <= rg_stage_valid[i-1];
		 end
	endrule
	method Action send(Tuple3#(FloatingPoint#(e,m),
				FloatingPoint#(e,m),
				RoundMode) operands);		
					 
					 rg_stage_out[0] <= fp_sub(operands);
					 rg_stage_valid[0] <= 1;

	endmethod

	method ReturnType#(e,m) receive();
		let x = ReturnType{valid:rg_stage_valid[valueOf(nos)-1] ,value:tpl_1(rg_stage_out[valueOf(nos)-1]) ,ex:tpl_2(rg_stage_out[valueOf(nos)-1])};
		return x;

	endmethod 
endmodule


interface Ifc_sub_s4;
    method Action get(Bit#(2) op, Float x);
    method Float sub_result; 
    method Action pass_in(Float x, Float add_r);
    method Float pass_out; 
    method Float add_res_out;
endinterface

module mksub_s4(Ifc_sub_s4); 
    Reg#(Float) input_x <- mkReg(0); 
    Reg#(Float) sub_res <- mkReg(0);
    Reg#(Float) add_pass <- mkReg(0);
    Reg#(Float) in <- mkReg(0);

    Reg#(Bit#(2)) opcode <- mkReg(0);
    Reg#(Bool) input_valid_sub <- mkReg(False);

    Reg#(Bool) sub1_valid <- mkReg(False);
    Reg#(Bool) sub2_valid <- mkReg(False);
    Reg#(Bool) sub3_valid <- mkReg(False);
    Reg#(Bool) sub4_valid <- mkReg(False);

    Ifc_sub#(8,23,4) sub <- mk_sub;

    rule sub_1 (((opcode == 2'b00) || (opcode == 2'b11))&& (input_valid_sub));
        RoundMode op4 = Rnd_Nearest_Even;
        sub.send(tuple3(input_x,-1,op4));
        sub1_valid <= True;
    endrule 
    rule sub1 (sub1_valid);
	    sub2_valid <= True;
    endrule 
    rule sub2 (sub2_valid);
	    sub3_valid <= True;
    endrule
    rule sub3 (sub3_valid);
	    sub4_valid <= True;
    endrule

    rule sub_1_res (((opcode == 2'b00) || (opcode == 2'b11)) && input_valid_sub && sub4_valid );
        let r = sub.receive();
        sub_res <= r.value;
    endrule

    rule sub2_res (((opcode == 2'b01)||(opcode == 2'b10)) && input_valid_sub );
        sub_res <= input_x;
    endrule

    method Action get(Bit#(2) op, Float x);
       input_x <= x;
       opcode <= op;
       input_valid_sub <= True;
    endmethod

    method Float sub_result; 
       return sub_res;
    endmethod 

    method Action pass_in(Float x, Float add_r);
       in <= x;
       add_pass <= add_r;
    endmethod 
    method Float pass_out;
       return in;
    endmethod
    method Float add_res_out;
       return add_pass;
    endmethod
endmodule 
//============================= End of SUB module ============================

//================================ DIV module ================================

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

    Ifc_div_op div <- mk_div_op;

    rule div_1 (((opcode == 2'b00)) && (input_valid_div));
        div.get(input_x, input_z);
        div1_valid <= True;
    endrule 

    rule div_1res (((opcode == 2'b00)) && (div1_valid));
        let d = div.res_op;
        div_res <= d;
    endrule 

    rule div_2 (((opcode == 2'b01)) && (input_valid_div));
        div.get(1, input_z);
        div2_valid <= True;
    endrule 

    rule div_2res (((opcode == 2'b01)) && (div2_valid));
        let d = div.res_op;
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

// ============================= End of DIV module =============================

// ============================== EXP module ===================================

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
	Reg#(Float) x_wire <- mkWire;
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

	rule enable_stage_1(en_stage1);
		stage0_x <=tagged Valid(x_wire);
	endrule 

	method Action get_x(Float x_input);
		x_wire <= x_input;
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

    Ifc_exp exp <- mkExp;

    rule exp_1 (((opcode == 2'b00) || (opcode == 2'b01) || (opcode == 2'b11)) && input_valid_exp);
       exp.get_x(input_x);
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
        let r = exp.res;
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


//============================== End of exp.bsv =========================

//============================== factor.bsv =============================

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

//=========================== End of factor.bsv =========================
//============================== comp.bsv ===============================

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
//============================ End of comp.bsv ===========================

//=============================== main.bsv ===============================
interface Ifc_main;
    method Action get(Bit#(2) op, Float x);
    method Float main_result;
endinterface

module mk_main(Ifc_main);
	Reg#(int) cycle <- mkReg(0);

    Reg#(Float) input_x <- mkReg(0);
    Reg#(Float) s_1 <- mkReg(0);
    Reg#(Float) s_2 <- mkReg(0);
	Reg#(Float) s_3 <- mkReg(0);
    Reg#(Float) s_4 <- mkReg(0);
	Reg#(Float) s_5 <- mkReg(0);
    Reg#(Float) s_6 <- mkReg(0);

    Reg#(Float) y <- mkReg(0);
    Reg#(Float) factor_res <- mkReg(0);

    Reg#(Bit#(2)) opcode <- mkReg(0);

    Reg#(Bool) stage_1_valid <- mkReg(False);
	Reg#(Bool) stage_2_valid <- mkReg(False);
    Reg#(Bool) stage_3_valid <- mkReg(False);
    Reg#(Bool) stage_4_valid <- mkReg(False);
    Reg#(Bool) stage_5_valid <- mkReg(False);
    Reg#(Bool) stage_6_valid <- mkReg(False);
	Reg#(Bool) result_valid <- mkReg(False);
	Reg#(Bool) delay_valid <- mkReg(False);
	Reg#(Bool) return_valid <- mkReg(False);
	Reg#(Bool) delay_disable_stage_1_valid <- mkReg(False);

	PulseWire enable_stage_1 <- mkPulseWire;

    Ifc_factor_s1 factor_s1 <- mkfactor_s1;
    Ifc_exp_s2 exp_s2 <- mkexp_s2;
    Ifc_add_s3 add_s3 <- mkadd_s3;
    Ifc_sub_s4 sub_s4 <- mksub_s4;
    Ifc_div_s5 div_s5 <- mkdiv_s5;
	Ifc_comp_s6 comp_s6 <- mk_comp_s6;

	rule cyc;
		cycle <= cycle + 1;
	endrule
    
	// STAGE 1 
    rule stage_1 (stage_1_valid);
        factor_s1.get(opcode, input_x);
        factor_s1.pass_in(input_x);
        stage_2_valid <= True;
    endrule

	rule disable_stage_2 (!stage_1_valid);
	    stage_2_valid <= False;
	endrule 
    
	// STAGE 2
    rule stage_2 (stage_2_valid);
        let factor_res = factor_s1.factor_result;
        let x1 = factor_s1.pass_out;
		s_1 <= factor_res;
        exp_s2.pass_in(x1);
        exp_s2.get(opcode, factor_res);
        stage_3_valid <= True;
    endrule 

	rule disable_stage_3 (!stage_2_valid);
	    stage_3_valid <= False;
	endrule
    
	// STAGE 3
    rule stage_3 (stage_3_valid);
        let exp_res = exp_s2.exp_result;
        let x2 = exp_s2.pass_out;
		s_2 <= exp_res;
        add_s3.pass_in(x2, exp_res);
        add_s3.get(opcode, exp_res);
        stage_4_valid <= True;
    endrule 

	rule disable_stage_4 (!stage_3_valid);
	    stage_4_valid <= False;
	endrule
    
	// STAGE 4
    rule stage_4 (stage_4_valid);
        let x3 = add_s3.add_result;
        let x4 = add_s3.pass_out;
        let x5 = add_s3.exp_res_out;
		s_3 <= x3;
        sub_s4.pass_in(x4, x3);
        sub_s4.get(opcode, x5);
        stage_5_valid <= True;
    endrule 

	rule disable_stage_5 (!stage_4_valid);
	    stage_5_valid <= False;
	endrule
    
	// STAGE 5
    rule stage_5 (stage_5_valid);
        let sub_res = sub_s4.sub_result;
        let add_res = sub_s4.add_res_out;
        let x6 = sub_s4.pass_out;
		s_4 <= sub_res;
        div_s5.get(opcode, sub_res, add_res);
        div_s5.pass_in(x6, sub_res);
        stage_6_valid <= True;
    endrule 

	rule disable_stage_6 (!stage_5_valid);
	    stage_6_valid <= False;
	endrule
    
	// STAGE 6
    rule stage_6 (stage_6_valid);
        let div_res = div_s5.div_result;
        let x7 = div_s5.sub_res_out;
        let x8 = div_s5.pass_out;
		s_5 <= div_res;
        comp_s6.get(opcode, x8, x7, div_res);
        delay_valid <= True;
    endrule 

	rule disable_delay (!stage_6_valid);
	    delay_valid <= False;
	endrule

	rule delayed_res (delay_valid);
		result_valid <= True;
	endrule 
	
	rule disable_result (!delay_valid);
		result_valid <= False;
	endrule

	rule disable_stage_1 (!enable_stage_1);
		stage_1_valid <= False;
	endrule 

	rule en_stage_1 (enable_stage_1);
        stage_1_valid <= True;
    endrule

    method Action get(Bit#(2) op, Float x);
        input_x <= x;
        opcode <= op;
		enable_stage_1.send();
    endmethod

    method Float main_result if (result_valid);
		let comp_res = comp_s6.comp_result;
		return comp_res;
    endmethod
endmodule 
//=========================== testbench ================================

(*synthesize*)
module mkTb();

    Reg#(int) status <- mkReg(0);
    Ifc_main ifc_main <- mk_main;

    rule stage;
        status <= status + 1;
    endrule 

    rule get_input1(status == 0);
        ifc_main.get(2'b11, 2.3);
    endrule

    rule get_input2(status == 1);
        ifc_main.get(2'b11, 3.3);
    endrule

	rule get_input3(status == 2);
	    ifc_main.get(2'b11, 7.3);
    endrule

	rule get_input4(status == 3);
	    ifc_main.get(2'b11, 10.3);
    endrule

	rule get_input5(status == 4);
	    ifc_main.get(2'b11, 15.3);
    endrule
	
    rule result;
		let r = ifc_main.main_result;
		$display("Result = %b : cycle =%d", r, status);
	endrule

    rule finish(status == 30);
        $finish(0);
    endrule
endmodule: mkTb
