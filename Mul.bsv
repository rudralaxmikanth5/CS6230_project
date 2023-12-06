//import fpu_common    ::*;
import Vector            ::*;
import FloatingPoint     ::*;
import Real              ::*;
import BUtils            ::*;
import DefaultValue      ::*;
import FShow             ::*;
import GetPut            ::*;
import ClientServer      ::*;
import FIFO              ::*;
import FixedPoint        ::*;
import DReg ::*;
import GetPut            ::*;

// export mk_multiplier;
// export Ifc_fpu_multiplier;
// export ReturnType;
//`include "fpu_parameters.bsv"

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
	       'b01: out = din_inc;
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

function Tuple2#(FloatingPoint#(e,m),Exception) fn_fpu_multiplier ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2, RoundMode rmode )
provisos(
	Add#(a__, TLog#(TAdd#(1, TAdd#(TAdd#(m, 1), TAdd#(m, 1)))), TAdd#(e, 1))
);

   function Tuple5#(CommonState#(e,m),
		    Bit#(TAdd#(m,1)),
		    Bit#(TAdd#(m,1)),
		    Int#(TAdd#(e,2)),
		    Bool) s1_stage(Tuple3#(FloatingPoint#(e,m),
					   FloatingPoint#(e,m),
					   RoundMode) op);

      match { .opA, .opB, .rmode } = op;

      CommonState#(e,m) s = CommonState {
	 res: tagged Invalid,
	 exc: defaultValue,
	 rmode: rmode
	 };

      Int#(TAdd#(e,2)) expA = isSubNormal(opA) ? fromInteger(minexp(opA)) : signExtend(unpack(unbias(opA)));
      Int#(TAdd#(e,2)) expB = isSubNormal(opB) ? fromInteger(minexp(opB)) : signExtend(unpack(unbias(opB)));
      Int#(TAdd#(e,2)) newexp = expA + expB;

      Bool sign = (opA.sign != opB.sign);

      Bit#(TAdd#(m,1)) opAsfd = { getHiddenBit(opA), opA.sfd };
      Bit#(TAdd#(m,1)) opBsfd = { getHiddenBit(opB), opB.sfd };

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
      else if ((isInfinity(opA) && isZero(opB)) || (isZero(opA) && isInfinity(opB))) begin
	 s.res = tagged Valid qnan();
	 s.exc.invalid_op = True;
      end
      else if (isInfinity(opA) || isInfinity(opB)) begin
	 s.res = tagged Valid infinity(opA.sign != opB.sign);
      end
      else if (isZero(opA) || isZero(opB)) begin
	 s.res = tagged Valid zero(opA.sign != opB.sign);
      end
      else if (newexp > fromInteger(maxexp(opA))) begin
	 FloatingPoint#(e,m) out;
	 out.sign = (opA.sign != opB.sign);
	 out.exp = maxBound - 1;
	 out.sfd = maxBound;

	 s.exc.overflow = True;
	 s.exc.inexact = True;

	 let y = round(rmode, out, '1);
	 s.res = tagged Valid tpl_1(y);
	 s.exc = s.exc | tpl_2(y);
      end
      else if (newexp < (fromInteger(minexp_subnormal(opA))-2)) begin
	 FloatingPoint#(e,m) out;
	 out.sign = (opA.sign != opB.sign);
	 out.exp = 0;
	 out.sfd = 0;

	 s.exc.underflow = True;
	 s.exc.inexact = True;

	 let y = round(rmode, out, 'b01);
	 s.res = tagged Valid tpl_1(y);
	 s.exc = s.exc | tpl_2(y);
      end

      return tuple5(s,
		    opAsfd,
		    opBsfd,
		    newexp,
		    sign);
   endfunction

   function Tuple4#(CommonState#(e,m),
		    Bit#(TAdd#(TAdd#(m,1),TAdd#(m,1))),
		    Int#(TAdd#(e,2)),
		    Bool) s2_stage(Tuple5#(CommonState#(e,m),
					   Bit#(TAdd#(m,1)),
					   Bit#(TAdd#(m,1)),
					   Int#(TAdd#(e,2)),
					   Bool) op);

      match {.s, .opAsfd, .opBsfd, .exp, .sign} = op;

      Bit#(TAdd#(TAdd#(m,1),TAdd#(m,1))) sfdres = primMul(opAsfd, opBsfd);

      return tuple4(s,
		    sfdres,
		    exp,
		    sign);
   endfunction

   function Tuple4#(CommonState#(e,m),
		    Bit#(TAdd#(TAdd#(m,1),TAdd#(m,1))),
		    Int#(TAdd#(e,2)),
		    Bool) s3_stage(Tuple4#(CommonState#(e,m),
					   Bit#(TAdd#(TAdd#(m,1),TAdd#(m,1))),
					   Int#(TAdd#(e,2)),
					   Bool) op);
      return op;
   endfunction

   function Tuple3#(CommonState#(e,m),
		    FloatingPoint#(e,m),
		    Bit#(2)) s4_stage(Tuple4#(CommonState#(e,m),
					      Bit#(TAdd#(TAdd#(m,1),TAdd#(m,1))),
					      Int#(TAdd#(e,2)),
					      Bool) op);

      match {.s, .sfdres, .exp, .sign} = op;

      FloatingPoint#(e,m) result = defaultValue;
      Bit#(2) guard = ?;

      if (s.res matches tagged Invalid) begin
	 //$display("sfdres = 'h%x", sfdres);

	 let shift = fromInteger(minexp(result)) - exp;
	 if (shift > 0) begin
	    // subnormal
	    Bit#(1) sfdlsb = |(sfdres << (fromInteger(valueOf(TAdd#(TAdd#(m,1),TAdd#(m,1)))) - shift));

	    //$display("sfdlsb = |'h%x = 'b%b", (sfdres << (fromInteger(valueOf(TAdd#(TAdd#(m,1),TAdd#(m,1)))) - shift)), sfdlsb);

            sfdres = sfdres >> shift;
            sfdres[0] = sfdres[0] | sfdlsb;

	    result.exp = 0;
	 end
	 else begin
	    result.exp = cExtend(exp + fromInteger(bias(result)));
	 end

	 // $display("shift = %d", shift);
	 // $display("sfdres = 'h%x", sfdres);
	 // $display("result = ", fshow(result));
	 // $display("exc = 'b%b", pack(exc));
	 // $display("zeros = %d", countZerosMSB(sfdres));

	 result.sign = sign;
	 let y = normalize(result, sfdres);
	 result = tpl_1(y);
	 guard = tpl_2(y);
	 s.exc = s.exc | tpl_3(y);

	 // $display("result = ", fshow(result));
	 // $display("exc = 'b%b", pack(exc));
      end

      return tuple3(s,
		    result,
		    guard);
   endfunction

   function Tuple2#(FloatingPoint#(e,m),
		    Exception) s5_stage(Tuple3#(CommonState#(e,m),
						FloatingPoint#(e,m),
						Bit#(2)) op);

      match {.s, .rnd, .guard} = op;

      FloatingPoint#(e,m) out = rnd;

      if (s.res matches tagged Valid .x)
	 out = x;
      else begin
	 let y = round(s.rmode, out, guard);
	 out = tpl_1(y);
	 s.exc = s.exc | tpl_2(y);
      end
//*********************************************************************************
	if(out.exp == 1 && out.sfd == 0 && guard == 'h3)
		s.exc.underflow = False;
//************************************************************************************
      return tuple2(canonicalize(out),s.exc);
   endfunction

   return s5_stage( s4_stage( s3_stage( s2_stage( s1_stage(tuple3(in1,in2,rmode)) ) ) ) );
endfunction

interface Ifc_fpu_multiplier#(numeric type e, numeric type m,numeric type nos);
//	method Action send(FloatingPoint#(e,m) in1 ,FloatingPoint#(e,m) in2,RoundMode rmode);
	method Action send(Tuple3#(FloatingPoint#(e,m),FloatingPoint#(e,m), RoundMode) operands);					 
//	method Tuple2#(Bit#(1),Tuple2#(FloatingPoint#(e,m),Exception)) receive();
	method ReturnType#(e,m) receive();
endinterface
module mk_multiplier(Ifc_fpu_multiplier#(e,m,nos))
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
	method Action send(Tuple3#(FloatingPoint#(e,m),FloatingPoint#(e,m), RoundMode) operands);					 
					 rg_stage_out[0] <= fn_fpu_multiplier(tpl_1(operands),tpl_2(operands),tpl_3(operands));
					 rg_stage_valid[0] <= 1;

	endmethod
	method ReturnType#(e,m) receive();
		return ReturnType{valid:rg_stage_valid[valueOf(nos)-1],value:tpl_1(rg_stage_out[valueOf(nos)-1]),ex:tpl_2(rg_stage_out[valueOf(nos)-1])} ;
    endmethod  
endmodule

(*synthesize*)
 module mkTb();
 	Reg#(int) rg_cycle <- mkReg(0);
 	Ifc_fpu_multiplier#(8,23,4) ifc <- mk_multiplier();
 	rule rl_cycle;
 		rg_cycle <= rg_cycle +1;
 		if(rg_cycle>9)
 			$finish(0);
 	endrule
 	rule rl_stage1(rg_cycle == 1);               //input 1
 		RoundMode op4 = Rnd_Nearest_Even;
 		ifc.send(tuple3(1.2,12.56,op4));
 	endrule
	rule rl_stage2(rg_cycle == 2);               //input 2
	    RoundMode op4 = Rnd_Nearest_Even;
		ifc.send(tuple3(1.8,22.3,op4));
    endrule
	rule rl_stage3(rg_cycle == 3);              //input 3
	    RoundMode op4 = Rnd_Nearest_Even;
	    ifc.send(tuple3(14.5,2.3,op4));
    endrule
	rule rl_stage4(rg_cycle == 4);              //input 4
	    RoundMode op4 = Rnd_Nearest_Even;
	    ifc.send(tuple3(1.5,6.3,op4));
    endrule
 	rule rl_receive;
// 		match {.valid, .out} = ifc.receive();
 		let x = ifc.receive();
 		$display("cycle %d: result = %b",rg_cycle,x.value);
 	endrule
 endmodule
 