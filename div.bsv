import Real              ::*;
import Vector            ::*;
import BUtils            ::*;
import GetPut            ::*;
import ClientServer      ::*;
import FIFO              ::*;
import FixedPoint        ::*;
import FloatingPoint     ::*;
import Divide            ::*;
import GetPut            ::*;
//typedef FloatingPoint#(8,23) Float;

// typedef struct {
//    Bool        sign;
//    Bit#(e)     exp;
//    Bit#(m)     sfd;
// } FloatingPoint#(numeric type e, numeric type m) deriving (Bits);

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
   provisos(  Add#(m, 1, m1) , Add#(m, 2, m2));

   FloatingPoint#(e,m) out = 0;
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

typedef struct {
   Int#(TAdd#(1,n)) d;
   Int#(TAdd#(2,TAdd#(n,n))) r;
   Int#(TAdd#(1,n)) q;
} DivState#(numeric type n) deriving(Bits, Eq, FShow);

function DivState#(n) operands_to_divstate(Tuple2#(UInt#(m),UInt#(n)) ops)
   provisos(Add#(n, n, m));

   match {.n, .d} = ops;
   return DivState{d: unpack({1'b0,pack(d)}),
                   q: 0,
                   r: unpack({2'b0,pack(n)})
                  };
endfunction

function DivState#(n) iterate_divstate(DivState#(n) f, indexT start_index, Integer s)
   provisos(Add#(n, n, m),
   Alias#(UInt#(TAdd#(TLog#(n),1)), indexT));

   Int#(TAdd#(2,m)) bigd = unpack(zExtendLSB(pack(f.d)));
   for (indexT i = 0; i < fromInteger(s); i = i + 1) begin
      indexT index = start_index + i;
      if (!done(index, fromInteger(valueOf(n)))) begin
         if (f.r >= 0) begin
            f.q = (f.q << 1) | 1;
            f.r = (f.r << 1) - bigd;
         end
         else begin
            f.q = (f.q << 1);
            f.r = (f.r << 1) + bigd;
         end
      end
   end
   return f;
endfunction

function Tuple2#(UInt#(n),UInt#(n)) divstate_to_returns(DivState#(n) f);
	f.q = f.q + (-(~f.q));
	if (f.r < 0) begin
	   f.q = f.q - 1;
	   f.r = f.r + cExtendLSB(f.d);
	end
	UInt#(TAdd#(1,n)) qq = unpack(pack(f.q));
	UInt#(TAdd#(1,n)) rr = cExtendLSB(f.r);
	return tuple2(truncate(qq),truncate(rr));
 endfunction

function Bool done(t cmp, t top) provisos(Ord#(t)) = (cmp > top);

module mkDivider#(Integer s)(Server#(Tuple2#(UInt#(m),UInt#(n)),Tuple2#(UInt#(n),UInt#(n))))
   provisos(Add#(n, n, m));

   FIFO#(Tuple2#(UInt#(m),UInt#(n))) fRequest <- mkLFIFO;
   FIFO#(Tuple2#(UInt#(n),UInt#(n))) fResponse <- mkLFIFO;
   FIFO#(DivState#(n)) fFirst <- mkLFIFO;

   rule start;
      fFirst.enq(operands_to_divstate(fRequest.first));
      fRequest.deq;
   endrule

   FIFO#(DivState#(n)) fThis = fFirst;
   FIFO#(DivState#(n)) fNext;

   for (Integer i = 0; !done(i, valueOf(n)); i = i + s) begin
      fNext <- mkLFIFO;
      rule work;
         DivState#(n) f <- toGet(fThis).get;
         fNext.enq(iterate_divstate(f, fromInteger(i), s));
      endrule
      fThis = fNext;
   end

   rule finish;
      DivState#(n) f <- toGet(fThis).get;
      fResponse.enq(divstate_to_returns(f));
   endrule

   interface request = toPut(fRequest);
   interface response = toGet(fResponse);

endmodule

module mkFloatingPointDivider#(Server#(Tuple2#(UInt#(nbits),UInt#(dbits)),Tuple2#(UInt#(dbits),UInt#(dbits))) div)(Server#(Tuple3#(FloatingPoint#(e,m), FloatingPoint#(e,m), RoundMode), Tuple2#(FloatingPoint#(e,m),Exception)))
   provisos(
      Add#(e,1,ebits),
      Add#(ebits,1,ebits1),
      Add#(m,1,m1bits),
      Add#(m,5,dbits),
      Add#(dbits,1,dbits1),
      Add#(dbits,dbits,nbits),
      // per bsc request
      Mul#(2, dbits, nbits),
      Add#(1, nbits, TAdd#(dbits, a__)),
      Add#(m, b__, dbits1),
      Add#(c__, TLog#(TAdd#(1, dbits1)), TAdd#(e, 1)),
      Add#(d__, e, ebits1),
      Add#(e__, TLog#(TAdd#(1, m1bits)), ebits1),
      Add#(2, f__, dbits),
      Add#(2, g__, dbits1)
      );
   FIFO#(Tuple3#(FloatingPoint#(e,m),
		 FloatingPoint#(e,m),
		 RoundMode))                     fOperands_S0  <- mkLFIFO;

//    Server#(Tuple2#(UInt#(nbits),UInt#(dbits)),Tuple2#(UInt#(dbits),UInt#(dbits))) div <- mkDivider(1);

   FIFO#(Tuple7#(Maybe#(FloatingPoint#(e,m)),
		 Exception,
		 RoundMode,
		 FloatingPoint#(e,m),
		 Bit#(nbits),
		 Bit#(dbits),
		 Bit#(e))) fState_S1 <- mkLFIFO;

   rule s1_stage;
      match {.in1, .in2, .rmode} <- toGet(fOperands_S0).get;
      Maybe#(FloatingPoint#(e,m)) out = tagged Invalid;
      Exception exc = defaultValue;
      FloatingPoint#(e,m) result = defaultValue;
      Bit#(m1bits) sfdA = {getHiddenBit(in1), in1.sfd};
      Bit#(m1bits) sfdB = {getHiddenBit(in2), in2.sfd};
      Bit#(e) shift = 0;

      let zerosA = countZerosMSB(sfdA);
      sfdA = sfdA << zerosA;

      let zerosB = countZerosMSB(sfdB);
      sfdB = sfdB << zerosB;

      // calculate the new exponent
      Int#(ebits1) exp1 = isSubNormal(in1) ? fromInteger(minexp(in1)) : signExtend(unpack(unbias(in1)));
      Int#(ebits1) exp2 = isSubNormal(in2) ? fromInteger(minexp(in2)) : signExtend(unpack(unbias(in2)));
      Int#(ebits1) newexp = (exp1 - zeroExtend(unpack(pack(zerosA)))) - (exp2 - zeroExtend(unpack(pack(zerosB))));

      Bit#(nbits) opA = zExtendLSB({ 1'b0, sfdA });
      Bit#(dbits) opB = zExtend({ sfdB, 4'b0000 });

      // calculate the sign
      result.sign = in1.sign != in2.sign;

      if (isSNaN(in1)) begin
	 out = tagged Valid nanQuiet(in1);
	 exc.invalid_op = True;
      end
      else if (isSNaN(in2)) begin
	 out = tagged Valid nanQuiet(in2);
	 exc.invalid_op = True;
      end
      else if (isQNaN(in1)) begin
	 out = tagged Valid in1;
      end
      else if (isQNaN(in2)) begin
	 out = tagged Valid in2;
      end
      else if ((isInfinity(in1) && isInfinity(in2)) || (isZero(in1) && isZero(in2))) begin
	 out = tagged Valid qnan();
	 exc.invalid_op = True;
      end
      else if (isZero(in2) && !isInfinity(in1)) begin
	 out = tagged Valid infinity(result.sign);
	 exc.divide_0 = True;
      end
      else if (isInfinity(in1)) begin
	 out = tagged Valid infinity(result.sign);
      end
      else if (isZero(in1) || isInfinity(in2)) begin
	 out = tagged Valid zero(result.sign);
      end
      else if (newexp > fromInteger(maxexp(in1)+1)) begin
	 result.exp = maxBound - 1;
	 result.sfd = maxBound;

	 exc.overflow = True;
	 exc.inexact = True;

	 let y = round(rmode, result, '1);
	 out = tagged Valid tpl_1(y);
	 exc = exc | tpl_2(y);
      end
      else if (newexp < (fromInteger(minexp_subnormal(in1))-2)) begin
	 result.exp = 0;
	 result.sfd = 0;

	 exc.underflow = True;
	 exc.inexact = True;

	 let y = round(rmode, result, 'b01);
	 out = tagged Valid tpl_1(y);
	 exc = exc | tpl_2(y);
      end
      else if (newexp < fromInteger(minexp(result))) begin
	 result.exp = 0;
	 shift = cExtend(fromInteger(minexp(result)) - newexp);
      end
      else begin
	 result.exp = cExtend(newexp + fromInteger(bias(result)));
      end

      fState_S1.enq(tuple7(out,exc,rmode,result,opA,opB,shift));
   endrule

   FIFO#(Tuple5#(Maybe#(FloatingPoint#(e,m)),
		 Exception,
		 RoundMode,
		 FloatingPoint#(e,m),
		 Bit#(e))) fState_S2 <- mkLFIFO;

   rule s2_stage;
      match {.out,.exc,.rmode,.result,.opA,.opB,.shift} <- toGet(fState_S1).get;

      if (out matches tagged Invalid) begin
	 //$display("%d / %d", opA, opB);
	 div.request.put(tuple2(unpack(opA),unpack(opB)));
      end

      fState_S2.enq(tuple5(out,exc,rmode,result,shift));
   endrule

   FIFO#(Tuple5#(Maybe#(FloatingPoint#(e,m)),
		 Exception,
		 RoundMode,
		 FloatingPoint#(e,m),
		 Bit#(dbits1))) fState_S3 <- mkLFIFO;

   rule s3_stage_div (fState_S2.first matches {tagged Invalid,.exc,.rmode,.result,.shift});
      fState_S2.deq;

      Bit#(dbits1) rsfd;

	 match {.q,.p} <- div.response.get;

	 if (shift < fromInteger(valueOf(dbits1))) begin
            UInt#(dbits1) qdbits1 = extend(q);
	    Bit#(1) sfdlsb = |(pack(qdbits1 << (fromInteger(valueOf(dbits1)) - shift)));
	    rsfd = cExtend(q >> shift);
	    rsfd[0] = rsfd[0] | sfdlsb;
	 end
	 else begin
	    Bit#(1) sfdlsb = |(pack(q));
	    rsfd = 0;
	    rsfd[0] = sfdlsb;
	 end

	 if (p != 0) begin
	    rsfd[0] = 1;
	 end

	 //$display(" = %d, %d", q, p);

      fState_S3.enq(tuple5(tagged Invalid,exc,rmode,result,rsfd));
   endrule

   rule s3_stage_no_div (fState_S2.first matches {tagged Valid .res,.exc,.rmode,.result,.shift});
      fState_S2.deq;
      fState_S3.enq(tuple5(tagged Valid res,exc,rmode,result,?));
   endrule

   FIFO#(Tuple5#(Maybe#(FloatingPoint#(e,m)),
		 Exception,
		 RoundMode,
		 FloatingPoint#(e,m),
		 Bit#(2))) fState_S4 <- mkLFIFO;

   rule s4_stage;
      match {.out,.exc,.rmode,.result,.rsfd} <- toGet(fState_S3).get;

      Bit#(2) guard = ?;

      if (result.exp == maxBound) begin
	 if (truncateLSB(rsfd) == 2'b00) begin
	    rsfd = rsfd << 1;
	    result.exp = result.exp - 1;
	 end
	 else begin
	    result.exp = maxBound - 1;
	    result.sfd = maxBound;

	    exc.overflow = True;
	    exc.inexact = True;

	    let y = round(rmode, result, '1);
	    out = tagged Valid tpl_1(y);
	    exc = exc | tpl_2(y);
	 end
      end

      if (out matches tagged Invalid) begin
	 // $display("result = ", fshow(result));
	 // $display("rsfd = 'h%x", rsfd);
	 // $display("zeros = %d", countZerosMSB(rsfd));

	 match {.out_, .guard_, .exc_} = normalize(result, rsfd);
	 result = out_;
	 guard = guard_;
	 exc = exc | exc_;

	 // $display("result = ", fshow(result));
	 // $display("guard = 'b%b", guard);
	 // $display("exc = 'b%b", pack(exc));
      end

      fState_S4.enq(tuple5(out,exc,rmode,result,guard));
   endrule

   FIFO#(Tuple2#(FloatingPoint#(e,m),Exception)) fResult_S5  <- mkLFIFO;

   rule s5_stage;
      match {.out,.exc,.rmode,.result,.guard} <- toGet(fState_S4).get;

      if (out matches tagged Valid .x)
	 result = x;
      else begin
	 match {.out_, .exc_} = round(rmode,result,guard);
	 result = out_;
	 exc = exc | exc_;
      end

      fResult_S5.enq(tuple2(result,exc));
   endrule

   interface request = toPut(fOperands_S0);
   interface response = toGet(fResult_S5);
endmodule

// (*synthesize*)
// module mkTb(Empty);
// 	Server#(Tuple2#(UInt#(56),UInt#(28)),Tuple2#(UInt#(28),UInt#(28)))
// 	int_divider <- mkDivider(1);
 
// 	Server#(Tuple3#(FloatingPoint#(8,23), FloatingPoint#(8,23), RoundMode), Tuple2#(FloatingPoint#(8,23),Exception))
// 	fp_divider <- mkFloatingPointDivider(int_divider);

// 	Reg#(Bit#(4)) status <- mkReg(0);
 
// 	rule stages;
// 		status <= status + 1;
// 		$display("cycle = %d", status);
// 		if(status>10)
// 		    $finish(0);
// 	endrule 

// 	rule rl_stage1(status == 1); 
// 	    fp_divider.request.put(tuple3(2, 2, defaultValue));
// 	endrule 
    
// 	rule res (status [0] == 1); 
// 	    let res <- fp_divider.response.get();
// 		$display("cycle %d: result = %b",status,res);
// 	endrule 
// endmodule 

(* synthesize *)
module mkTb(Empty);

	Server#(Tuple2#(UInt#(56),UInt#(28)),Tuple2#(UInt#(28),UInt#(28)))
	int_divider <- mkDivider(1);
 
	Server#(Tuple3#(FloatingPoint#(8,23), FloatingPoint#(8,23), RoundMode), Tuple2#(FloatingPoint#(8,23),Exception))
	fp_divider <- mkFloatingPointDivider(int_divider);
 
	Reg #(int) rg_state <- mkReg (0);

   rule stages;
      rg_state <= rg_state + 1;
   endrule

   rule rl_div (rg_state == 0);
      fp_divider.request.put(tuple3(3, 2, defaultValue));
      $display($time, "Hi");
   endrule

   // rule rl_div_0_by_1 (rg_state == 2);
   //    fp_divider.request.put(tuple3(3, 4, defaultValue));

   //    rg_state <= 3;
   // endrule

   rule rl_retire; //(rg_state [0] == 1);
      let r <- fp_divider.response.get();
      $display($time, " result: %b cycle: %d", r, rg_state);
   endrule

   rule rl_finish (rg_state == 100);
      $display ($time, " Finished");
      $finish (0);
   endrule
 endmodule
 
	    

