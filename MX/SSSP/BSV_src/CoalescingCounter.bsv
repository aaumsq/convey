
package CoalescingCounter;

import Vector::*;

interface CoalescingCounter;
    method Action init(Bit#(10) val);
    method Action inc();
    method Action dec();
    method Action dec2();
    method Bit#(10) getVal();
    method Bool notZero();
    method Bool notMax();
endinterface

(* synthesize *)
module mkCCounter(CoalescingCounter);
    
    Reg#(Bit#(10)) val <- mkRegU;
    Reg#(Bit#(10)) maxVal <- mkRegU;
    
    PulseWire increment <- mkPulseWire();
    PulseWire decrement <- mkPulseWire();
    PulseWire decrement2 <- mkPulseWire();
    
    rule setValue;
        Vector#(2, Bool) decs;
        decs[0] = decrement;
        decs[1] = decrement2;
        
        let numDecs = countElem(True, decs);
        if(increment && (numDecs == 0)) begin
            if(val < maxVal)
                val <= val + 1;
        end
        else if(increment && (numDecs == 1)) begin end
        else if(increment && (numDecs == 2)) begin
            if(val > 0)
                val <= val - 1;
        end
        else if(!increment && (numDecs == 0)) begin end
        else if(!increment && (numDecs == 1)) begin
            if(val > 0)
                val <= val - 1;
        end
        else if(!increment && (numDecs == 2)) begin
            if(val > 0)
                val <= val - 2;
        end
    endrule
    
    method Action init(Bit#(10) max);
        val <= 0;
        maxVal <= max;
    endmethod
    
    method Action inc() = increment.send();
    method Action dec() = decrement.send();
    method Action dec2() = decrement2.send();
    
    method Bit#(10) getVal() = val;
    method Bool notZero() = (val != 0);
    method Bool notMax() = (val != maxVal);
endmodule


endpackage
