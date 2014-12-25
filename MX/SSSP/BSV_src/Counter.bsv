
package Counter;


interface Counter;
    method Action reset();
    method Action setMaxVal(Bit#(32) val);
    method Action inc();
    method Action dec();
    method ActionValue#(Bit#(32)) getVal();
endinterface

module mkCounter(Counter);
    
    Reg#(Bit#(32)) val <- mkReg(0);
    Reg#(Bit#(32)) maxVal <- mkReg(16);
    
    PulseWire increment <- mkPulseWire();
    PulseWire decrement <- mkPulseWire();
    
    rule setValue;
        if(increment && !decrement) begin
            val <= val + 1;
        end
        else if(!increment && decrement) begin
            val <= val - 1;
        end
        else begin
            val <= val;
        end
    endrule
    
    method Action reset();
        val <= 0;
    endmethod
    
    method Action setMaxVal(Bit#(32) max);
        maxVal <= max;
    endmethod
    
    method Action inc();
        increment.send();
    endmethod
    
    method Action dec();
        decrement.send();
    endmethod
    
    method ActionValue#(Bit#(32)) getVal();
        return val;
    endmethod
endmodule


endpackage
