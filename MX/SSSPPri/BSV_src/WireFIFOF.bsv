
package WireFIFOF;

import FIFOF::*;

/*
interface WireFIFOF#(type fifo_type);
    method Action enq(fifo_type x);
    method Action deq();
    method fifo_type first();
    method Bool notFull();
    method Bool notEmpty();
    method Action clear();
endinterface
*/
module mkSizedWireFIFOF#(Integer count) (FIFOF#(fifo_type))
    provisos (Bits#(fifo_type, fifo_size), Add#(1, a__, fifo_size));
    
    FIFOF#(fifo_type) fifo <- mkSizedFIFOF(count);
    Wire#(Bool) notFullWire <- mkBypassWire();
    Wire#(Bool) notEmptyWire <- mkBypassWire();
    
    rule setFullEmptyWires;
        notFullWire <= fifo.notFull();
        notEmptyWire <= fifo.notEmpty();
    endrule
    
    method Action enq(fifo_type x) = fifo.enq(x);
    method Action deq() = fifo.deq();
    method fifo_type first() = fifo.first();
    method Bool notFull() = notFullWire;
    method Bool notEmpty() = notEmptyWire;
    method Action clear() = fifo.clear();
endmodule

endpackage
