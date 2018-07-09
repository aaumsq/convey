package BufBRAMFIFOF;

import FIFOF::*;
import BRAMFIFO::*;

module mkSizedBufBRAMFIFOF#(Integer count) (FIFOF#(fifo_type))
  provisos // Don't know why Add is necessary, wtf?
          (Bits#(fifo_type, fifo_size), Add#(1, a__, fifo_size));
    
    FIFOF#(fifo_type) fifo <- mkSizedBRAMFIFOF(count);
    FIFOF#(fifo_type) enqQ <- mkFIFOF();
    FIFOF#(fifo_type) deqQ <- mkFIFOF();
    
    rule enqToFIFO;
        fifo_type pkt = enqQ.first();
        enqQ.deq();
        fifo.enq(pkt);
    endrule
    
    rule fifoToDeq;
        fifo_type pkt = fifo.first();
        fifo.deq();
        deqQ.enq(pkt);
    endrule
    
    method Action enq(fifo_type data);
        enqQ.enq(data);
    endmethod
    
    method Action deq();
        deqQ.deq();
    endmethod
    
    method fifo_type first();
        return deqQ.first();
    endmethod
    
    method Bool notFull();
        return enqQ.notFull();
    endmethod
    
    method Bool notEmpty();
        return deqQ.notEmpty();
    endmethod
    
    method Action clear();
        fifo.clear();
    endmethod
    
endmodule

endpackage