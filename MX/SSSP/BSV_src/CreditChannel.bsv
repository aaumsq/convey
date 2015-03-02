
package CreditChannel;

import Vector           :: *;
import FIFOF            :: *;
import SpecialFIFOs     :: *;
import GetPut           :: *;
import ClientServer     :: *;
import Connectable      :: *;
import StmtFSM          :: *;
import FShow            :: *;
import BRAMFIFO         :: *;

// ----------------
// BC library imports

import BC_Utils           :: *;
import BC_HW_IFC          :: *;
import BC_Transactors     :: *;

import GaloisTypes::*;
`include "GaloisDefs.bsv"


interface Channel#(type req_t, type resp_t);
    interface Put#(req_t) reqToChan;
    interface Get#(req_t) reqFromChan;
    interface Get#(resp_t) respFromChan;
    interface Put#(resp_t) respToChan;
    method Action init();
endinterface

module mkCreditChannel#(Integer count) (Channel#(req_t, resp_t))
    provisos (Bits#(req_t, req_size), Bits#(resp_t, resp_size));
    Reg#(Bit#(10)) credits <- mkRegU;
    Reg#(Bool) started <- mkReg(False);
    
    FIFOF#(req_t) reqQ <- mkSizedFIFOF(2);
    FIFOF#(resp_t) respQ <- mkSizedFIFOF(count);
    
    PulseWire inc <- mkPulseWire();
    PulseWire dec <- mkPulseWire();
    
    rule setCredits(started);
        if(inc && !dec) begin
            credits <= credits + 1;
        end
        else if(!inc && dec) begin
            credits <= credits - 1;
        end
    endrule
    
    
    method Action init();
        credits <= fromInteger(count);
        started <= True;
    endmethod
    
    interface Put reqToChan;
        method Action put(pkt) if(started && (credits > 0));
            reqQ.enq(pkt);
            dec.send();
        endmethod
    endinterface
    
    interface Get reqFromChan;
        method ActionValue#(req_t) get();
            reqQ.deq();
            return reqQ.first();
        endmethod
    endinterface
    
    interface Put respToChan;
        method Action put(pkt) if(started);
            respQ.enq(pkt);
        endmethod
    endinterface
    
    interface Get respFromChan;
        method ActionValue#(resp_t) get();
            respQ.deq();
            inc.send();
            return respQ.first();
        endmethod
    endinterface
endmodule

endpackage