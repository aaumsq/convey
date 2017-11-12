
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


interface CreditChannel#(type req_t, type resp_t);
    interface Put#(req_t) reqToChan;
    interface Get#(req_t) reqFromChan;
    interface Get#(resp_t) respFromChan;
    interface Put#(resp_t) respToChan;
    method Action init(Bit#(4) laneId, Bit#(4) chanId);
endinterface

module mkCreditChannel#(Integer count) (CreditChannel#(req_t, resp_t))
    provisos (Bits#(req_t, req_size), Bits#(resp_t, resp_size));
    Reg#(Bit#(10)) credits <- mkRegU;
    Reg#(Bool) started <- mkReg(False);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(Bit#(4)) laneId <- mkRegU;
    Reg#(Bit#(4)) chanId <- mkRegU;
    
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
    
    rule resp(respQ.notEmpty);
        $display("CreditChannel[%0d][%0d][%0d] respQ not empty", fpgaId, laneId, chanId);
    endrule
    
    method Action init(BC_AEId fpgaid, Bit#(4) laneid, Bit#(4) chanid);
        fpgaId <= fpgaid;
        laneId <= laneid;
        chanId <= chanid;
        credits <= fromInteger(count);
        started <= True;
    endmethod
    
    interface Put reqToChan;
        method Action put(pkt) if(started && (credits > 0));
            reqQ.enq(pkt);
            dec.send();
            $display("CreditChannel[%0d][%0d][%0d] enq succeeded, credits left: %0d/%0d", fpgaId, laneId, chanId, credits, count);
        endmethod
    endinterface
    
    interface Get reqFromChan;
        method ActionValue#(req_t) get();
            reqQ.deq();
            $display("CreditChannel[%0d][%0d][%0d] req leaving reqQ", fpgaId, laneId, chanId);
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
            $display("CreditChannel resp");
            respQ.deq();
            inc.send();
            return respQ.first();
        endmethod
    endinterface
endmodule

endpackage