
package CreditFIFOF;

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


interface CreditFIFOF#(type req_t, type resp_t);
    interface FIFOF#(req_t) req;
    interface FIFOF#(resp_t) resp;
    method Action init(BC_AEId fpgaId, Bit#(4) laneId, Bit#(4) chanId);
endinterface

module mkCreditFIFOF#(Integer count) (CreditFIFOF#(req_t, resp_t))
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
    
    rule respDisp(respQ.notEmpty);
        if(`DEBUG) $display("CreditFIFOF[%0d][%0d][%0d] respQ not empty", fpgaId, laneId, chanId);
    endrule
    
    method Action init(BC_AEId fpgaid, Bit#(4) laneid, Bit#(4) chanid);
        fpgaId <= fpgaid;
        laneId <= laneid;
        chanId <= chanid;
        credits <= fromInteger(count);
        started <= True;
    endmethod
    
    interface FIFOF req;
        method Action enq(pkt) if(started && (credits > 0));
            reqQ.enq(pkt);
            dec.send();
            if(`DEBUG) $display("CreditFIFOF[%0d][%0d][%0d] enq succeeded, credits left: %0d/%0d", fpgaId, laneId, chanId, credits, count);
        endmethod
        method Action deq() if(started);
            reqQ.deq();
        endmethod
        method req_t first();
            return reqQ.first();
        endmethod
        method Action clear() if(started);
            reqQ.clear();
        endmethod
        method Bool notFull();
            return reqQ.notFull();
        endmethod
        method Bool notEmpty();
            return reqQ.notEmpty();
        endmethod
    endinterface
    
    interface FIFOF resp;
        method Action enq(pkt) if(started);
            respQ.enq(pkt);
        endmethod
        method Action deq() if(started);
            respQ.deq();
            inc.send();
        endmethod
        method resp_t first() = respQ.first();
        method Action clear() if(started);
            respQ.clear();
        endmethod
        method Bool notFull() = respQ.notFull();
        method Bool notEmpty() = respQ.notEmpty();
    endinterface
        
endmodule

endpackage