
package SSSPEngine;

import Vector           :: *;
import FIFOF            :: *;
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

interface Engine;
    method Action init(BC_AEId fpgaId);
    method ActionValue#(Bit#(64)) result;
    
    interface Put#(WLEntry) workIn;
    interface Get#(WLEntry) workOut;

    interface Get#(GraphReq) graphReq;
    interface Put#(GraphResp) graphResp;
endinterface


module mkSSSPEngine(Engine);
    Reg#(BC_AEId) fpgaId <-mkRegU;
    Reg#(Bool) started <- mkReg(False);
    Reg#(Bool) done <- mkReg(False);
    
    FIFOF#(WLEntry) workInQ <- mkFIFOF;
    FIFOF#(WLEntry) workOutQ <- mkFIFOF;
    
    FIFOF#(GraphReq) graphReqQ <- mkFIFOF;
    FIFOF#(GraphResp) graphRespQ <- mkFIFOF;
    
    rule getSrcNode(started);
        WLEntry pkt = workInQ.first();
        workInQ.deq();
        
        
    endrule
    
    
    method Action init(BC_AEId fpgaid);
        fpgaId <= fpgaid;
        started <= True;
        done <= False;
    endmethod
    
    method ActionValue#(Bit#(64)) result() if(done);
        return 64'hDEAD_BEEF;
    endmethod
    
    interface workIn = toPut(workInQ);
    interface workOut = toGet(workOutQ);
    interface graphReq = toGet(graphReqQ);
    interface graphResp = toPut(graphRespQ);
endmodule

endpackage
