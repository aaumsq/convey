package GraphEXCHPipe;

import Vector           :: *;
import FIFOF            :: *;
import GetPut           :: *;
import ClientServer     :: *;
import Connectable      :: *;
import StmtFSM          :: *;
import FShow            :: *;
import BRAMFIFO         :: *;

import LFSR::*;

// ----------------
// BC library imports

import BC_Utils           :: *;
import BC_HW_IFC          :: *;
import BC_Transactors     :: *;

import BufBRAMFIFOF::*;
import GaloisTypes::*;
`include "GaloisDefs.bsv"


interface GraphEXCHIfc;
    interface Put#(GraphEXCHReq) req;
    interface Get#(GraphEXCHResp) resp;
    interface Get#(MemReq) memReq;
    interface Put#(MemResp) memResp;
    
    method Action init(BC_AEId fpgaId, Bit#(4) laneId, BC_Addr nodePtr);
    method Bool reqNotEmpty();
endinterface


(* descending_urgency = "exch2, exch" *)
module mkGraphEXCHPipe#(Integer lane0)(GraphEXCHIfc);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(Bit#(4)) laneId <- mkRegU;
    Reg#(BC_Addr) nodePtr <- mkRegU;
    
    // Graph Payload EXCH pipeline
    FIFOF#(GraphEXCHReq) reqQ <- mkFIFOF;
    FIFOF#(GraphEXCHResp) respQ <- mkFIFOF;
    FIFOF#(MemReq) memReqQ <- mkFIFOF;
    FIFOF#(MemResp) memRespQ <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT);
    
    rule exch;
        GraphEXCHReq exchReq = reqQ.first();
        reqQ.deq();
        
        GaloisAddress gaddr = GaloisAddress{mod: MK_GRAPH, addr: fromInteger(lane0)};
        Bit#(48) vaddrBase = nodePtr + (extend(exchReq.id) << `LG_GRAPH_NODE_SIZE);
        
        // residual is the 5th 32-bit (4B) entry in struct
        Bit#(48) vaddr = vaddrBase + (4 * 4);
        
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] EXCH FSM start! enq mem req @ vaddr: %0x, exch_idx: %0d, swapVal: %0d", cur_cycle, fpgaId, laneId, vaddr, exchReq.id, exchReq.swapVal);
        memReqQ.enq(tagged MemEXCH32{addr: vaddr, gaddr: gaddr, swapVal: exchReq.swapVal});
    endrule
    
    rule exch2;
        MemResp rsp = memRespQ.first();
        memRespQ.deq();
        
        // rsp.data is old data previous to swap, or the new data if cmp failed
        Bit#(32) curData = truncate(rsp.data);
        respQ.enq(GraphEXCHResp{oldVal: curData});
    endrule
    
    method Action init(BC_AEId fid, Bit#(4) lid, BC_Addr ptr);
        fpgaId <= fid;
        laneId <= lid;
        nodePtr <= ptr;
    endmethod

    method Bool reqNotEmpty();
        return memReqQ.notEmpty;
    endmethod
    
    interface req = toPut(reqQ);
    interface resp = toGet(respQ);
    interface memReq = toGet(memReqQ);
    interface memResp = toPut(memRespQ);
endmodule

endpackage
