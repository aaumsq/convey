package GraphCASPipe;

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


typedef struct {
   NodePayload cmpVal;
} CASPipe deriving(Bits, Eq);

interface GraphCASIfc;
    interface Put#(GraphCASReq) req;
    interface Get#(GraphCASResp) resp;
    interface Get#(MemReq) memReq;
    interface Put#(MemResp) memResp;
    
    method Action init(BC_AEId fpgaId, Bit#(4) laneId, BC_Addr nodePtr);
    method Bool reqNotEmpty();
endinterface


(* descending_urgency = "cas2, cas" *)
module mkGraphCASPipe#(Integer lane0)(GraphCASIfc);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(Bit#(4)) laneId <- mkRegU;
    Reg#(BC_Addr) nodePtr <- mkRegU;
    
    // Graph Payload CAS pipeline
    FIFOF#(GraphCASReq) reqQ <- mkFIFOF;
    FIFOF#(CASPipe) casQ <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT);
    FIFOF#(GraphCASResp) respQ <- mkFIFOF;
    FIFOF#(MemReq) memReqQ <- mkFIFOF;
    FIFOF#(MemResp) memRespQ <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT);
    
    rule cas;
        GraphCASReq casReq = reqQ.first();
        reqQ.deq();
        
        casQ.enq(CASPipe{cmpVal: casReq.cmpVal});
        
        GaloisAddress gaddr = GaloisAddress{mod: MK_GRAPH, addr: fromInteger(lane0)};
        Bit#(48) vaddrBase = nodePtr + (extend(casReq.id) << `LG_GRAPH_NODE_SIZE);
        
        // Payload is the 3rd 32-bit (4B) entry in struct
        Bit#(48) vaddr = vaddrBase + (2 * 4);
        
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] CAS FSM start! enq mem req @ vaddr: %0x, cas_idx: %0d, cmpVal: %0d, swapVal: %0d", cur_cycle, fpgaId, laneId, vaddr, casReq.id, casReq.cmpVal, casReq.swapVal);
        memReqQ.enq(tagged MemCAS32{addr: vaddr, gaddr: gaddr, cmpVal: casReq.cmpVal, swapVal: casReq.swapVal});
    endrule
    
    rule cas2;
        CASPipe cxt = casQ.first();
        casQ.deq();
        
        MemResp rsp = memRespQ.first();
        memRespQ.deq();
        
        // rsp.data is old data previous to swap, or the new data if cmp failed
        Bit#(32) curData = truncate(rsp.data);
        Bool success = (curData == pack(cxt.cmpVal));
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] CAS complete, success: %d, oldVal: %0d", cur_cycle, fpgaId, laneId, success, curData);
        respQ.enq(GraphCASResp{success: success, oldVal: curData});
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
