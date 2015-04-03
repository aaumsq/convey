package GraphEdgePipe;

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

import GaloisTypes::*;
`include "GaloisDefs.bsv"

interface GraphEdgeIfc;
    interface Put#(GraphEdgeReq) req;
    interface Get#(GraphEdgeResp) resp;
    interface Get#(MemReq) memReq;
    interface Put#(MemResp) memResp;
    
    method Action init(BC_AEId fpgaId, Bit#(4) laneId, BC_Addr edgePtr);
endinterface

typedef struct {
   EdgePtr gedge;
} ReadEdgePipe deriving(Bits, Eq);

(* descending_urgency = "readEdge2, readEdge" *)
module mkGraphEdgePipe#(Integer lane0)(GraphEdgeIfc);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(Bit#(4)) laneId <- mkRegU;
    Reg#(BC_Addr) edgePtr <- mkRegU;
    
    // Read Edge Pipeline
    FIFOF#(GraphEdgeReq) reqQ <- mkFIFOF;
    FIFOF#(ReadEdgePipe) readEdgeQ <- mkSizedFIFOF(`GRAPH_NUM_IN_FLIGHT);
    FIFOF#(GraphEdgeResp) respQ <- mkFIFOF;
    
    FIFOF#(MemReq) memReqQ <- mkFIFOF;
    FIFOF#(MemResp) memRespQ <- mkSizedFIFOF(`GRAPH_NUM_IN_FLIGHT);
    
    /*
    rule print;
        let cycle <- cur_cycle;
        if(cycle > 60000) $display("%0d: GraphEdgePipe[%0d][%0d] fulls: reqQ:%b readEdgeQ:%b respQ:%b memReqQ:%b memRespQ:%b", cur_cycle, fpgaId, laneId, 
           !reqQ.notFull, !readEdgeQ.notFull, !respQ.notFull, !memReqQ.notFull, !memRespQ.notFull);
        if(cycle > 60000) $display("%0d: GraphEdgePipe[%0d][%0d] empty: reqQ:%b readEdgeQ:%b respQ:%b memReqQ:%b memRespQ:%b", cur_cycle, fpgaId, laneId, 
           !reqQ.notEmpty, !readEdgeQ.notEmpty, !respQ.notEmpty, !memReqQ.notEmpty, !memRespQ.notEmpty);
    endrule
    */
    rule readEdge;
        GraphEdgeReq edgeReq = reqQ.first();
        reqQ.deq();
        
        readEdgeQ.enq(ReadEdgePipe{gedge: edgeReq.id});
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] readEdge1 edgeId=%0d", cur_cycle, fpgaId, laneId, edgeReq.id); 
       
        GaloisAddress gaddr = GaloisAddress{mod: MK_GRAPH, addr: fromInteger(lane0)};
        Bit#(48) vaddr = edgePtr + (extend(edgeReq.id) << `LG_GRAPH_EDGE_SIZE);
        memReqQ.enq(tagged MemRead64{addr: vaddr, gaddr: gaddr});
    endrule
    
    rule readEdge2;
        ReadEdgePipe cxt = readEdgeQ.first();
        readEdgeQ.deq();
        
        MemResp rsp = memRespQ.first();
        memRespQ.deq();
        GraphEdge gedge = unpack(rsp.data);
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] readEdge2 done! edgeDest=%0d, edgeWeight=%0d", cur_cycle, fpgaId, laneId, gedge.dest, gedge.weight);
        
        respQ.enq(GraphEdgeResp{gedge: gedge});
    endrule
    
    method Action init(BC_AEId fid, Bit#(4) lid, BC_Addr ptr);
        fpgaId <= fid;
        laneId <= lid;
        edgePtr <= ptr;
    endmethod
    
    interface req = toPut(reqQ);
    interface resp = toGet(respQ);
    interface memReq = toGet(memReqQ);
    interface memResp = toPut(memRespQ);
endmodule

endpackage
