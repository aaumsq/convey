package GraphPartialNodePipe;

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

import BufBRAMFIFOF::*;
import BC_Utils           :: *;
import BC_HW_IFC          :: *;
import BC_Transactors     :: *;

import GaloisTypes::*;
`include "GaloisDefs.bsv"

typedef struct {
   NodeID nodeID;
} ReadNodePipe deriving(Bits, Eq);


interface GraphPartialNodeIfc;
    interface Put#(GraphPartialNodeReq) req;
    interface Get#(GraphPartialNodeResp) resp;
    interface Get#(MemReq) memReq;
    interface Put#(MemResp) memResp;
    
    method Action init(BC_AEId fpgaId, Bit#(4) laneId, BC_Addr nodePtr);
endinterface

(* descending_urgency = "graphNode2, graphNode" *)
module mkGraphPartialNodePipe#(Integer lane0, Integer offset)(GraphPartialNodeIfc);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(Bit#(4)) laneId <- mkRegU;
    Reg#(BC_Addr) nodePtr <- mkRegU;

    // Graph Node Pipe
    FIFOF#(GraphPartialNodeReq) reqQ <- mkFIFOF;
    FIFOF#(ReadNodePipe) readNodeQ <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT);
    FIFOF#(GraphPartialNodeResp) respQ <- mkFIFOF;
    FIFOF#(MemReq) memReqQ <- mkFIFOF;
    FIFOF#(MemResp) memRespQ <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT);
    
    rule print;
        let cycle <- cur_cycle;
        //if(cycle % 8192 == 0)
        //    $display("%0d: graphNodePipe[%0d][%0d][%0d]: readNodeFull: %0d, readNode2Full: %0d, readNode3Full: %0d, respFull: %0d, memReq0Full: %0d, memReq1Full: %0d", cycle, fpgaId, laneId, lane0, readNodeFull, readNode2Full, readNode3Full, respFull, memReq0Full, memReq1Full);
    endrule
    
    rule graphNode;
        GraphPartialNodeReq nodeReq = reqQ.first();
        reqQ.deq();
        
        readNodeQ.enq(ReadNodePipe{nodeID: nodeReq.id});
        
        GaloisAddress gaddr = GaloisAddress{mod: MK_GRAPH, addr: fromInteger(lane0)};
        Bit#(48) vaddr = nodePtr + (extend(nodeReq.id) << `LG_GRAPH_NODE_SIZE) + fromInteger(offset); // base + offset*16 (16B nodes)
	if (nodeReq.op == 0)
            memReqQ.enq(tagged MemRead64{addr: vaddr, gaddr: gaddr});
	else
            memReqQ.enq(tagged MemWrite64{addr: vaddr, gaddr: gaddr, data: nodeReq.data});
        if (`DEBUG) $display("%0d: GraphEngine[%0d][%0d] #1 request for Node %0d, addr = %0x", cur_cycle, fpgaId, laneId, nodeReq.id, vaddr);
    endrule
    
    rule graphNode2;
        ReadNodePipe cxt = readNodeQ.first();
        readNodeQ.deq();
        
        MemResp rsp = memRespQ.first();
        memRespQ.deq();

        GraphPartialNodeResp resp = unpack(rsp.data);
        
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] receive readNode resp #2 packed node: %x", cur_cycle, fpgaId, laneId, resp);
        respQ.enq(resp);
    endrule

    method Action init(BC_AEId fid, Bit#(4) lid, BC_Addr ptr);
        fpgaId <= fid;
        laneId <= lid;
        nodePtr <= ptr;
    endmethod

    interface req = toPut(reqQ);
    interface resp = toGet(respQ);
    interface memReq = toGet(memReqQ);
    interface memResp = toPut(memRespQ);
endmodule

endpackage
