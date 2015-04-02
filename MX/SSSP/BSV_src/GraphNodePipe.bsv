package GraphNodePipe;

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

typedef struct {
   NodeID nodeID;
} ReadNodePipe deriving(Bits, Eq);


interface GraphNodeIfc;
    interface Put#(GraphNodeReq) req;
    interface Get#(GraphNodeResp) resp;
    interface Get#(MemReq) memReq;
    interface Vector#(2, Put#(MemResp)) memResps;
    
    method Action init(BC_AEId fpgaId, Bit#(4) laneId, BC_Addr nodePtr);
endinterface

(* descending_urgency = "readNode3, readNode2, readNode" *)
module mkGraphNodePipe#(Integer lane0, Integer lane1)(GraphNodeIfc);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(Bit#(4)) laneId <- mkRegU;
    Reg#(BC_Addr) nodePtr <- mkRegU;

    // Read Node Pipe
    FIFOF#(GraphNodeReq) reqQ <- mkFIFOF;
    FIFOF#(ReadNodePipe) readNodeQ <- mkSizedFIFOF(`GRAPH_NUM_IN_FLIGHT);
    FIFOF#(ReadNodePipe) readNodeQ2 <- mkSizedFIFOF(`GRAPH_NUM_IN_FLIGHT);
    FIFOF#(Bit#(64)) readNodeQ3_partialNode <- mkSizedFIFOF(`GRAPH_NUM_IN_FLIGHT);
    FIFOF#(GraphNodeResp) respQ <- mkFIFOF;
    FIFOF#(MemReq) memReqQ <- mkFIFOF;
    Vector#(2, FIFOF#(MemResp)) memRespQs <- replicateM(mkSizedFIFOF(`GRAPH_NUM_IN_FLIGHT));

    rule readNode;
        GraphNodeReq nodeReq = reqQ.first();
        reqQ.deq();
        
        readNodeQ.enq(ReadNodePipe{nodeID: nodeReq.id});
        
        GaloisAddress gaddr = GaloisAddress{mod: MK_GRAPH, addr: fromInteger(lane0)};
        Bit#(48) vaddr = nodePtr + (extend(nodeReq.id) << `LG_GRAPH_NODE_SIZE); // base + offset*16 (16B nodes)
        memReqQ.enq(tagged MemRead64{addr: vaddr, gaddr: gaddr});
    endrule
    
    rule readNode2;
        ReadNodePipe cxt = readNodeQ.first();
        readNodeQ.deq();
        
        readNodeQ2.enq(cxt);

        MemResp rsp = memRespQs[0].first();
        memRespQs[0].deq();
        readNodeQ3_partialNode.enq(rsp.data);
        
        Tuple2#(Bit#(32), Bit#(32)) tmp = unpack(rsp.data);
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] receive readNode resp #1, data = %0x (%0d %0d)", cur_cycle, fpgaId, laneId, rsp.data);
        GaloisAddress gaddr = GaloisAddress{mod: MK_GRAPH, addr: fromInteger(lane1)};
        Bit#(48) vaddr = nodePtr + (extend(cxt.nodeID) << `LG_GRAPH_NODE_SIZE) + 8;
        memReqQ.enq(tagged MemRead64{addr: vaddr, gaddr: gaddr});
    endrule
    
    rule readNode3;
        ReadNodePipe cxt = readNodeQ2.first();
        readNodeQ2.deq();
        
        Bit#(64) partialNode = readNodeQ3_partialNode.first();
        readNodeQ3_partialNode.deq();
        
        MemResp rsp = memRespQs[1].first();
        memRespQs[1].deq();
        GraphNode node = unpack({rsp.data, partialNode});
        
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] receive readNode resp #2 packed node: %x", cur_cycle, fpgaId, laneId, node);
        respQ.enq(GraphNodeResp{node: node});
    endrule

    method Action init(BC_AEId fid, Bit#(4) lid, BC_Addr ptr);
        fpgaId <= fid;
        laneId <= lid;
        nodePtr <= ptr;
    endmethod
    
    interface req = toPut(reqQ);
    interface resp = toGet(respQ);
    interface memReq = toGet(memReqQ);
    interface memResps = map(toPut, memRespQs);
endmodule

endpackage