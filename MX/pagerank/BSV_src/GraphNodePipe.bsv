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

import BufBRAMFIFOF::*;
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
    interface Vector#(3, Get#(MemReq)) memReqs;
    interface Vector#(3, Put#(MemResp)) memResps;
    
    method Action init(BC_AEId fpgaId, Bit#(4) laneId, BC_Addr nodePtr);
    method Bool reqNotEmpty0();
    method Bool reqNotEmpty1();
    method Bool reqNotEmpty2();
endinterface

(* descending_urgency = "readNode4, readNode3, readNode2, readNode" *)
module mkGraphNodePipe#(Integer lane0, Integer lane1, Integer lane2)(GraphNodeIfc);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(Bit#(4)) laneId <- mkRegU;
    Reg#(BC_Addr) nodePtr <- mkRegU;

    // Read Node Pipe
    FIFOF#(GraphNodeReq) reqQ <- mkFIFOF;
    FIFOF#(ReadNodePipe) readNodeQ <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT);
    FIFOF#(ReadNodePipe) readNodeQ2 <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT);
    FIFOF#(ReadNodePipe) readNodeQ3 <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT);
    FIFOF#(Bit#(64)) readNodeQ3_partialNode <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT);
    FIFOF#(Bit#(128)) readNodeQ4_partialNode <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT);
    FIFOF#(GraphNodeResp) respQ <- mkFIFOF;
    Vector#(3, FIFOF#(MemReq)) memReqQs <- replicateM(mkFIFOF);
    Vector#(3, FIFOF#(MemResp)) memRespQs <- replicateM(mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT));
    //Vector#(2, FIFOF#(MemResp)) memRespQs <- replicateM(mkFIFOF);
    Reg#(Bit#(48)) readNodeFull <- mkRegU;
    Reg#(Bit#(48)) readNode2Full <- mkRegU;
    Reg#(Bit#(48)) readNode3Full <- mkRegU;
    Reg#(Bit#(48)) respFull <- mkRegU;
    Reg#(Bit#(48)) memReq0Full <- mkRegU;
    Reg#(Bit#(48)) memReq1Full <- mkRegU;
    
    rule setCount;
        if(!readNodeQ.notFull)
            readNodeFull <= readNodeFull + 1;
        if(!readNodeQ2.notFull)
            readNode2Full <= readNode2Full + 1;
        if(!readNodeQ3_partialNode.notFull)
            readNode3Full <= readNode3Full + 1;
        if(!respQ.notFull)
            respFull <= respFull + 1;
        if(!memReqQs[0].notFull)
            memReq0Full <= memReq0Full + 1;
        if(!memReqQs[1].notFull)
            memReq1Full <= memReq1Full + 1;
    endrule
    
    rule print;
        let cycle <- cur_cycle;
        //if(cycle % 8192 == 0)
        //    $display("%0d: graphNodePipe[%0d][%0d][%0d]: readNodeFull: %0d, readNode2Full: %0d, readNode3Full: %0d, respFull: %0d, memReq0Full: %0d, memReq1Full: %0d", cycle, fpgaId, laneId, lane0, readNodeFull, readNode2Full, readNode3Full, respFull, memReq0Full, memReq1Full);
    endrule
    
    rule readNode;
        GraphNodeReq nodeReq = reqQ.first();
        reqQ.deq();
        
        readNodeQ.enq(ReadNodePipe{nodeID: nodeReq.id});
        
        GaloisAddress gaddr = GaloisAddress{mod: MK_GRAPH, addr: fromInteger(lane0)};
        Bit#(48) vaddr = nodePtr + (extend(nodeReq.id) << `LG_GRAPH_NODE_SIZE); // base + offset*32 (32B nodes)
        memReqQs[0].enq(tagged MemRead64{addr: vaddr, gaddr: gaddr});
        if (`DEBUG) $display("%0d: GraphEngine[%0d][%0d] #1 request for Node %0d, addr = %0x", cur_cycle, fpgaId, laneId, nodeReq.id, vaddr);
    endrule
    
    rule readNode2;
        ReadNodePipe cxt = readNodeQ.first();
        readNodeQ.deq();
        
        readNodeQ2.enq(cxt);

        MemResp rsp = memRespQs[0].first();
        memRespQs[0].deq();
        readNodeQ3_partialNode.enq(rsp.data);
        
        Tuple2#(Bit#(32), Bit#(32)) tmp = unpack(rsp.data);
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] receive readNode resp #1, data = %0x", cur_cycle, fpgaId, laneId, rsp.data);
        GaloisAddress gaddr = GaloisAddress{mod: MK_GRAPH, addr: fromInteger(lane1)};
        Bit#(48) vaddr = nodePtr + (extend(cxt.nodeID) << `LG_GRAPH_NODE_SIZE) + 8;
        memReqQs[1].enq(tagged MemRead64{addr: vaddr, gaddr: gaddr});
        if (`DEBUG) $display("%0d: GraphEngine[%0d][%0d] #2 request for Node %0d, addr = %0x", cur_cycle, fpgaId, laneId, cxt.nodeID, vaddr);
    endrule
    
    rule readNode3;
        ReadNodePipe cxt = readNodeQ2.first();
        readNodeQ2.deq();
        
        readNodeQ3.enq(cxt);

        MemResp rsp = memRespQs[1].first();
        memRespQs[1].deq();
	Bit#(64) partialNode_q3 = readNodeQ3_partialNode.first();
	readNodeQ3_partialNode.deq();

        Bit#(128) partialNode_q4 = unpack({rsp.data, partialNode_q3});
        readNodeQ4_partialNode.enq(partialNode_q4);
        
        Tuple2#(Bit#(32), Bit#(32)) tmp = unpack(rsp.data);
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] receive readNode resp #1, data = %0x", cur_cycle, fpgaId, laneId, rsp.data);
        GaloisAddress gaddr = GaloisAddress{mod: MK_GRAPH, addr: fromInteger(lane2)};
        Bit#(48) vaddr = nodePtr + (extend(cxt.nodeID) << `LG_GRAPH_NODE_SIZE) + 16;
        memReqQs[2].enq(tagged MemRead64{addr: vaddr, gaddr: gaddr});
        if (`DEBUG) $display("%0d: GraphEngine[%0d][%0d] #2 request for Node %0d, addr = %0x", cur_cycle, fpgaId, laneId, cxt.nodeID, vaddr);
    endrule

    rule readNode4;
        ReadNodePipe cxt = readNodeQ3.first();
        readNodeQ3.deq();
        
        Bit#(128) partialNode = readNodeQ4_partialNode.first();
        readNodeQ4_partialNode.deq();
        
        MemResp rsp = memRespQs[2].first();
        memRespQs[2].deq();
        GraphNode node = unpack({rsp.data, partialNode});
        
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] receive readNode resp #2 packed node: %x", cur_cycle, fpgaId, laneId, node);
        respQ.enq(GraphNodeResp{node: node});
    endrule

    method Action init(BC_AEId fid, Bit#(4) lid, BC_Addr ptr);
        fpgaId <= fid;
        laneId <= lid;
        nodePtr <= ptr;
        
        readNodeFull <= 0;
        readNode2Full <= 0;
        readNode3Full <= 0;
        respFull <= 0;
        memReq0Full <= 0;
        memReq1Full <= 0;
    endmethod

    method Bool reqNotEmpty0();
        return memReqQs[0].notEmpty;
    endmethod

    method Bool reqNotEmpty1();
        return memReqQs[1].notEmpty;
    endmethod
    
    method Bool reqNotEmpty2();
        return memReqQs[2].notEmpty;
    endmethod

    interface req = toPut(reqQ);
    interface resp = toGet(respQ);
    interface memReqs = map(toGet, memReqQs);
    interface memResps = map(toPut, memRespQs);
endmodule

endpackage
