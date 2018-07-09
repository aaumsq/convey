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
   Bit#(1) procId;
} ReadNodePipe deriving(Bits, Eq);


interface GraphNodeIfc;
    interface Put#(GraphNodeReq) req;
    interface Get#(GraphNodeResp) resp;
    interface Vector#(2, Get#(MemReq)) memReqs;
    interface Vector#(2, Put#(MemResp)) memResps;
    
    method Action init(BC_AEId fpgaId, Bit#(4) laneId, BC_Addr nodePtr);
    method Bool reqNotEmpty0();
    method Bool reqNotEmpty1();
endinterface

(* descending_urgency = "readNode1, readNode" *)
module mkGraphNodePipe#(Integer lane0, Integer lane1)(GraphNodeIfc);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(Bit#(4)) laneId <- mkRegU;
    Reg#(BC_Addr) nodePtr <- mkRegU;

    // Read Node Pipe
    FIFOF#(GraphNodeReq) reqQ <- mkFIFOF;
    FIFOF#(ReadNodePipe) readNodeQ <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT);
    FIFOF#(GraphNodeResp) respQ <- mkFIFOF;
    Vector#(2, FIFOF#(MemReq)) memReqQs <- replicateM(mkFIFOF);
    Vector#(2, FIFOF#(MemResp)) memRespQs <- replicateM(mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT));
    //Vector#(2, FIFOF#(MemResp)) memRespQs <- replicateM(mkFIFOF);
    Reg#(Bit#(48)) readNodeFull <- mkRegU;
    Reg#(Bit#(48)) respFull <- mkRegU;
    Reg#(Bit#(48)) memReq0Full <- mkRegU;
    Reg#(Bit#(48)) memReq1Full <- mkRegU;
    
    rule setCount;
        if(!readNodeQ.notFull)
            readNodeFull <= readNodeFull + 1;
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
        
        readNodeQ.enq(ReadNodePipe{nodeID: nodeReq.id, procId: nodeReq.procId});
        
        GaloisAddress gaddr0 = GaloisAddress{mod: MK_GRAPH, addr: fromInteger(lane0)};
        Bit#(48) vaddr01 = nodePtr + (extend(nodeReq.id) << `LG_GRAPH_NODE_SIZE); // base + offset*32 (32B nodes)
	Bit#(48) vaddr02 = nodePtr + (extend(nodeReq.id) << `LG_GRAPH_NODE_SIZE) + 16;
	Bit#(48) vaddr0 = (nodeReq.procId == 1) ? vaddr02 : vaddr01;
        memReqQs[0].enq(tagged MemRead64{addr: vaddr0, gaddr: gaddr0});
        if (`DEBUG) $display("%0d: GraphEngine[%0d][%0d] #1 request for Node %0d, addr = %0x", cur_cycle, fpgaId, laneId, nodeReq.id, vaddr0);

        GaloisAddress gaddr1 = GaloisAddress{mod: MK_GRAPH, addr: fromInteger(lane1)};
        Bit#(48) vaddr11 = nodePtr + (extend(nodeReq.id) << `LG_GRAPH_NODE_SIZE) + 8;
	Bit#(48) vaddr12 = nodePtr + (extend(nodeReq.id) << `LG_GRAPH_NODE_SIZE) + 24;
	Bit#(48) vaddr1 = (nodeReq.procId == 1) ? vaddr12 : vaddr11;
        memReqQs[1].enq(tagged MemRead64{addr: vaddr1, gaddr: gaddr1});
        if (`DEBUG) $display("%0d: GraphEngine[%0d][%0d] #2 request for Node %0d, addr = %0x", cur_cycle, fpgaId, laneId, nodeReq.id, vaddr1);
    endrule
    
    rule readNode1;
        ReadNodePipe cxt = readNodeQ.first();
        readNodeQ.deq();

        MemResp rsp0 = memRespQs[0].first();
        memRespQs[0].deq();
        
        MemResp rsp1 = memRespQs[1].first();
        memRespQs[1].deq();
        GraphNode node = unpack({rsp1.data, rsp0.data});
        
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] receive readNode resp packed node: %x", cur_cycle, fpgaId, laneId, node);
        respQ.enq(GraphNodeResp{node: node});
    endrule

    method Action init(BC_AEId fid, Bit#(4) lid, BC_Addr ptr);
        fpgaId <= fid;
        laneId <= lid;
        nodePtr <= ptr;
        
        readNodeFull <= 0;
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
    
    interface req = toPut(reqQ);
    interface resp = toGet(respQ);
    interface memReqs = map(toGet, memReqQs);
    interface memResps = map(toPut, memRespQs);
endmodule

endpackage
