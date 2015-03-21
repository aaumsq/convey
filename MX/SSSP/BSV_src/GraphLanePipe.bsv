package GraphLanePipe;

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
   NodePayload cmpVal;
   NodePayload swapVal;
   Channel channel;
} CASPipe deriving(Bits, Eq);

typedef struct {
   EdgePtr edgeID;
   Channel channel;
} ReadEdgePipe deriving(Bits, Eq);

typedef struct {
   NodeID nodeID;
   Channel channel;
} ReadNodePipe deriving(Bits, Eq);

interface GraphLane;
    interface Put#(GraphReq) req;
    interface Get#(GraphResp) resp;
    interface Get#(MemReq) memReq;
    interface Put#(MemResp) memResp;

    method Action init(BC_AEId fpgaId, Bit#(4) laneId, BC_Addr nodePtr, BC_Addr edgePtr);
endinterface


(* synthesize *)
(* descending_urgency = "cas2, cas, readEdge2, readEdge, readNode3, readNode2, readNode" *)
module mkGraphLanePipe(GraphLane);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(Bit#(4)) laneId <- mkRegU;
    Reg#(BC_Addr) nodePtr <- mkRegU;
    Reg#(BC_Addr) edgePtr <- mkRegU;
    
    Reg#(Bool) started <- mkReg(False);
    
    FIFOF#(GraphResp) respQ <- mkSizedFIFOF(`GRAPH_NUM_IN_FLIGHT);

    FIFOF#(MemReq) memReqQ <- mkFIFOF;
    FIFOF#(MemResp) memRespQ <- mkFIFOF;
    Vector#(4, FIFOF#(MemResp)) memRespQs <- replicateM(mkFIFOF);
    
    rule memResp_distribute;
        MemResp rsp = memRespQ.first();
        memRespQ.deq();
        memRespQs[rsp.gaddr.addr].enq(rsp);
        if(`DEBUG)$display("%d: GraphEngine[%0d][%0d] redirecting memResp to channel %0d", cur_cycle, fpgaId, laneId, rsp.gaddr.addr);
    endrule
    
    
    
    // Graph Payload CAS pipeline
    FIFOF#(CASPipe) casQ <- mkFIFOF;
    FIFOF#(CASPipe) casQ2 <- mkSizedFIFOF(16);
    
    rule cas;
        CASPipe cxt = casQ.first();
        casQ.deq();
        
        casQ2.enq(cxt);
        
        GaloisAddress gaddr = GaloisAddress{mod: MK_GRAPH, addr: 0};
        Bit#(48) vaddrBase = nodePtr + (extend(cxt.nodeID) << `LG_GRAPH_NODE_SIZE);
        
        // Payload is the 3rd 32-bit (4B) entry in struct
        Bit#(48) vaddr = vaddrBase + (2 * 4);
        
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] CAS FSM start! enq mem req @ vaddr: %0x, cas_idx: %0d, cmpVal: %0d, swapVal: %0d", cur_cycle, fpgaId, laneId, vaddr, cxt.nodeID, cxt.cmpVal, cxt.swapVal);
        memReqQ.enq(tagged MemCAS32{addr: vaddr, gaddr: gaddr, cmpVal: cxt.cmpVal, swapVal: cxt.swapVal});
    endrule
    
    rule cas2;
        CASPipe cxt = casQ2.first();
        casQ2.deq();
        
        MemResp rsp = memRespQs[0].first();
        memRespQs[0].deq();
        
        // rsp.data is old data previous to swap, or the new data if cmp failed
        Bit#(32) curData = truncate(rsp.data);
        Bool success = (curData == pack(cxt.cmpVal));
        GraphResp resp = tagged CAS{success: success, oldVal: curData, channel: cxt.channel};
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] CAS complete, success: %d, oldVal: %0d, channel: %d", cur_cycle, fpgaId, laneId, success, curData, cxt.channel);
        respQ.enq(resp);
    endrule
    
    
    // Read Edge Pipeline
    FIFOF#(ReadEdgePipe) readEdgeQ <- mkFIFOF;
    FIFOF#(ReadEdgePipe) readEdgeQ2 <- mkSizedFIFOF(16);
    
    rule readEdge;
        ReadEdgePipe cxt = readEdgeQ.first();
        readEdgeQ.deq();
        
        readEdgeQ2.enq(cxt);
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] readEdge1 edgeId=%0d channel=%0d", cur_cycle, fpgaId, laneId, cxt.edgeID, cxt.channel); 
       
        GaloisAddress gaddr = GaloisAddress{mod: MK_GRAPH, addr: 1};
        Bit#(48) vaddr = edgePtr + (extend(cxt.edgeID) << `LG_GRAPH_EDGE_SIZE);
        memReqQ.enq(tagged MemRead64{addr: vaddr, gaddr: gaddr});
    endrule
    
    rule readEdge2;
        ReadEdgePipe cxt = readEdgeQ2.first();
        readEdgeQ2.deq();
        
        MemResp rsp = memRespQs[1].first();
        memRespQs[1].deq();
        GraphEdge gedge = unpack(rsp.data);
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] readEdge2 done! edgeDest=%0d, edgeWeight=%0d, channel=%0d", cur_cycle, fpgaId, laneId, gedge.dest, gedge.weight, cxt.channel);

        GraphResp resp = tagged Edge{gedge: gedge, channel: cxt.channel};
        respQ.enq(resp);
    endrule
    

    // Read Node Pipe
    FIFOF#(ReadNodePipe) readNodeQ <- mkFIFOF;
    FIFOF#(ReadNodePipe) readNodeQ2 <- mkSizedFIFOF(16);
    FIFOF#(ReadNodePipe) readNodeQ3 <- mkSizedFIFOF(16);
    FIFOF#(Bit#(64)) readNodeQ3_partialNode <- mkSizedFIFOF(16);
    
    rule readNode;
        ReadNodePipe cxt = readNodeQ.first();
        readNodeQ.deq();
        
        readNodeQ2.enq(cxt);
        
        GaloisAddress gaddr = GaloisAddress{mod: MK_GRAPH, addr: 2};
        Bit#(48) vaddr = nodePtr + (extend(cxt.nodeID) << `LG_GRAPH_NODE_SIZE); // base + offset*16 (16B nodes)
        memReqQ.enq(tagged MemRead64{addr: vaddr, gaddr: gaddr});
    endrule
    
    rule readNode2;
        ReadNodePipe cxt = readNodeQ2.first();
        readNodeQ2.deq();
        
        readNodeQ3.enq(cxt);

        MemResp rsp = memRespQs[2].first();
        memRespQs[2].deq();
        readNodeQ3_partialNode.enq(rsp.data);
        
        Tuple2#(Bit#(32), Bit#(32)) tmp = unpack(rsp.data);
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] receive readNode resp #1, data = %0x (%0d %0d)", cur_cycle, fpgaId, laneId, rsp.data);
        GaloisAddress gaddr = GaloisAddress{mod: MK_GRAPH, addr: 3};
        Bit#(48) vaddr = nodePtr + (extend(cxt.nodeID) << `LG_GRAPH_NODE_SIZE) + 8;
        memReqQ.enq(tagged MemRead64{addr: vaddr, gaddr: gaddr});
    endrule
    
    rule readNode3;
        ReadNodePipe cxt = readNodeQ3.first();
        readNodeQ3.deq();
        
        Bit#(64) partialNode = readNodeQ3_partialNode.first();
        readNodeQ3_partialNode.deq();
        
        MemResp rsp = memRespQs[3].first();
        memRespQs[3].deq();
        GraphNode node = unpack({rsp.data, partialNode});
        
        if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] receive readNode resp #2 packed node: %x", cur_cycle, fpgaId, laneId, node);
        GraphResp ret = tagged Node{node: node, channel: cxt.channel};
        respQ.enq(ret);    
    endrule
    
    method Action init(BC_AEId fpgaid, Bit#(4) laneid, BC_Addr nodeptr, BC_Addr edgeptr);
        $display("%0d: mkGraphEngine[%0d][%0d]: init nodePtr = %0x, edgePtr = %0x", cur_cycle, fpgaid, laneid, nodeptr, edgeptr);
        fpgaId <= fpgaid;
        laneId <= laneid;
        nodePtr <= nodeptr;
        edgePtr <= edgeptr;
        
        started <= True;
    endmethod
    
    interface Put req;
        method Action put(GraphReq pkt);
            if(pkt matches tagged ReadNode .rd) begin
                if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] starting ReadNode op ID %0d, channel %0d", cur_cycle, fpgaId, laneId, rd.id, rd.channel);
                readNodeQ.enq(ReadNodePipe{nodeID: rd.id, channel: rd.channel});
            end
            else if(pkt matches tagged ReadEdge .re) begin
                if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] starting ReadEdge op edgeID %0d, channel %0d", cur_cycle, fpgaId, laneId, re.edgeID, re.channel);
                readEdgeQ.enq(ReadEdgePipe{edgeID: re.edgeID, channel: re.channel});
            end
            else if(pkt matches tagged CAS .cas) begin
                if(`DEBUG) $display("%0d: GraphEngine[%0d][%0d] starting CAS node ID %0d channel %0d", cur_cycle, fpgaId, laneId, cas.id, cas.channel);
                casQ.enq(CASPipe{nodeID: cas.id, cmpVal: cas.cmpVal, swapVal: cas.swapVal, channel: cas.channel});
            end
        endmethod
    endinterface
    
    interface resp = toGet(respQ);
    interface memReq = toGet(memReqQ);
    interface memResp = toPut(memRespQ);
endmodule

endpackage