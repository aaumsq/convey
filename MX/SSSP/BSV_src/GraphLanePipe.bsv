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

import GraphLane::*;


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


(* descending_urgency = "cas2, cas, readEdge2, readEdge, readNode3, readNode2, readNode" *)
(* synthesize *)
module mkGraphLanePipe(GraphLane);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(BC_Addr) nodePtr <- mkRegU;
    Reg#(BC_Addr) edgePtr <- mkRegU;
    
    Reg#(Bool) started <- mkReg(False);
    
    FIFOF#(GraphResp) respQ <- mkSizedFIFOF(`GRAPH_NUM_IN_FLIGHT);

    FIFOF#(BC_MC_REQ) memReqQ <- mkFIFOF;
    FIFOF#(BC_MC_RSP) memRespQ <- mkFIFOF;
    Vector#(4, FIFOF#(BC_MC_RSP)) memRespQs <- replicateM(mkFIFOF);
    
    rule memResp_distribute;
        BC_MC_RSP rsp = memRespQ.first;
        memRespQ.deq;
        GaloisAddress gaddr = unpack(rsp.rtnctl);
        memRespQs[gaddr.addr].enq(rsp);
        //$display("~~~~ GraphEngine redirecting memResp[%d] to channel %d", i, gaddr.addr);
    endrule
    
    
    
    // Graph Payload CAS pipeline
    FIFOF#(CASPipe) casQ <- mkFIFOF;
    FIFOF#(CASPipe) casQ2 <- mkSizedFIFOF(16);
    
    rule cas1;
        CASPipe cxt = casQ.first();
        casQ.deq();
        
        casQ2.enq(cxt);
        
        Bit#(32) gaddr = pack(GaloisAddress{mod: MK_GRAPH, addr: 0});
        Bit#(48) vaddrBase = nodePtr + (extend(cxt.nodeID) << `LG_GRAPH_NODE_SIZE);
        
        // Payload is the 3rd 32-bit (4B) entry in struct
        Bit#(48) vaddr = vaddrBase + (2 * 4);
        
        //$display("~~~ GraphEngine CAS FSM start! enq mem req @ vaddr: %0x, cas_idx: %0d, cmpVal: %0d, swapVal: %0d", vaddr, cas_idx, cas_cmpVal, cas_swapVal);
        //$display("~~~  nodePtr: %0x, vaddrBase: %0x, nodeID: %0x, vaddr: %0x", nodePtr, vaddrBase, cas_nodeID, vaddr);
        memReqQ.enq(BC_MC_REQ{ cmd_sub: REQ_ATOM_CAS, rtnctl: gaddr, len: BC_4B, vadr: vaddr, data: {cxt.cmpVal, cxt.swapVal}});
    endrule
    
    rule cas2;
        CASPipe cxt = casQ2.first();
        casQ2.deq();
        
        BC_MC_RSP rsp = memRespQs[0].first();
        memRespQs[0].deq();
        
        // rsp.data is old data previous to swap, or the new data if cmp failed
        Bit#(32) curData = truncate(rsp.data);
        Bool success = (curData == pack(cxt.cmpVal));
        GraphResp resp = tagged CAS{success: success, oldVal: curData, channel: cxt.channel};
        //$display("~~~ GraphEngine CAS complete, success: %d, oldVal: %0d, channel: %d", success, curData, cas_channel);
        respQ.enq(resp);
    endrule
    
    
    // Read Edge Pipeline
    FIFOF#(ReadEdgePipe) readEdgeQ <- mkFIFOF;
    FIFOF#(ReadEdgePipe) readEdgeQ2 <- mkSizedFIFOF(16);
    
    rule readEdge;
        ReadEdgePipe cxt = readEdgeQ.first();
        readEdgeQ.deq();
        
        readEdgeQ2.enq(cxt);
        
        Bit#(32) gaddr = pack(GaloisAddress{mod: MK_GRAPH, addr: 1});
        Bit#(48) vaddr = edgePtr + (extend(cxt.edgeID) << `LG_GRAPH_EDGE_SIZE);
        memReqQ.enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: gaddr, len: BC_8B, vadr: vaddr, data: ?});
    endrule
    
    rule readEdge2;
        ReadEdgePipe cxt = readEdgeQ2.first();
        readEdgeQ2.deq();
        
        BC_MC_RSP rsp = memRespQs[1].first();
        memRespQs[1].deq();
        GraphEdge gedge = unpack(rsp.data);
        
        GraphResp resp = tagged Edge{gedge: gedge, channel: cxt.channel};
        respQ.enq(resp);
        //$display("Edge read finished: %x, channel: %x", gedge, readEdge_channel);
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
        
        Bit#(32) gaddr = pack(GaloisAddress{mod: MK_GRAPH, addr: 2});
        Bit#(48) vaddr = nodePtr + (extend(cxt.nodeID) << `LG_GRAPH_NODE_SIZE); // base + offset*16 (16B nodes)
        memReqQ.enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: gaddr, len: BC_8B, vadr: vaddr, data: ?});
    endrule
    
    rule readNode2;
        ReadNodePipe cxt = readNodeQ2.first();
        readNodeQ2.deq();
        
        readNodeQ3.enq(cxt);

        BC_MC_RSP rsp = memRespQs[2].first();
        memRespQs[2].deq();
        readNodeQ3_partialNode.enq(rsp.data);
        
        Tuple2#(Bit#(32), Bit#(32)) tmp = unpack(rsp.data);
        //$display("~~~ GraphEngine lane %0d receive readNode resp #1, data = %0x (%0d %0d)", readNode_idx, rsp.data, tpl_1(tmp), tpl_2(tmp));
        Bit#(32) gaddr = pack(GaloisAddress{mod: MK_GRAPH, addr: 3});
        Bit#(48) vaddr = nodePtr + (extend(cxt.nodeID) << `LG_GRAPH_NODE_SIZE) + 8;
        memReqQ.enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: gaddr, len: BC_8B, vadr: vaddr, data: ?});    
    endrule
    
    rule readNode3;
        ReadNodePipe cxt = readNodeQ3.first();
        readNodeQ3.deq();
        
        Bit#(64) partialNode = readNodeQ3_partialNode.first();
        readNodeQ3_partialNode.deq();
        
        BC_MC_RSP rsp = memRespQs[3].first();
        memRespQs[3].deq();
        GraphNode node = unpack({rsp.data, partialNode});
        
        //$display("~~~ GraphEngine lane %0d receive readNode resp #2 packed node: %x, raw1: %x raw2: %x", readNode_idx, node, readNode_graphPt1, rsp.data);
        GraphResp ret = tagged Node{node: node, channel: cxt.channel};
        respQ.enq(ret);    
    endrule
    
    method Action init(BC_AEId fpgaid, BC_Addr nodeptr, BC_Addr edgeptr);
        $display("%0d: ~~~~ mkGraphEngine[%0d]: init nodePtr = %0x, edgePtr = %0x", cur_cycle, fpgaid, nodeptr, edgeptr);
        fpgaId <= fpgaid;
        nodePtr <= nodeptr;
        edgePtr <= edgeptr;
        
        started <= True;
    endmethod
    
    interface Put req;
        method Action put(GraphReq pkt);
            //$display(" ~~~ GraphEngine put request");
            //reqQ[i].enq(pkt);
            if(pkt matches tagged ReadNode .rd) begin
                //$display(" ~~~ GraphEngine starting ReadNode op ID %0d, channel %0d", rd.id, rd.channel);
                readNodeQ.enq(ReadNodePipe{nodeID: rd.id, channel: rd.channel});
            end
            else if(pkt matches tagged ReadEdge .re) begin
                //$display(" ~~~ GraphEngine starting ReadEdge op edgeID %0d, channel %0d", re.edgeID, re.channel);
                readEdgeQ.enq(ReadEdgePipe{edgeID: re.edgeID, channel: re.channel});
            end
            else if(pkt matches tagged CAS .cas) begin
                //$display(" ~~~ GraphEngine starting CAS node ID %0d", cas.id);
                casQ.enq(CASPipe{nodeID: cas.id, cmpVal: cas.cmpVal, swapVal: cas.swapVal, channel: cas.channel});
            end
        endmethod
    endinterface
    
    interface resp = toGet(respQ);
    interface memReq = toGet(memReqQ);
    interface memResp = toPut(memRespQ);
endmodule

endpackage