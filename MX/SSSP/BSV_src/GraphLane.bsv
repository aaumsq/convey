package GraphLane;

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

interface GraphLane;
    interface Put#(GraphReq) req;
    interface Get#(GraphResp) resp;
    interface Get#(BC_MC_REQ) memReq;
    interface Put#(BC_MC_RSP) memResp;

    method Action init(BC_AEId fpgaId, BC_Addr nodePtr, BC_Addr edgePtr);
endinterface

(* synthesize *)
module mkGraphLane(GraphLane);
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
    
    
    // Graph Payload CAS FSM
    Reg#(NodeID) cas_nodeID <- mkRegU;
    Reg#(NodePayload) cas_cmpVal <- mkRegU;
    Reg#(NodePayload) cas_swapVal <- mkRegU;
    Reg#(Channel) cas_channel <- mkRegU;
    Stmt casStmt = 
    seq
        action
            Bit#(32) gaddr = pack(GaloisAddress{mod: MK_GRAPH, addr: 0});
            Bit#(48) vaddrBase = nodePtr + (extend(cas_nodeID) << `LG_GRAPH_NODE_SIZE);
            // Payload is the 3rd 32-bit (4B) entry in struct
            Bit#(48) vaddr = vaddrBase + (2 * 4);
            //$display("~~~ GraphEngine CAS FSM start! enq mem req @ vaddr: %0x, cas_idx: %0d, cmpVal: %0d, swapVal: %0d", vaddr, cas_idx, cas_cmpVal, cas_swapVal);
            //$display("~~~  nodePtr: %0x, vaddrBase: %0x, nodeID: %0x, vaddr: %0x", nodePtr, vaddrBase, cas_nodeID, vaddr);
            memReqQ.enq(BC_MC_REQ{ cmd_sub: REQ_ATOM_CAS, rtnctl: gaddr, len: BC_4B, vadr: vaddr, data: {cas_cmpVal, cas_swapVal}});
        endaction
        
        action
            BC_MC_RSP rsp = memRespQs[0].first();
            memRespQs[0].deq();
            
            // rsp.data is old data previous to swap, or the new data if cmp failed
            Bit#(32) curData = truncate(rsp.data);
            Bool success = (curData == pack(cas_cmpVal));
            GraphResp resp = tagged CAS{success: success, oldVal: curData, channel: cas_channel};
            //$display("~~~ GraphEngine CAS complete, success: %d, oldVal: %0d, channel: %d", success, curData, cas_channel);
            respQ.enq(resp);
        endaction
    endseq;
    
    FSM casFSM <- mkFSM(casStmt);
    
    // Read Edge FSM
    Reg#(EdgePtr) readEdge_edgeID <- mkRegU;
    Reg#(Channel) readEdge_channel <- mkRegU;
    Stmt readEdgeStmt =
    seq
        action
            Bit#(32) gaddr = pack(GaloisAddress{mod: MK_GRAPH, addr: 1});
            Bit#(48) vaddr = edgePtr + (extend(readEdge_edgeID) << `LG_GRAPH_EDGE_SIZE);
            memReqQ.enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: gaddr, len: BC_8B, vadr: vaddr, data: ?});
        endaction
        
        action
            BC_MC_RSP rsp = memRespQs[1].first();
            memRespQs[1].deq();
            GraphEdge gedge = unpack(rsp.data);
            
            GraphResp resp = tagged Edge{gedge: gedge, channel: readEdge_channel};
            respQ.enq(resp);
            $display("Edge read finished: %x, channel: %x", gedge, readEdge_channel);
        endaction
    endseq;
    
    FSM readEdgeFSM <- mkFSM(readEdgeStmt);

    // Read Node FSM
    // Reading same node conflict?
    Reg#(NodeID) readNode_nodeID <- mkRegU;
    Reg#(Channel) readNode_channel <-mkRegU;
    Reg#(Bit#(64)) readNode_graphPt1 <- mkRegU;
    Reg#(GraphResp) readNode_resp <-mkRegU;
    Stmt readNodeStmt = 
    seq
        action
            //$display("~~~ GraphEngine lane %0d processing readNode %0d", readNode_idx, readNode_nodeID);
            Bit#(32) gaddr = pack(GaloisAddress{mod: MK_GRAPH, addr: 2});
            Bit#(48) vaddr = nodePtr + (extend(readNode_nodeID) << `LG_GRAPH_NODE_SIZE); // base + offset*16 (16B nodes)
            memReqQ.enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: gaddr, len: BC_8B, vadr: vaddr, data: ?});
        endaction
        
        action
            BC_MC_RSP rsp = memRespQs[2].first();
            memRespQs[2].deq();
            readNode_graphPt1 <= rsp.data;
            Tuple2#(Bit#(32), Bit#(32)) tmp = unpack(rsp.data);
            //$display("~~~ GraphEngine lane %0d receive readNode resp #1, data = %0x (%0d %0d)", readNode_idx, rsp.data, tpl_1(tmp), tpl_2(tmp));
            Bit#(32) gaddr = pack(GaloisAddress{mod: MK_GRAPH, addr: 3});
            Bit#(48) vaddr = nodePtr + (extend(readNode_nodeID) << `LG_GRAPH_NODE_SIZE) + 8;
            memReqQ.enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: gaddr, len: BC_8B, vadr: vaddr, data: ?});
        endaction
        
        action
            BC_MC_RSP rsp = memRespQs[3].first();
            memRespQs[3].deq();
            Tuple2#(Bit#(32), Bit#(32)) bits1 = unpack(readNode_graphPt1);
            Tuple2#(Bit#(32), Bit#(32)) bits2 = unpack(rsp.data);
            GraphNode node = unpack({rsp.data, readNode_graphPt1});
            
            //$display("~~~ GraphEngine lane %0d receive readNode resp #2 packed node: %x, raw1: %x raw2: %x", readNode_idx, node, readNode_graphPt1, rsp.data);
            GraphResp ret = tagged Node{node: node, channel: readNode_channel};
            respQ.enq(ret);
        endaction        
    endseq;
    
    FSM readNodeFSM <- mkFSM(readNodeStmt); 
   
    
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
                readNodeFSM.start();
                readNode_nodeID <= rd.id;
                readNode_channel <= rd.channel;
            end
            else if(pkt matches tagged ReadEdge .re) begin
                //$display(" ~~~ GraphEngine starting ReadEdge op edgeID %0d, channel %0d", re.edgeID, re.channel);
                readEdgeFSM.start();
                readEdge_edgeID <= re.edgeID;
                readEdge_channel <= re.channel;
            end
            else if(pkt matches tagged CAS .cas) begin
                //$display(" ~~~ GraphEngine starting CAS node ID %0d", cas.id);
                casFSM.start();
                cas_nodeID <= cas.id;
                cas_cmpVal <= cas.cmpVal;
                cas_swapVal <= cas.swapVal;
                cas_channel <= cas.channel;
            end
        endmethod
    endinterface
    
    interface resp = toGet(respQ);
    interface memReq = toGet(memReqQ);
    interface memResp = toPut(memRespQ);
endmodule

endpackage