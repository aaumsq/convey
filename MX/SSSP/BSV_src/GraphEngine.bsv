package GraphEngine;

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

interface GraphEngine;
    interface Vector#(`GRAPH_PORTS, Put#(GraphReq)) req;
    interface Vector#(`GRAPH_PORTS, Get#(GraphResp)) resp;
    interface Vector#(16, Get#(BC_MC_REQ)) memReq;
    interface Vector#(16, Put#(BC_MC_RSP)) memResp;

    method Action init(BC_AEId fpgaId, BC_Addr nodePtr, BC_Addr edgePtr);
endinterface

(* synthesize *)
module mkGraphEngine(GraphEngine);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(BC_Addr) nodePtr <- mkRegU;
    Reg#(BC_Addr) edgePtr <- mkRegU;
    
    Reg#(Bool) started <- mkReg(False);
    
    Vector#(`GRAPH_PORTS, FIFOF#(GraphReq)) reqQ <- replicateM(mkFIFOF);
    Vector#(`GRAPH_PORTS, FIFOF#(GraphResp)) respQ <- replicateM(mkSizedFIFOF(`GRAPH_NUM_IN_FLIGHT));
    Vector#(`GRAPH_PORTS, Reg#(Bit#(10))) respCredits <- replicateM(mkRegU);

    Vector#(16, FIFOF#(BC_MC_REQ)) memReqQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(BC_MC_RSP)) memRespQ <- replicateM(mkFIFOF);
    Vector#(16, Vector#(4, FIFOF#(BC_MC_RSP))) memRespQs <- replicateM(replicateM(mkFIFOF));
    
    for(Integer i = 0; i < 16; i = i+1) begin
        rule memResp_distribute;
            BC_MC_RSP rsp = memRespQ[i].first;
            memRespQ[i].deq;
            GaloisAddress gaddr = unpack(rsp.rtnctl);
            memRespQs[i][gaddr.addr].enq(rsp);
            //$display("~~~~ GraphEngine redirecting memResp[%d] to channel %d", i, gaddr.addr);
        endrule
        
        rule memReqDisplay;
            //$display("~~~~ GraphEngine sending memReq[%d], channel %x ", i, memReqQ[i].first.rtnctl, fshow(memReqQ[i].first));
        endrule
    end
    
    
    // Graph Payload CAS FSM
    Reg#(NodeID) cas_nodeID <- mkRegU;
    Reg#(NodePayload) cas_cmpVal <- mkRegU;
    Reg#(NodePayload) cas_swapVal <- mkRegU;
    Reg#(Bit#(8)) cas_idx <- mkRegU;
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
            memReqQ[cas_idx].enq(BC_MC_REQ{ cmd_sub: REQ_ATOM_CAS, rtnctl: gaddr, len: BC_4B, vadr: vaddr, data: {cas_cmpVal, cas_swapVal}});
        endaction
        
        action
            BC_MC_RSP rsp = memRespQs[cas_idx][0].first();
            memRespQs[cas_idx][0].deq();
            
            // rsp.data is old data previous to swap, or the new data if cmp failed
            Bit#(32) curData = truncate(rsp.data);
            Bool success = (curData == pack(cas_cmpVal));
            GraphResp resp = tagged CAS{success: success, oldVal: curData, channel: cas_channel};
            //$display("~~~ GraphEngine CAS complete, success: %d, oldVal: %0d, channel: %d", success, curData, cas_channel);
            respQ[cas_idx].enq(resp);
        endaction
    endseq;
    
    Vector#(`GRAPH_PORTS, FSM) casFSMs <- replicateM(mkFSM(casStmt));
    
    // Read Edge FSM
    Reg#(EdgePtr) readEdge_edgeID <- mkRegU;
    Reg#(Bit#(8)) readEdge_idx <- mkRegU;
    Reg#(Channel) readEdge_channel <- mkRegU;
    Stmt readEdgeStmt =
    seq
        action
            Bit#(32) gaddr = pack(GaloisAddress{mod: MK_GRAPH, addr: 1});
            Bit#(48) vaddr = edgePtr + (extend(readEdge_edgeID) << `LG_GRAPH_EDGE_SIZE);
            memReqQ[readEdge_idx].enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: gaddr, len: BC_8B, vadr: vaddr, data: ?});
        endaction
        
        action
            BC_MC_RSP rsp = memRespQs[readEdge_idx][1].first();
            memRespQs[readEdge_idx][1].deq();
            GraphEdge gedge = unpack(rsp.data);
            
            GraphResp resp = tagged Edge{gedge: gedge, channel: readEdge_channel};
            respQ[readEdge_idx].enq(resp);
            $display("Edge read finished: %x, channel: %x", gedge, readEdge_channel);
        endaction
    endseq;
    
    Vector#(`GRAPH_PORTS, FSM) readEdgeFSMs <- replicateM(mkFSM(readEdgeStmt));

    // Read Node FSM
    // Reading same node conflict?
    Reg#(NodeID) readNode_nodeID <- mkRegU;
    Reg#(Bit#(8)) readNode_idx <- mkRegU;
    Reg#(Channel) readNode_channel <-mkRegU;
    Reg#(Bit#(64)) readNode_graphPt1 <- mkRegU;
    Reg#(GraphResp) readNode_resp <-mkRegU;
    Stmt readNodeStmt = 
    seq
        action
            //$display("~~~ GraphEngine lane %0d processing readNode %0d", readNode_idx, readNode_nodeID);
            Bit#(32) gaddr = pack(GaloisAddress{mod: MK_GRAPH, addr: 2});
            Bit#(48) vaddr = nodePtr + (extend(readNode_nodeID) << `LG_GRAPH_NODE_SIZE); // base + offset*16 (16B nodes)
            memReqQ[readNode_idx].enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: gaddr, len: BC_8B, vadr: vaddr, data: ?});
        endaction
        
        action
            BC_MC_RSP rsp = memRespQs[readNode_idx][2].first();
            memRespQs[readNode_idx][2].deq();
            readNode_graphPt1 <= rsp.data;
            Tuple2#(Bit#(32), Bit#(32)) tmp = unpack(rsp.data);
            //$display("~~~ GraphEngine lane %0d receive readNode resp #1, data = %0x (%0d %0d)", readNode_idx, rsp.data, tpl_1(tmp), tpl_2(tmp));
            Bit#(32) gaddr = pack(GaloisAddress{mod: MK_GRAPH, addr: 3});
            Bit#(48) vaddr = nodePtr + (extend(readNode_nodeID) << `LG_GRAPH_NODE_SIZE) + 8;
            memReqQ[readNode_idx].enq(BC_MC_REQ{cmd_sub: REQ_RD, rtnctl: gaddr, len: BC_8B, vadr: vaddr, data: ?});
        endaction
        
        action
            BC_MC_RSP rsp = memRespQs[readNode_idx][3].first();
            memRespQs[readNode_idx][3].deq();
            Tuple2#(Bit#(32), Bit#(32)) bits1 = unpack(readNode_graphPt1);
            Tuple2#(Bit#(32), Bit#(32)) bits2 = unpack(rsp.data);
            GraphNode node = unpack({rsp.data, readNode_graphPt1});
            
            //$display("~~~ GraphEngine lane %0d receive readNode resp #2 packed node: %x, raw1: %x raw2: %x", readNode_idx, node, readNode_graphPt1, rsp.data);
            GraphResp ret = tagged Node{node: node, channel: readNode_channel};
            respQ[readNode_idx].enq(ret);
        endaction        
    endseq;
    
    Vector#(`GRAPH_PORTS, FSM) readNodeFSMs <- replicateM(mkFSM(readNodeStmt)); 
   
    function Put#(GraphReq) mkEnqF(Integer i);
        return interface Put#(GraphReq);
            method Action put(GraphReq pkt) if(started);
                //$display(" ~~~ GraphEngine put request");
                //reqQ[i].enq(pkt);
                if(pkt matches tagged ReadNode .rd) begin
                    //$display(" ~~~ GraphEngine starting ReadNode op ID %0d, channel %0d", rd.id, rd.channel);
                    readNodeFSMs[i].start();
                    readNode_nodeID <= rd.id;
                    readNode_idx <= fromInteger(i);
                    readNode_channel <= rd.channel;
                end
                else if(pkt matches tagged ReadEdge .re) begin
                    //$display(" ~~~ GraphEngine starting ReadEdge op edgeID %0d, channel %0d", re.edgeID, re.channel);
                    readEdgeFSMs[i].start();
                    readEdge_idx <= fromInteger(i);
                    readEdge_edgeID <= re.edgeID;
                    readEdge_channel <= re.channel;
                end
                else if(pkt matches tagged CAS .cas) begin
                    //$display(" ~~~ GraphEngine starting CAS node ID %0d", cas.id);
                    casFSMs[i].start();
                    cas_idx <= fromInteger(i);
                    cas_nodeID <= cas.id;
                    cas_cmpVal <= cas.cmpVal;
                    cas_swapVal <= cas.swapVal;
                    cas_channel <= cas.channel;
                end
            endmethod
        endinterface;
    endfunction
    
    method Action init(BC_AEId fpgaid, BC_Addr nodeptr, BC_Addr edgeptr);
        $display("%0d: ~~~~ mkGraphEngine[%0d]: init nodePtr = %0x, edgePtr = %0x", cur_cycle, fpgaid, nodeptr, edgeptr);
        fpgaId <= fpgaid;
        nodePtr <= nodeptr;
        edgePtr <= edgeptr;
        
        started <= True;
        
    endmethod
    
    interface req = genWith(mkEnqF);
    interface resp = map(toGet, respQ);
    interface memReq = map(toGet, memReqQ);
    interface memResp = map(toPut, memRespQ);
endmodule

endpackage