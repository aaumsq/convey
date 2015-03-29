
package SSSPEngine;

import Vector           :: *;
import FIFOF            :: *;
import SpecialFIFOs     :: *;
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

import CreditFIFOF::*;
import CoalescingCounter::*;
import GaloisTypes::*;
`include "GaloisDefs.bsv"

interface Engine;
    method Action init(BC_AEId fpgaId, Bit#(4) laneId);
    method ActionValue#(Bit#(64)) result;
    method Bool isDone;
    
    interface Put#(WLEntry) workIn;
    interface Get#(WLEntry) workOut;

    interface Vector#(2, Get#(GraphNodeReq)) graphNodeReqs;
    interface Vector#(1, Get#(GraphEdgeReq)) graphEdgeReqs;
    interface Vector#(1, Get#(GraphCASReq)) graphCASReqs;
    interface Vector#(2, Put#(GraphNodeResp)) graphNodeResps;
    interface Vector#(1, Put#(GraphEdgeResp)) graphEdgeResps;
    interface Vector#(1, Put#(GraphCASResp)) graphCASResps;
endinterface

(* synthesize, descending_urgency = "casDone, cas, deqCasStall, recvDestNode, casRetry, getDestNode, getEdges, recvSrcNode, getSrcNode" *)
module mkSSSPEngine(Engine ifc);
    Reg#(BC_AEId) fpgaId <-mkRegU;
    Reg#(Bit#(4)) laneId <- mkRegU;
    Reg#(Bool) started <- mkReg(False);
    Reg#(Bool) done <- mkRegU;
    
    FIFOF#(WLEntry) workInQ <- mkFIFOF;
    FIFOF#(WLEntry) workOutQ <- mkFIFOF;
    
    Vector#(2, FIFOF#(GraphNodeReq)) graphNodeReqQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphEdgeReq)) graphEdgeReqQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphCASReq)) graphCASReqQs <- replicateM(mkFIFOF);
    Vector#(2, FIFOF#(GraphNodeResp)) graphNodeRespQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphEdgeResp)) graphEdgeRespQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphCASResp)) graphCASRespQs <- replicateM(mkFIFOF);
    
    FIFOF#(GraphNode) graphNodeQ1 <- mkSizedFIFOF(2);
    FIFOF#(GraphNode) graphNodeQ2 <- mkSizedFIFOF(`SSSPENGINE_NUM_IN_FLIGHT);  // # entries = # edgeReq in flight
    
    FIFOF#(NodePayload) newDistQ <- mkSizedFIFOF(16);   // # entries = # destNode in flight
    FIFOF#(Tuple3#(NodePayload, NodePayload, GraphNode)) casContextQ1 <- mkSizedFIFOF(2);
    FIFOF#(Tuple3#(NodePayload, NodePayload, GraphNode)) casContextQ2 <- mkSizedFIFOF(`SSSPENGINE_NUM_IN_FLIGHT); // # entries = # CAS requests in flight
    FIFOF#(Tuple3#(NodePayload, NodePayload, GraphNode)) casContextRetryQ <- mkSizedFIFOF(2);
    CoalescingCounter casNumInFlight <- mkCCounter();
    
    //LFSR#(Bit#(8)) lfsr <- mkLFSR_8;
    Reg#(Bit#(8)) casStall <- mkRegU;
    
    Reg#(Bit#(48)) numWorkFetched <- mkRegU;
    Reg#(Bit#(48)) numWorkRetired <- mkRegU;
    Reg#(Bit#(48)) numEdgesFetched <- mkRegU;
    Reg#(Bit#(48)) numEdgesRetired <- mkRegU;
    Reg#(Bit#(48)) numEdgesDiscarded <- mkRegU;
    
    Reg#(Bit#(48)) numCASIssued <- mkRegU;
    Reg#(Bit#(48)) numCASRetried <- mkRegU;
    
    function Bool isChannel(GraphResp resp, Channel chan);
        Bool ret = False;
        if(resp matches tagged Node .gnode) begin
            if(gnode.channel == chan) begin
                ret = True;
            end
        end
        return ret;
    endfunction
    /*
    rule printFIFOFulls;
 //       if(fpgaId == 0 && laneId==0) begin
        let cycle <- cur_cycle;
            if(cycle == 10000000) $display("%0d: SSSPEngine[%0d][%0d] all fulls: workIn:%b workOut:%b graphReq:%b graphResp:%b graphNodeQ1:%b graphNodeQ2:%b newDist:%b casQ1:%b casQ2:%b",
                     cur_cycle, fpgaId, laneId,
                     !workInQ.notFull, !workOutQ.notFull, !graphReqQ.notFull, !graphRespQ.notFull, !graphNodeQ1.notFull, !graphNodeQ2.notFull, !newDistQ.notFull,
                     !casContextQ1.notFull, !casContextQ2.notFull);
            if(cycle == 10000000) $display("%0d: SSSPEngine[%0d][%0d] all empties: workIn:%b workOut:%b graphReq:%b graphResp:%b graphNodeQ1:%b graphNodeQ2:%b newDist:%b casQ1:%b casQ2:%b",
                     cur_cycle, fpgaId, laneId,
                     !workInQ.notEmpty, !workOutQ.notEmpty, !graphReqQ.notEmpty, !graphRespQ.notEmpty, !graphNodeQ1.notEmpty, !graphNodeQ2.notEmpty, !newDistQ.notEmpty,
                     !casContextQ1.notEmpty, !casContextQ2.notEmpty);
            
   //     end
    endrule
    */
    rule calcDone;
        Bool workEmpty = !workInQ.notEmpty && !workOutQ.notEmpty;
        
        Bool noNodesInFlight = (numWorkFetched == numWorkRetired);
        Bool noEdgesInFlight = (numEdgesFetched == (numEdgesRetired + numEdgesDiscarded));
        
        if(workEmpty && noNodesInFlight && noEdgesInFlight) begin
            done <= True;
        end
        else begin
            done <= False;
        end
    endrule
    
    rule getSrcNode(started);
        WLEntry pkt = workInQ.first();
        workInQ.deq();
        WLJob job = tpl_2(pkt);
        
        if(`DEBUG) $display("%0d: ~~~ SSSPEngine[%0d]: START getSrcNode priority: %0d, nodeID: %0d", cur_cycle, fpgaId, tpl_1(pkt), job);
        
        graphNodeReqQs[0].enq(GraphNodeReq{id: job});
        numWorkFetched <= numWorkFetched + 1;
    endrule
    
    rule recvSrcNode;
        GraphNodeResp nodeResp = graphNodeRespQs[0].first();
        graphNodeRespQs[0].deq();
        GraphNode node = nodeResp.node;
        graphNodeQ1.enq(node);
        if(`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: graphNode ID %0d payload %0d edgePtr %0d numEdges %0d ", cur_cycle, fpgaId, laneId, node.id, node.payload, node.edgePtr, node.numEdges);
    endrule
    
    Reg#(NodeNumEdges) edgeIdx <- mkReg(0);
    rule getEdges;
        GraphNode node = graphNodeQ1.first();        
        EdgePtr edgeID = node.edgePtr + edgeIdx;
        graphEdgeReqQs[0].enq(GraphEdgeReq{id: edgeID});
        graphNodeQ2.enq(node);
        if(`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: getEdges %0d of %0d", cur_cycle, fpgaId, laneId, edgeIdx, node.numEdges-1);
        numEdgesFetched <= numEdgesFetched + 1;
        
        if(edgeIdx == (node.numEdges - 1)) begin
            edgeIdx <= 0;
            graphNodeQ1.deq();
            numWorkRetired <= numWorkRetired + 1;
        end
        else begin
            edgeIdx <= edgeIdx + 1;
        end
    endrule
    
    rule getDestNode;
        GraphEdgeResp edgeResp = graphEdgeRespQs[0].first();
        graphEdgeRespQs[0].deq();
        
        GraphNode n = graphNodeQ2.first();
        graphNodeQ2.deq();
        
        GraphEdge e = edgeResp.gedge;
        NodePayload newDist = n.payload + e.weight;
        newDistQ.enq(newDist);
        if(`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: getDestNode num %0d", cur_cycle, fpgaId, laneId, e.dest);
        graphNodeReqQs[1].enq(GraphNodeReq{id: e.dest});
    endrule
    
    rule deqCasStall(casStall > 0);
        casStall <= casStall - 1;
    endrule        
    
    rule casRetry;
        casContextQ1.enq(casContextRetryQ.first());
        casContextRetryQ.deq();
    endrule
    
    rule recvDestNode(casNumInFlight.notMax);
        GraphNodeResp nodeResp = graphNodeRespQs[1].first();
        graphNodeRespQs[1].deq();
        
        GraphNode node = nodeResp.node;
        NodePayload newDist = newDistQ.first();
        newDistQ.deq();
        if(`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: recvDestNode, enqueueing CAS: %0d < %0d?", cur_cycle, fpgaId, laneId, newDist, node.payload);
        // tuple3(cmpVal, newVal, destNode)
        casContextQ1.enq(tuple3(node.payload, newDist, node));
        casNumInFlight.inc();
    endrule
    
    rule cas;
        // tuple3(cmpVal, newVal, destNode)
        Tuple3#(NodePayload, NodePayload, GraphNode) cxt = casContextQ1.first();
        casContextQ1.deq();
        if(`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: Attempting CAS...", cur_cycle, fpgaId, laneId);
        if(tpl_2(cxt) < tpl_1(cxt)) begin
            if(`DEBUG) $display("   %d < %d, executing CAS!", tpl_2(cxt), tpl_1(cxt));
            graphCASReqQs[0].enq(GraphCASReq{id: tpl_3(cxt).id, cmpVal: tpl_1(cxt), swapVal: tpl_2(cxt)});
            casContextQ2.enq(cxt);
            numCASIssued <= numCASIssued + 1;
        end
        else begin
            casNumInFlight.dec();
            numEdgesDiscarded <= numEdgesDiscarded + 1;
        end
    endrule
    
    rule casDone;
        GraphCASResp casResp = graphCASRespQs[0].first();
        graphCASRespQs[0].deq();
        
        // tuple3(cmpVal, newVal, destNode)
        Tuple3#(NodePayload, NodePayload, GraphNode) cxt = casContextQ2.first();
        casContextQ2.deq();
        
        if(casResp.success) begin
            GraphNode node = tpl_3(cxt);
            WLEntry newWork = tuple2(0, node.id);
            workOutQ.enq(newWork);
            if(`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: CAS Success! Num retired: %0d, num discarded: %0d. Enqueueing new work item: ", cur_cycle, fpgaId, laneId, numEdgesRetired+1, numEdgesDiscarded, fshow(newWork));
            else begin
                if(numEdgesDiscarded % 1024 == 0) begin
                    $display("%0d: SSSPEngine[%0d][%0d]: CAS Success! Edges retired: %0d, edges discarded: %0d, CAS issued: %0d, CAS retried: %0d. Enqueueing new work item: ", cur_cycle, fpgaId, laneId, numEdgesRetired+1, numEdgesDiscarded, numCASIssued, numCASRetried, fshow(newWork));
                end
            end
            numEdgesRetired <= numEdgesRetired + 1;
            casNumInFlight.dec2();
        end
        else begin
            casContextRetryQ.enq(tuple3(casResp.oldVal, tpl_2(cxt), tpl_3(cxt)));
            //casStall <= lfsr.value() >> 2;
            //lfsr.next();
            numCASRetried <= numCASRetried + 1;
            //$display("%0d: SSSPEngine[%0d][%0d]: CAS failed, retry...", cur_cycle, fpgaId, laneId);
        end
    endrule
    
    method Action init(BC_AEId fpgaid, Bit#(4) laneid);
        fpgaId <= fpgaid;
        laneId <= laneid;
        started <= True;
        done <= False;
        
        //lfsr.seed({2'b0, fpgaid, laneid});
        casStall <= 0;
        casNumInFlight.init(`SSSPENGINE_NUM_IN_FLIGHT);
        
        numWorkFetched <= 0;
        numWorkRetired <= 0;
        numEdgesFetched <= 0;
        numEdgesRetired <= 0;
        numEdgesDiscarded <= 0;
        
        numCASIssued  <= 0;
        numCASRetried <= 0;
    endmethod
    
    method ActionValue#(Bit#(64)) result();
        return extend(numEdgesFetched);
    endmethod
    
    method Bool isDone;
        return done;
    endmethod
    
    interface workIn = toPut(workInQ);
    interface workOut = toGet(workOutQ);
    interface graphNodeReqs = map(toGet, graphNodeReqQs);
    interface graphEdgeReqs = map(toGet, graphEdgeReqQs);
    interface graphCASReqs = map(toGet, graphCASReqQs);
    interface graphNodeResps = map(toPut, graphNodeRespQs);
    interface graphEdgeResps = map(toPut, graphEdgeRespQs);
    interface graphCASResps = map(toPut, graphCASRespQs);
endmodule

endpackage
