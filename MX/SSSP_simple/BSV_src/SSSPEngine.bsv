
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

import BufBRAMFIFOF::*;
import CreditFIFOF::*;
import CoalescingCounter::*;
import GaloisTypes::*;
`include "GaloisDefs.bsv"

interface Engine;
    method Action init(BC_AEId fpgaId, Bit#(4) laneId, Bit#(5) bSize, Bit#(32) bias);
    method ActionValue#(Bit#(64)) result;
    method ActionValue#(Bit#(64)) numCASRetry();
    method ActionValue#(Bit#(64)) numWLStall();
    method ActionValue#(Bit#(64)) numEPStall();
    method ActionValue#(Bit#(64)) numNodes();
    method Action setOffset(Bit#(64) offset);
    method Action stop;
    method Bool isDone;
    
    interface Put#(WLEntry) workIn;
    interface Get#(WLEntry) workOut;

    interface Vector#(1, Get#(GraphNodeReq)) graphNodeReqs;
    interface Vector#(1, Get#(GraphPartialNodeReq)) graphPartialNodeReqs;
    interface Vector#(1, Get#(GraphEdgeReq)) graphEdgeReqs;
    interface Vector#(1, Get#(GraphCASReq)) graphCASReqs;
    interface Vector#(1, Put#(GraphNodeResp)) graphNodeResps;
    interface Vector#(1, Put#(GraphPartialNodeResp)) graphPartialNodeResps;
    interface Vector#(1, Put#(GraphEdgeResp)) graphEdgeResps;
    interface Vector#(1, Put#(GraphCASResp)) graphCASResps;
endinterface

(* synthesize, descending_urgency = "casDone, cas, recvDestNode, casRetry, getDestNode, getEdges, recvSrcNode, getSrcNode" *)
//(* synthesize, descending_urgency = "casDone, cas, recvDestNode, casRetry, getDestNode, recvSrcNode, getEdges, getSrcNode" *)
module mkSSSPEngine(Engine ifc);
    Reg#(BC_AEId) fpgaId <-mkRegU;
    Reg#(Bit#(4)) laneId <- mkRegU;
    Reg#(Bool) started <- mkReg(False);
    Reg#(Bool) done <- mkReg(True);
    Reg#(Bit#(5)) rg_bSize <- mkRegU;
    Reg#(Bit#(32)) rg_bias <- mkRegU;
    Reg#(Bit#(64)) rg_offset <- mkRegU;
    Reg#(Bit#(32)) rg_totalBias <- mkRegU;
    
    FIFOF#(WLEntry) workInQ <- mkFIFOF;
    FIFOF#(WLEntry) workOutQ <- mkFIFOF;
    
    Vector#(1, FIFOF#(GraphNodeReq)) graphNodeReqQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphPartialNodeReq)) graphPartialNodeReqQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphEdgeReq)) graphEdgeReqQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphCASReq)) graphCASReqQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphNodeResp)) graphNodeRespQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphPartialNodeResp)) graphPartialNodeRespQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphEdgeResp)) graphEdgeRespQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphCASResp)) graphCASRespQs <- replicateM(mkFIFOF);
    
    FIFOF#(Bit#(1)) graphNodeQ0 <- mkSizedBufBRAMFIFOF(2*`GRAPH_NUM_IN_FLIGHT); // # nodes to fetch ahead of edges
    FIFOF#(GraphTmp) graphNodeQ1 <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT); // # nodes to fetch ahead of edges
    FIFOF#(GraphTmp) graphNodeQ2 <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT);  // # entries = # edgeReq in flight
    
    FIFOF#(NodePayload) newDistQ <- mkSizedBufBRAMFIFOF(`SSSPENGINE_NUM_IN_FLIGHT);   // # entries = # destNode in flight
    FIFOF#(GraphTmp2) newDistProcQ <- mkSizedBufBRAMFIFOF(`SSSPENGINE_NUM_IN_FLIGHT);   // # entries = # destNode in flight
    FIFOF#(Tuple3#(NodePayload, NodePayload, GraphTmp3)) casContextQ1 <- mkSizedFIFOF(4);
    FIFOF#(Tuple3#(NodePayload, NodePayload, GraphTmp3)) casContextQ2 <- mkSizedBufBRAMFIFOF(`SSSPENGINE_NUM_IN_FLIGHT); // # entries = # CAS requests in flight
    FIFOF#(Tuple3#(NodePayload, NodePayload, GraphTmp3)) casContextRetryQ <- mkSizedBufBRAMFIFOF(`SSSPENGINE_NUM_CAS_RETRY_IN_FLIGHT);
    FIFOF#(Bit#(8)) casContextRetryStallQ <- mkSizedBufBRAMFIFOF(`SSSPENGINE_NUM_CAS_RETRY_IN_FLIGHT);
    CoalescingCounter casNumInFlight <- mkCCounter();
    
    LFSR#(Bit#(8)) lfsr <- mkLFSR_8;
    
    Reg#(Bit#(48)) numWorkFetched <- mkRegU;
    Reg#(Bit#(48)) numWorkRetired <- mkRegU;
    Reg#(Bit#(48)) numWorkDiscarded <- mkRegU;
    Reg#(Bit#(48)) numEdgesFetched <- mkRegU;
    Reg#(Bit#(48)) numEdgesRetired <- mkRegU;
    Reg#(Bit#(64)) numWorklistStall <- mkRegU;
    Reg#(Bit#(64)) numEdgePipeStall <- mkRegU;
    Reg#(Bit#(48)) numEdgesDiscarded <- mkRegU;
    
    Reg#(Bit#(48)) numCASIssued <- mkRegU;
    Reg#(Bit#(64)) numCASRetried <- mkRegU;
    
    Reg#(Bit#(48)) numWorkInEmpty <- mkRegU;
    Reg#(Bit#(48)) numWorkOutFull <- mkRegU;
    
    Reg#(Bit#(48)) numGraphNodeReqFull <- mkRegU;
    Reg#(Bit#(48)) numGraphEdgeReqFull <- mkRegU;
    Reg#(Bit#(48)) numGraphCASReqFull <- mkRegU;
    
    Reg#(Bit#(48)) numNewDistFull <- mkRegU;
    Reg#(Bit#(48)) numGraphNode1Full <- mkRegU;
    Reg#(Bit#(48)) numGraphNode2Full <- mkRegU;
    
    Reg#(Bit#(48)) numCasCxt1Full <- mkRegU;
    Reg#(Bit#(48)) numCasCxt2Full <- mkRegU;
    Reg#(Bit#(48)) numCasCxtRetryFull <- mkRegU;
    Reg#(Bit#(48)) numCasCxtRetryStallFull <- mkRegU;

    //Reg#(Bit#(8)) entry_counter <- mkReg(0);
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
        if(fpgaId == 0 && laneId==1) begin
        let cycle <- cur_cycle;
            if(cycle > 109000 && cycle < 110000) $display("%0d: SSSPEngine[%0d][%0d] all fulls: workIn:%b workOut:%b graphsReqs:%b%b%b%b graphResps:%b%b%b%b graphNodeQ1:%b graphNodeQ2:%b newDist:%b casQ1:%b casQ2:%b",
                     cur_cycle, fpgaId, laneId,
                     !workInQ.notFull, !workOutQ.notFull, !graphNodeReqQs[0].notFull, !graphNodeReqQs[1].notFull, !graphEdgeReqQs[0].notFull, !graphCASReqQs[0].notFull,
               !graphNodeRespQs[0].notFull, !graphNodeRespQs[1].notFull, !graphEdgeRespQs[0].notFull, !graphCASRespQs[0].notFull,
               !graphNodeQ1.notFull, !graphNodeQ2.notFull, !newDistQ.notFull,
                     !casContextQ1.notFull, !casContextQ2.notFull);
            if(cycle > 109000 && cycle < 110000) $display("%0d: SSSPEngine[%0d][%0d] all empties: workIn:%b workOut:%b graphReqs:%b%b%b%b graphResps:%b%b%b%b graphNodeQ1:%b graphNodeQ2:%b newDist:%b casQ1:%b casQ2:%b",
                     cur_cycle, fpgaId, laneId,
                     !workInQ.notEmpty, !workOutQ.notEmpty, !graphNodeReqQs[0].notEmpty, !graphNodeReqQs[1].notEmpty, !graphEdgeReqQs[0].notEmpty, !graphCASReqQs[0].notEmpty,
               !graphNodeRespQs[0].notEmpty, !graphNodeRespQs[1].notEmpty, !graphEdgeRespQs[0].notEmpty, !graphCASRespQs[0].notEmpty,
               !graphNodeQ1.notEmpty, !graphNodeQ2.notEmpty, !newDistQ.notEmpty,
                     !casContextQ1.notEmpty, !casContextQ2.notEmpty);
            if(cycle > 109000 && cycle < 110000) $display("%0d: CAS num in flight max: %0d", cur_cycle, casNumInFlight.notMax);
            
        end
    endrule
*/    
    rule calcDone(started);
        Bool workEmpty = !workInQ.notEmpty && !workOutQ.notEmpty;
        
        Bool noNodesInFlight = (numWorkFetched == (numWorkRetired + numWorkDiscarded));
        Bool noEdgesInFlight = (numEdgesFetched == (numEdgesRetired + numEdgesDiscarded));
        //$display("%0d: SSSPEngine[%0d][%0d]: numEdgesInFlight: %0d", cur_cycle, fpgaId, laneId, numEdgesFetched-numEdgesRetired-numEdgesDiscarded);
        
        //if (`DEBUG) $display("%0d: SSSPEngine[%0d]: numWorkFetched: %0d, numWorkRetired: %0d, numEdgesFetched: %0d, numEdgesRetired: %0d, noEdgesDiscarded: %0d", cur_cycle, fpgaId, numWorkFetched, numWorkRetired, numEdgesFetched, numEdgesRetired, numEdgesDiscarded);
        if(workEmpty && noNodesInFlight && noEdgesInFlight) begin
            done <= True;
        end
        else begin
            done <= False;
        end
    endrule
    
    rule getSrcNode(started && graphNodeReqQs[0].notFull);
        if (workInQ.notEmpty && graphNodeQ0.notFull) begin
            WLEntry pkt = workInQ.first();
            workInQ.deq();
            WLJob job = tpl_2(pkt);
	    Bit#(32) pri = tpl_1(pkt);
	    Bit#(1) procId = pri[31];
            
            //$display("%0d: ~~~ SSSPEngine[%0d][%0d]: START getSrcNode priority: %0d, nodeID: %0d", cur_cycle, fpgaId, laneId, tpl_1(pkt), job);
            
            graphNodeReqQs[0].enq(GraphNodeReq{id: job, procId: procId});
	    graphNodeQ0.enq(procId);
            numWorkFetched <= numWorkFetched + 1;
        end
    endrule

    //Reg#(Bit#(64)) numStall <- mkReg(0);
    //rule prof(started);
    //    if (!graphNodeQ1.notEmpty)
    //        numStall <= numStall + 1;
    //    let cycle <- cur_cycle;
    //    if (cycle % 1024 == 0) $display("%0d: SSSPEngine[%0d][%0d] numStall %0d", cur_cycle, fpgaId, laneId, numStall);
    //endrule

    
    rule recvSrcNode;
        GraphNodeResp nodeResp = graphNodeRespQs[0].first();
	Bit#(1) procId = graphNodeQ0.first();
        graphNodeRespQs[0].deq();
	graphNodeQ0.deq();
        GraphTmp node = GraphTmp{node: nodeResp.node, procId: procId};
        if (node.node.numEdges == 0)
            numWorkDiscarded <= numWorkDiscarded + 1;
        else
            graphNodeQ1.enq(node);
        if(`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: graphNode ID %0d payload %0d edgePtr %0d numEdges %0d ", cur_cycle, fpgaId, laneId, node.node.id, node.node.payload, node.node.edgePtr, node.node.numEdges);
    endrule
    
    Reg#(NodeNumEdges) edgeIdx <- mkReg(0);
    rule getEdges;
        if (graphNodeQ1.notEmpty) begin
            GraphTmp node = graphNodeQ1.first();        
	    if (graphEdgeReqQs[0].notFull) begin
                EdgePtr edgeID = node.node.edgePtr + edgeIdx;
                graphEdgeReqQs[0].enq(GraphEdgeReq{id: edgeID});
                graphNodeQ2.enq(node);
                if(`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: getEdges %0d of %0d", cur_cycle, fpgaId, laneId, edgeIdx, node.node.numEdges-1);
                numEdgesFetched <= numEdgesFetched + 1;
                
                if(edgeIdx == (node.node.numEdges - 1)) begin
                    edgeIdx <= 0;
                    graphNodeQ1.deq();
                    //entry_counter <= entry_counter - 1;
                    numWorkRetired <= numWorkRetired + 1;
                end
                else begin
                    edgeIdx <= edgeIdx + 1;
                end
	    end
	    else
	        numEdgePipeStall <= numEdgePipeStall + 1;
	end
	else begin
	    numWorklistStall <= numWorklistStall + 1;
	    let cycle <- cur_cycle;
	    //if (cycle % 2048 == 0) $display("%0d: mkSSSPEngine[%0d][%0d] stalled by worklist for %0d cycles", cur_cycle, fpgaId, laneId, numWorklistStall);
	end
    endrule

    
    rule getDestNode;
        GraphEdgeResp edgeResp = graphEdgeRespQs[0].first();
        graphEdgeRespQs[0].deq();
        
        GraphTmp n = graphNodeQ2.first();
        graphNodeQ2.deq();
        
        GraphEdge e = edgeResp.gedge;
        NodePayload newDist = n.node.payload + e.weight;
        newDistQ.enq(newDist);
	newDistProcQ.enq(GraphTmp2{id: e.dest, procId: n.procId});

        if(`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: getDestNode num %0d", cur_cycle, fpgaId, laneId, e.dest);
        graphPartialNodeReqQs[0].enq(GraphPartialNodeReq{id: e.dest, procId: n.procId, data: ?});
    endrule

    Reg#(Tuple3#(NodePayload, NodePayload, GraphTmp3)) casRetryPkt <- mkRegU;
    Reg#(Bit#(8)) casRetryStall <- mkReg(0);
    Reg#(Bool) casRetryWait <- mkReg(True);
    /*
    rule casRetry;
        casContextRetryStallQ.deq();
        casContextQ1.enq(casContextRetryQ.first);
        casContextRetryQ.deq;
    endrule
    */
    
    rule casRetry(!casRetryWait && started);
        let cycle <- cur_cycle;
        //if(cycle > 109000 && cycle < 110000) $display("%0d: retry", cur_cycle);
        if(casRetryStall == 0) begin
            casContextQ1.enq(casRetryPkt);
            casRetryWait <= True;
            //$display("%0d: SSSPEngine[%0d][%0d]: enqueueing retry packet %0x", cur_cycle, fpgaId, laneId, casRetryPkt);
        end
        else begin
            casRetryStall <= casRetryStall - 1;
        end
    endrule
    
    rule casRetryDeq(casRetryWait);
        casRetryWait <= False;
        casRetryPkt <= casContextRetryQ.first();
        casContextRetryQ.deq();
        casRetryStall <= casContextRetryStallQ.first();
        casContextRetryStallQ.deq();
        //$display("%0d: SSSPEngine[%0d][%0d]: init cas stall %0d", cur_cycle, fpgaId, laneId, casContextRetryStallQ.first());    
    endrule
      
    rule recvDestNode(casNumInFlight.notMax);
        GraphPartialNodeResp nodeResp = graphPartialNodeRespQs[0].first();
        graphPartialNodeRespQs[0].deq();
        
        NodePayload newDist = newDistQ.first();
        newDistQ.deq();
	GraphTmp2 node = newDistProcQ.first();
	newDistProcQ.deq();
        let cycle <- cur_cycle;
        //if(cycle > 109000 && cycle < 110000) $display("%0d: SSSPEngine[%0d][%0d]: recvDestNode, enqueueing CAS: %0d < %0d?", cur_cycle, fpgaId, laneId, newDist, node.payload);
        // tuple3(cmpVal, newVal, destNode)
        casContextQ1.enq(tuple3(nodeResp.data1, newDist, GraphTmp3{id: node.id, numEdges: nodeResp.data2, procId: node.procId}));
        casNumInFlight.inc();
    endrule
    
    rule cas;
        // tuple3(cmpVal, newVal, destNode)
        Tuple3#(NodePayload, NodePayload, GraphTmp3) cxt = casContextQ1.first();
        casContextQ1.deq();
        if(`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: Attempting CAS...", cur_cycle, fpgaId, laneId);
        if(tpl_2(cxt) < tpl_1(cxt)) begin
            //$display("%0d:  %d < %d, executing CAS!", cur_cycle, tpl_2(cxt), tpl_1(cxt));
            graphCASReqQs[0].enq(GraphCASReq{id: tpl_3(cxt).id, cmpVal: tpl_1(cxt), swapVal: tpl_2(cxt), procId: tpl_3(cxt).procId});
            casContextQ2.enq(cxt);
            numCASIssued <= numCASIssued + 1;
        end
        else begin
            //$display("%0d:  straightly retire");
            casNumInFlight.dec();
            numEdgesDiscarded <= numEdgesDiscarded + 1;
        end
    endrule
    
    rule casDone;
        GraphCASResp casResp = graphCASRespQs[0].first();
        graphCASRespQs[0].deq();
        
        // tuple3(cmpVal, newVal, destNode)
        Tuple3#(NodePayload, NodePayload, GraphTmp3) cxt = casContextQ2.first();
        casContextQ2.deq();
	rg_totalBias <= truncate(rg_offset << (rg_bSize + 3)) + rg_bias;
        if(casResp.success) begin
            GraphTmp3 node = tpl_3(cxt);
	    if (node.numEdges > 0) begin
                Bit#(32) distance = (tpl_2(cxt) > rg_totalBias) ? (tpl_2(cxt) - rg_totalBias) : 0;
                Bit#(32) distance_2 = distance >> rg_bSize;
                Bit#(32) pri = (distance_2 > 7) ? 7 : distance_2;
		pri[31] = node.procId;
                //$display("%0d: engine[%0d][%0d] generate node %0d with distance %0d, pri %0d, off %0d", cur_cycle, fpgaId, laneId, node.id, tpl_2(cxt), pri, rg_offset);
                WLEntry newWork = tuple2(pri, node.id);
                workOutQ.enq(newWork);
	    end
            //$display("%0d: SSSPEngine[%0d][%0d]: CAS Success! Num retired: %0d, num discarded: %0d. Enqueueing new work item: ", cur_cycle, fpgaId, laneId, numEdgesRetired+1, numEdgesDiscarded, fshow(newWork));
                //if(numEdgesDiscarded % 1024 == 0) begin
                    //$display("%0d: SSSPEngine[%0d][%0d]: CAS Success! Edges retired: %0d, edges discarded: %0d, CAS issued: %0d, CAS retried: %0d. Enqueueing new work item: ", cur_cycle, fpgaId, laneId, numEdgesRetired+1, numEdgesDiscarded, numCASIssued, numCASRetried, fshow(newWork));
                //end
            //end
            numEdgesRetired <= numEdgesRetired + 1;
            casNumInFlight.dec2();
        end
        else if (tpl_2(cxt) < casResp.oldVal) begin
            casContextRetryQ.enq(tuple3(casResp.oldVal, tpl_2(cxt), tpl_3(cxt)));
            let stallVal = lfsr.value() >> 4;
            casContextRetryStallQ.enq(stallVal);
            lfsr.next();
            numCASRetried <= numCASRetried + 1;
            //$display("%0d: SSSPEngine[%0d][%0d]: CAS failed, retry... set casStall = %0d", cur_cycle, fpgaId, laneId, stallVal);
        end
	else begin
	    numEdgesRetired <= numEdgesRetired + 1;
            casNumInFlight.dec2();
	end
    endrule
    
    /*
    rule monitorWL;
        if(!workInQ.notEmpty)
            numWorkInEmpty <= numWorkInEmpty + 1;
        if(!workOutQ.notFull)
            numWorkOutFull <= numWorkOutFull + 1;
        if(!graphNodeReqQs[1].notFull)
            numGraphNodeReqFull <= numGraphNodeReqFull + 1;
        if(!graphEdgeReqQs[0].notFull)
            numGraphEdgeReqFull <= numGraphEdgeReqFull + 1;
        if(!graphCASReqQs[0].notFull)
            numGraphCASReqFull <= numGraphCASReqFull + 1;
        if(!newDistQ.notFull)
            numNewDistFull <= numNewDistFull + 1;
        if(!graphNodeQ1.notFull)
            numGraphNode1Full <= numGraphNode1Full + 1;
        if(!graphNodeQ2.notFull)
            numGraphNode2Full <= numGraphNode2Full + 1;
        if(!casContextQ1.notFull)
            numCasCxt1Full <= numCasCxt1Full + 1;
        if(!casContextQ2.notFull)
            numCasCxt2Full <= numCasCxt2Full + 1;
        if(!casContextRetryQ.notFull)
            numCasCxtRetryFull <= numCasCxtRetryFull + 1;
        if(!casContextRetryStallQ.notFull)
            numCasCxtRetryStallFull <= numCasCxtRetryStallFull + 1;
    endrule
    */
    
    //rule printStats(started);
    //    let cycle <- cur_cycle;
    //    if(cycle % 8192 == 0) begin
            //$display("%0d: SSSPEngine[%0d][%0d]: Edges retired: %0d, Edges discarded: %0d, CAS issued: %0d, CAS retried: %0d. Nodes in flight: %0d, Edges in flight: %0d", cycle, fpgaId, laneId, numEdgesRetired, numEdgesDiscarded, numCASIssued, numCASRetried, (numWorkFetched - numWorkRetired), (numEdgesFetched-numEdgesRetired-numEdgesDiscarded));
            //$display("%0d: SSSPEngine[%0d][%0d]: Edges retired: %0d, Edges discarded: %0d, CAS issued: %0d, CAS retried: %0d, workInEmpty: %0d, workOutFull: %0d, nodeFull: %0d, edgeFull: %0d, casFull: %0d, newDistFull: %0d, graphNode1Full: %0d, graphNode2Full: %0d, casCxt1Full: %0d, casCxt2Full: %0d, casRetryFull: %0d, casRetryStallFull: %0d, Edges in flight: %0d", cycle, fpgaId, laneId, numEdgesRetired, numEdgesDiscarded, numCASIssued, numCASRetried, numWorkInEmpty, numWorkOutFull, numGraphNodeReqFull, numGraphEdgeReqFull, numGraphCASReqFull, numNewDistFull, numGraphNode1Full, numGraphNode2Full, numCasCxt1Full, numCasCxt2Full, numCasCxtRetryFull, numCasCxtRetryStallFull, (numEdgesFetched-numEdgesRetired-numEdgesDiscarded));
    //    end
    //endrule
    
    method Action init(BC_AEId fpgaid, Bit#(4) laneid, Bit#(5) bSize, Bit#(32) bias);
        fpgaId <= fpgaid;
        laneId <= laneid;
        started <= True;
        done <= False;
        rg_bSize <= bSize;
        rg_bias <= bias;
	rg_totalBias <= bias;
	rg_offset <= 0;
        $display("bSize is %0d", bSize);
        
        lfsr.seed({2'b0, fpgaid, laneid});
        casNumInFlight.init(`SSSPENGINE_NUM_IN_FLIGHT);
        
        numWorkFetched <= 0;
        numWorkRetired <= 0;
        numWorkDiscarded <= 0;
        numEdgesFetched <= 0;
        numEdgesRetired <= 0;
        numEdgesDiscarded <= 0;
	numWorklistStall <= 0;
	numEdgePipeStall <= 0;
        
        numCASIssued  <= 0;
        numCASRetried <= 0;
        
        numWorkInEmpty <= 0;
        numWorkOutFull <= 0;
        
        numGraphNodeReqFull <= 0;
        numGraphCASReqFull <= 0;
        numGraphEdgeReqFull <= 0;

        numNewDistFull <= 0;
        numGraphNode1Full <= 0;
        numGraphNode2Full <= 0;
        numCasCxt1Full <= 0;
        numCasCxt2Full <= 0;
        numCasCxtRetryFull <= 0;
        numCasCxtRetryStallFull <= 0;

        //entry_counter <= 0;

    endmethod
    
    method ActionValue#(Bit#(64)) result();
        return extend(numEdgesFetched);
    endmethod

    method ActionValue#(Bit#(64)) numNodes();
        return extend(numWorkFetched);
    endmethod

    method ActionValue#(Bit#(64)) numCASRetry();
        return numCASRetried;
    endmethod
    
    method ActionValue#(Bit#(64)) numWLStall();
        return numWorklistStall;
    endmethod

    method ActionValue#(Bit#(64)) numEPStall();
        return numEdgePipeStall;
    endmethod
    
    method Bool isDone;
        return done;
    endmethod

    method Action stop;
        started <= False;
    endmethod
    
    method Action setOffset(Bit#(64) offset);
        rg_offset <= offset;
    endmethod

    interface workIn = toPut(workInQ);
    interface workOut = toGet(workOutQ);
    interface graphNodeReqs = map(toGet, graphNodeReqQs);
    interface graphPartialNodeReqs = map(toGet, graphPartialNodeReqQs);
    interface graphEdgeReqs = map(toGet, graphEdgeReqQs);
    interface graphCASReqs = map(toGet, graphCASReqQs);
    interface graphNodeResps = map(toPut, graphNodeRespQs);
    interface graphPartialNodeResps = map(toPut, graphPartialNodeRespQs);
    interface graphEdgeResps = map(toPut, graphEdgeRespQs);
    interface graphCASResps = map(toPut, graphCASRespQs);
endmodule

endpackage
