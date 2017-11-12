
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
import FloatingPoint	:: *;
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
    method Action init(BC_AEId fpgaId, Bit#(4) laneId, Bit#(32) tolerance);
    method ActionValue#(Bit#(64)) result;
    method ActionValue#(Bit#(64)) numCASRetry();
    method ActionValue#(Bit#(64)) numWLStall();
    method ActionValue#(Bit#(64)) numEPStall();
    method ActionValue#(Bit#(64)) numNodes();
    method Action stop;
    method Bool isDone;
    
    interface Put#(WLEntry) workIn;
    interface Get#(WLEntry) workOut;

    interface Vector#(1, Get#(GraphNodeReq)) graphNodeReqs;
    interface Vector#(2, Get#(GraphPartialNodeReq)) graphPartialNodeReqs;
    interface Vector#(1, Get#(GraphEdgeReq)) graphEdgeReqs;
    interface Vector#(1, Get#(GraphCASReq)) graphCASReqs;
    interface Vector#(1, Get#(GraphEXCHReq)) graphEXCHReqs;
    interface Vector#(1, Put#(GraphNodeResp)) graphNodeResps;
    interface Vector#(2, Put#(GraphPartialNodeResp)) graphPartialNodeResps;
    interface Vector#(1, Put#(GraphEdgeResp)) graphEdgeResps;
    interface Vector#(1, Put#(GraphCASResp)) graphCASResps;
    interface Vector#(1, Put#(GraphEXCHResp)) graphEXCHResps;
endinterface

//(* synthesize, descending_urgency = "casDone, cas, recvDestNode, casRetry, getDestNode, getEdges, recvSrcNode, getSrcNode" *)
(* synthesize, descending_urgency = "casRetry, recvDestNode, recvSrcNode" *)
module mkSSSPEngine(Engine ifc);
    Reg#(BC_AEId) fpgaId <-mkRegU;
    Reg#(Bit#(4)) laneId <- mkRegU;
    Reg#(Float) rg_tolerance <- mkRegU;
    Reg#(Bool) started <- mkReg(False);
    Reg#(Bool) done <- mkReg(True);
    
    FIFOF#(WLEntry) workInQ <- mkFIFOF;
    FIFOF#(WLEntry) workOutQ <- mkFIFOF;
    
    Vector#(1, FIFOF#(GraphNodeReq)) graphNodeReqQs <- replicateM(mkFIFOF);
    Vector#(2, FIFOF#(GraphPartialNodeReq)) graphPartialNodeReqQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphEdgeReq)) graphEdgeReqQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphCASReq)) graphCASReqQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphEXCHReq)) graphEXCHReqQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphNodeResp)) graphNodeRespQs <- replicateM(mkFIFOF);
    Vector#(2, FIFOF#(GraphPartialNodeResp)) graphPartialNodeRespQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphEdgeResp)) graphEdgeRespQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphCASResp)) graphCASRespQs <- replicateM(mkFIFOF);
    Vector#(1, FIFOF#(GraphEXCHResp)) graphEXCHRespQs <- replicateM(mkFIFOF);
    
    FIFOF#(GraphNode) graphNodeQ0 <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT); // # nodes to fetch ahead of edges
    FIFOF#(GraphNode) graphNodeQ1 <- mkSizedBufBRAMFIFOF(2*`GRAPH_NUM_IN_FLIGHT); // # nodes to fetch ahead of edges
    FIFOF#(GraphNode) graphNodeQ2 <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT);  // # entries = # edgeReq in flight
    FIFOF#(GraphNode) graphNodeQ2_0 <- mkSizedFIFOF(8);  // # entries = # Floating point adds in flight
    FIFOF#(GraphNode) graphNodeQ3 <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT);  // # entries = # edgeReq in flight
    FIFOF#(NodeID) graphNodeQ4 <- mkSizedBufBRAMFIFOF(`GRAPH_NUM_IN_FLIGHT);  // # entries = # edgeReq in flight
    FIFOF#(NodeID) graphNodeQ4_0 <- mkSizedFIFOF(8);  // # entries = # edgeReq in flight
    
    FIFOF#(Float) resIncQ <- mkSizedBufBRAMFIFOF(`SSSPENGINE_NUM_IN_FLIGHT);   // # entries = # destNode in flight
    FIFOF#(Tuple3#(NodeID, Float, Float)) casContextQ1 <- mkSizedFIFOF(4);
    FIFOF#(Tuple3#(NodeID, Float, Float)) casContextQ2 <- mkSizedBufBRAMFIFOF(`SSSPENGINE_NUM_IN_FLIGHT); // # entries = # CAS requests in flight
    FIFOF#(Tuple3#(NodeID, Float, Float)) casContextQ2_0 <- mkSizedFIFOF(8); // # entries = # CAS requests in flight
    FIFOF#(Tuple3#(NodeID, Float, Float)) casContextRetryQ <- mkSizedBufBRAMFIFOF(`SSSPENGINE_NUM_CAS_RETRY_IN_FLIGHT);
    FIFOF#(Bit#(8)) casContextRetryStallQ <- mkSizedBufBRAMFIFOF(`SSSPENGINE_NUM_CAS_RETRY_IN_FLIGHT);

    FIFOF#(GraphNode) casNodeContextRetryQ <- mkSizedBufBRAMFIFOF(`SSSPENGINE_NUM_CAS_RETRY_IN_FLIGHT);
    FIFOF#(Bit#(8)) casNodeContextRetryStallQ <- mkSizedBufBRAMFIFOF(`SSSPENGINE_NUM_CAS_RETRY_IN_FLIGHT);
    CoalescingCounter casNumInFlight <- mkCCounter();

    Server#(Tuple3#(Float, Float, RoundMode), Tuple2#(Float, Exception)) fpAdder <- mkFloatingPointAdder();
    Server#(Tuple3#(Float, Float, RoundMode), Tuple2#(Float, Exception)) fpAdder2 <- mkFloatingPointAdder();
    Server#(Tuple3#(Float, Float, RoundMode), Tuple2#(Float, Exception)) fpMultiplier <- mkFloatingPointMultiplier();
    
    LFSR#(Bit#(8)) lfsr <- mkLFSR_8;
    LFSR#(Bit#(8)) lfsr2 <- mkLFSR_8;
    
    Reg#(Bit#(48)) numWorkFetched <- mkRegU;
    Reg#(Bit#(48)) numWorkRetired <- mkRegU;
    Reg#(Bit#(48)) numWorkDiscarded <- mkRegU;
    Reg#(Bit#(48)) numEdgesFetched <- mkRegU;
    Reg#(Bit#(48)) numEdgesRetired <- mkRegU;
    Reg#(Bit#(64)) numWorklistStall <- mkRegU;
    Reg#(Bit#(64)) numEdgePipeStall <- mkRegU;
    Reg#(Bit#(48)) numEdgesDiscarded <- mkRegU;
    Reg#(Bit#(48)) numCASRetried <- mkRegU;
    
    Reg#(Bit#(48)) numCASIssued <- mkRegU;
    
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

	let cycle <- cur_cycle;
        
        if (cycle % 16384 == 0) $display("%0d: SSSPEngine[%0d][%0d]: numWorkFetched: %0d, numWorkRetired: %0d, numWorkDiscarded: %0d, numEdgesFetched: %0d, numEdgesRetired: %0d, noEdgesDiscarded: %0d", cur_cycle, fpgaId, laneId, numWorkFetched, numWorkRetired, numWorkDiscarded, numEdgesFetched, numEdgesRetired, numEdgesDiscarded);
        if(workEmpty && noNodesInFlight && noEdgesInFlight) begin
            done <= True;
        end
        else begin
            done <= False;
        end
    endrule
    
    rule getSrcNode(started && graphNodeReqQs[0].notFull);
        if (workInQ.notEmpty) begin
            WLEntry pkt = workInQ.first();
            workInQ.deq();
            WLJob job = tpl_2(pkt);
            
            if(`DEBUG) $display("%0d: ~~~ SSSPEngine[%0d]: START getSrcNode priority: %0d, nodeID: %0d", cur_cycle, fpgaId, tpl_1(pkt), job);
            
            graphNodeReqQs[0].enq(GraphNodeReq{id: job});
            numWorkFetched <= numWorkFetched + 1;
	end
    endrule

    Reg#(GraphNode) casNodeRetryPkt <- mkRegU;
    Reg#(Bit#(8)) casNodeRetryStall <- mkReg(0);
    Reg#(Bool) casNodeRetryWait <- mkReg(True);

    rule recvSrcNode;
        GraphNodeResp nodeResp = graphNodeRespQs[0].first();
        graphNodeRespQs[0].deq();
        GraphNode node = nodeResp.node;
        if (node.residual == 0) begin
            numWorkDiscarded <= numWorkDiscarded + 1;
	    //if (fpgaId == 0) $display("work discarded1 %0d", numWorkDiscarded.getVal());
        end
        else begin
            graphNodeQ0.enq(node);
	end
        if(`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: graphNode ID %0d payload %0d edgePtr %0d numEdges %0d residual %h numEdges_inv %h", cur_cycle, fpgaId, laneId, node.id, node.payload, node.edgePtr, node.numEdges, node.residual, node.numEdges_inv);
    endrule

    rule updateRes;
        GraphNode node = graphNodeQ0.first();
	graphNodeQ0.deq();
        graphEXCHReqQs[0].enq(GraphEXCHReq{id: node.id, swapVal: 0});
	graphNodeQ1.enq(node);
    endrule

    rule updatePR1;
        GraphEXCHResp exchResp = graphEXCHRespQs[0].first();
        graphEXCHRespQs[0].deq();

	GraphNode node = graphNodeQ1.first();
	graphNodeQ1.deq();

	Float pr = unpack(node.payload);
	Float residual = unpack(exchResp.oldVal);
	//Float new_pr = pr + residual;

	fpAdder.request.put(tuple3(pr, residual, Rnd_Nearest_Even));
	graphNodeQ2_0.enq(node);
	if (`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: graphNode ID %0d residual got, pr: %h, res: %h", cur_cycle, fpgaId, laneId, node.id, node.payload, exchResp.oldVal);
    endrule

    rule updatePR2;
        let new_pr <- fpAdder.response.get();
	GraphNode node = graphNodeQ2_0.first();
	graphNodeQ2_0.deq();
	graphPartialNodeReqQs[0].enq(GraphPartialNodeReq{id: node.id, op: 1, data: {node.id, pack(tpl_1(new_pr))}});
	if (`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: new pagerank: %h", cur_cycle, fpgaId, laneId, tpl_1(new_pr));
	graphNodeQ2.enq(node);
    endrule

    Reg#(NodeNumEdges) edgeIdx <- mkReg(0);
    rule getEdges;
        if (graphNodeQ2.notEmpty) begin
            GraphNode node = graphNodeQ2.first();        
	    if (node.numEdges == 0) begin
	        graphNodeQ2.deq();
                graphPartialNodeRespQs[0].deq();
	        numWorkRetired <= numWorkRetired + 1;
	        //if (fpgaId == 0) $display("work retired %0d", numWorkRetired);
	    end
	    else if (graphEdgeReqQs[0].notFull) begin
                EdgePtr edgeID = node.edgePtr + edgeIdx;
                graphEdgeReqQs[0].enq(GraphEdgeReq{id: edgeID});
                graphNodeQ3.enq(node);
                if(`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: node %0d: getEdges %0d of %0d", cur_cycle, fpgaId, laneId, node.id, edgeIdx, node.numEdges-1);
                numEdgesFetched <= numEdgesFetched + 1;
                
                if(edgeIdx == (node.numEdges - 1)) begin
                    edgeIdx <= 0;
                    graphNodeQ2.deq();
                    graphPartialNodeRespQs[0].deq();
                    //entry_counter <= entry_counter - 1;
                    numWorkRetired <= numWorkRetired + 1;
	            //if (fpgaId == 0) $display("work retired %0d", numWorkRetired);
                end
                else begin
                    edgeIdx <= edgeIdx + 1;
                end
	    end
	    else
	        numEdgePipeStall <= numEdgePipeStall + 1;
	end
	else
	    numWorklistStall <= numWorklistStall + 1;
    endrule
    
    rule getDestNode;
        GraphEdgeResp edgeResp = graphEdgeRespQs[0].first();
        graphEdgeRespQs[0].deq();
        
        GraphNode n = graphNodeQ3.first();
        graphNodeQ3.deq();
        
        GraphEdge e = edgeResp.gedge;
        Float numEdges_inv = unpack(n.numEdges_inv);
	Float residual = unpack(n.residual);
	//Float resInc = residual * numEdges_inv;
	fpMultiplier.request.put(tuple3(residual, numEdges_inv, Rnd_Nearest_Even));
	graphNodeQ4_0.enq(e.dest);
    endrule

    rule getDestNode2;
        NodeID id = graphNodeQ4_0.first();
        graphNodeQ4_0.deq();
	let resInc <- fpMultiplier.response.get();
	if (`DEBUG) $display("residual Increment is %h", tpl_1(resInc));
        resIncQ.enq(tpl_1(resInc));
        if(`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: getDestNode num %0d", cur_cycle, fpgaId, laneId, id);
        graphPartialNodeReqQs[1].enq(GraphPartialNodeReq{id: id, op: 0, data: ?});
	graphNodeQ4.enq(id);
    endrule

    Reg#(Tuple3#(NodeID, Float, Float)) casRetryPkt <- mkRegU;
    Reg#(Bit#(8)) casRetryStall <- mkReg(0);
    Reg#(Bool) casRetryWait <- mkReg(True);
    /*
    rule casRetry;
        casContextRetryStallQ.deq();
        casContextQ1.enq(casContextRetryQ.first);
        casContextRetryQ.deq;
    endrule
    */
    
    //rule casRetry(!casRetryWait && started);
    rule casRetry(started);
        //let cycle <- cur_cycle;
        //if(cycle > 109000 && cycle < 110000) $display("%0d: retry", cur_cycle);
        //if(casRetryStall == 0) begin
        Tuple3#(NodeID, Float, Float) casRetryPkt = casContextRetryQ.first();
	casContextRetryQ.deq();
            casContextQ1.enq(casRetryPkt);
            //casRetryWait <= True;
            if (`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: enqueueing retry packet %0x", cur_cycle, fpgaId, laneId, casRetryPkt);
        //end
        //else begin
        //    casRetryStall <= casRetryStall - 1;
        //end
    endrule
    
    //rule casRetryDeq(casRetryWait);
    //    casRetryWait <= False;
    //    casRetryPkt <= casContextRetryQ.first();
    //    casContextRetryQ.deq();
    //    casRetryStall <= casContextRetryStallQ.first();
    //    casContextRetryStallQ.deq();
    //    //$display("%0d: SSSPEngine[%0d][%0d]: init cas stall %0d", cur_cycle, fpgaId, laneId, casContextRetryStallQ.first());    
    //endrule
      
    rule recvDestNode(casNumInFlight.notMax);
        GraphPartialNodeResp nodeResp = graphPartialNodeRespQs[1].first();
        graphPartialNodeRespQs[1].deq();

	NodeID id = graphNodeQ4.first();
	graphNodeQ4.deq();
        
	Float residual = unpack(nodeResp.data2);
        Float resInc = resIncQ.first();
        resIncQ.deq();
        let cycle <- cur_cycle;
        //if(cycle > 109000 && cycle < 110000) $display("%0d: SSSPEngine[%0d][%0d]: recvDestNode, enqueueing CAS: %0d < %0d?", cur_cycle, fpgaId, laneId, newDist, node.payload);
        // tuple3(cmpVal, newVal, destNode)
	if (`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: Dest node %0d received, residual_dest is %h", cur_cycle, fpgaId, laneId, id, nodeResp.data2);
        casContextQ1.enq(tuple3(id, residual, resInc));
        casNumInFlight.inc();
    endrule
    
    rule cas;
        // tuple3(cmpVal, newVal, destNode)
        Tuple3#(NodeID, Float, Float) cxt = casContextQ1.first();
        casContextQ1.deq();
	fpAdder2.request.put(tuple3(tpl_2(cxt), tpl_3(cxt), Rnd_Nearest_Even));

	casContextQ2_0.enq(cxt);
    endrule

    rule cas2;
        Tuple3#(NodeID, Float, Float) cxt = casContextQ2_0.first();
        casContextQ2_0.deq();
	let new_residual <- fpAdder2.response.get();
	//Float new_residual = tpl_2(cxt) + tpl_3(cxt);
	if (`DEBUG) $display("new residual is %h", tpl_1(new_residual));
        if(`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: Attempting CAS...", cur_cycle, fpgaId, laneId);
        //$display("%0d:  %d < %d, executing CAS!", cur_cycle, tpl_2(cxt), tpl_1(cxt));
        graphCASReqQs[0].enq(GraphCASReq{id: tpl_1(cxt), cmpVal: pack(tpl_2(cxt)), swapVal: pack(tpl_1(new_residual))});
        casContextQ2.enq(cxt);
        numCASIssued <= numCASIssued + 1;
    endrule
    
    rule casDone;
        GraphCASResp casResp = graphCASRespQs[0].first();
        graphCASRespQs[0].deq();
        
        // tuple3(cmpVal, newVal, destNode)
        Tuple3#(NodeID, Float, Float) cxt = casContextQ2.first();
        casContextQ2.deq();
        
        if(casResp.success) begin
	    Disorder cmp1 = compareFP(tpl_2(cxt), rg_tolerance);
	    Disorder cmp2 = compareFP(tpl_3(cxt), rg_tolerance);
	    if ((cmp1 == LT) && ((cmp2 == GT) || (cmp2 == EQ))) begin
                NodeID id = tpl_1(cxt);
                WLEntry newWork = tuple2(0, id);
                workOutQ.enq(newWork);
		if (`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: CAS success, add new work item %0d", cur_cycle, fpgaId, laneId, id);
	    end
            //$display("%0d: SSSPEngine[%0d][%0d]: CAS Success! Num retired: %0d, num discarded: %0d. Enqueueing new work item: ", cur_cycle, fpgaId, laneId, numEdgesRetired+1, numEdgesDiscarded, fshow(newWork));
                //if(numEdgesDiscarded % 1024 == 0) begin
                    //$display("%0d: SSSPEngine[%0d][%0d]: CAS Success! Edges retired: %0d, edges discarded: %0d, CAS issued: %0d, CAS retried: %0d. Enqueueing new work item: ", cur_cycle, fpgaId, laneId, numEdgesRetired+1, numEdgesDiscarded, numCASIssued, numCASRetried, fshow(newWork));
                //end
            //end
            numEdgesRetired <= numEdgesRetired + 1;
            casNumInFlight.dec();
        end
        else begin
            casContextRetryQ.enq(tuple3(tpl_1(cxt), unpack(casResp.oldVal), tpl_3(cxt)));
            //let stallVal = lfsr.value() >> 4;
            //casContextRetryStallQ.enq(stallVal);
            //lfsr.next();
            numCASRetried <= numCASRetried + 1;
            //if (`DEBUG) $display("%0d: SSSPEngine[%0d][%0d]: CAS failed, retry... set casStall = %0d", cur_cycle, fpgaId, laneId, stallVal);
        end
    endrule

    /* 
    rule monitorWL;
        if(!workInQ.notEmpty)
            numWorkInEmpty <= numWorkInEmpty + 1;
        if(!workOutQ.notFull)
            numWorkOutFull <= numWorkOutFull + 1;
        if(!graphNodeReqQs[0].notFull)
            numGraphNodeReqFull <= numGraphNodeReqFull + 1;
        if(!graphEdgeReqQs[0].notFull)
            numGraphEdgeReqFull <= numGraphEdgeReqFull + 1;
        if(!graphCASReqQs[0].notFull)
            numGraphCASReqFull <= numGraphCASReqFull + 1;
        if(!resIncQ.notFull)
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

    Reg#(Bit#(48)) numGraphNodeQ0Full <- mkRegU;
    Reg#(Bit#(48)) numGraphNodeQ1Full <- mkRegU;
    Reg#(Bit#(48)) numGraphNodeQ2_0Full <- mkRegU;
    Reg#(Bit#(48)) numGraphNodeQ2Full <- mkRegU;
    Reg#(Bit#(48)) numGraphNodeQ3Full <- mkRegU;
    Reg#(Bit#(48)) numGraphNodeQ4_0Full <- mkRegU;
    Reg#(Bit#(48)) numGraphNodeQ4Full <- mkRegU;
    Reg#(Bit#(48)) numResIncQFull <- mkRegU;
    Reg#(Bit#(48)) numCasCxt2_0Full <- mkRegU;
    Reg#(Bit#(48)) numGraphEXCHQFull <- mkRegU;
    rule monitorWL;
        if(!workInQ.notEmpty)
            numWorkInEmpty <= numWorkInEmpty + 1;
        if(!workOutQ.notFull)
            numWorkOutFull <= numWorkOutFull + 1;
        if(!graphNodeQ0.notFull)
            numGraphNodeQ0Full <= numGraphNodeQ0Full + 1;
	if (!graphEXCHReqQs[0].notFull)
	    numGraphEXCHQFull <= numGraphEXCHQFull + 1;
	if (!graphEdgeReqQs[0].notFull)
            numGraphEdgeReqFull <= numGraphEdgeReqFull + 1;
        if(!graphNodeQ1.notFull)
            numGraphNodeQ1Full <= numGraphNodeQ1Full + 1;
        if(!graphNodeQ2_0.notFull)
            numGraphNodeQ2_0Full <= numGraphNodeQ2_0Full + 1;
        if(!graphNodeQ2.notFull)
            numGraphNodeQ2Full <= numGraphNodeQ2Full + 1;
        if(!graphNodeQ3.notFull)
            numGraphNodeQ3Full <= numGraphNodeQ3Full + 1;
        if(!graphNodeQ4_0.notFull)
            numGraphNodeQ4_0Full <= numGraphNodeQ4_0Full + 1;
        if(!graphNodeQ4.notFull)
            numGraphNodeQ4Full <= numGraphNodeQ4Full + 1;
        if(!resIncQ.notFull)
            numResIncQFull <= numResIncQFull + 1;
        if(!casContextQ1.notFull)
            numCasCxt1Full <= numCasCxt1Full + 1;
        if(!casContextQ2_0.notFull)
            numCasCxt2_0Full <= numCasCxt2_0Full + 1;
        if(!casContextQ2.notFull)
            numCasCxt2Full <= numCasCxt2Full + 1;
        if(!casContextRetryQ.notFull)
            numCasCxtRetryFull <= numCasCxtRetryFull + 1;
    endrule

    
    rule printStats(started);
        let cycle <- cur_cycle;
        if(cycle % 16384 == 0) begin
            //$display("%0d: SSSPEngine[%0d][%0d]: Edges retired: %0d, Edges discarded: %0d, CAS issued: %0d, CAS retried: %0d. Nodes in flight: %0d, Edges in flight: %0d", cycle, fpgaId, laneId, numEdgesRetired, numEdgesDiscarded, numCASIssued, numCASRetried, (numWorkFetched - numWorkRetired), (numEdgesFetched-numEdgesRetired-numEdgesDiscarded));
            //$display("%0d: SSSPEngine[%0d][%0d]: Edges retired: %0d, Edges discarded: %0d, CAS issued: %0d, CAS retried: %0d, workInEmpty: %0d, workOutFull: %0d, nodeFull: %0d, edgeFull: %0d, casFull: %0d, newDistFull: %0d, graphNode1Full: %0d, graphNode2Full: %0d, casCxt1Full: %0d, casCxt2Full: %0d, casRetryFull: %0d, casRetryStallFull: %0d, Edges in flight: %0d", cycle, fpgaId, laneId, numEdgesRetired, numEdgesDiscarded, numCASIssued, numCASRetried, numWorkInEmpty, numWorkOutFull, numGraphNodeReqFull, numGraphEdgeReqFull, numGraphCASReqFull, numNewDistFull, numGraphNode1Full, numGraphNode2Full, numCasCxt1Full, numCasCxt2Full, numCasCxtRetryFull, numCasCxtRetryStallFull, (numEdgesFetched-numEdgesRetired-numEdgesDiscarded));
	    $display("%0d: SSSPEngine[%0d][%0d]: WorkIn: %0d, WorkOut: %0d, GraphNodeQ0: %0d, GraphEXCHReq: %0d, GraphNodeQ1: %0d, GraphNodeQ2_0: %0d, GraphNodeQ2: %0d, GraphEdgeReq: %0d, GraphNodeQ3: %0d, GraphNodeQ4_0: %0d, GraphNodeQ4: %0d, ResIncQ: %0d, casCxt1: %0d, casCxt2_0: %0d, casCxt2: %0d, casCxtRetry: %0d", cur_cycle, fpgaId, laneId, numWorkInEmpty, numWorkOutFull, numGraphNodeQ0Full, numGraphEXCHQFull, numGraphNodeQ1Full, numGraphNodeQ2_0Full, numGraphNodeQ2Full, numGraphEdgeReqFull, numGraphNodeQ3Full, numGraphNodeQ4_0Full, numGraphNodeQ4Full, numResIncQFull, numCasCxt1Full, numCasCxt2_0Full, numCasCxt2Full, numCasCxtRetryFull);
        end
    endrule
    
    method Action init(BC_AEId fpgaid, Bit#(4) laneid, Bit#(32) tolerance);
        fpgaId <= fpgaid;
        laneId <= laneid;
        started <= True;
        done <= False;
	rg_tolerance <= unpack(tolerance);
        
        lfsr.seed({2'b0, fpgaid, laneid});
        casNumInFlight.init(`SSSPENGINE_NUM_IN_FLIGHT);
        
	numCASRetried <= 0;
        numWorkFetched <= 0;
        numWorkRetired <= 0;
	numWorkDiscarded <= 0;
        numEdgesFetched <= 0;
        numEdgesRetired <= 0;
        numEdgesDiscarded <= 0;
	numWorklistStall <= 0;
	numEdgePipeStall <= 0;
        numGraphNodeQ0Full <= 0;
        numGraphNodeQ1Full <= 0;
        numGraphNodeQ2_0Full <= 0;
        numGraphNodeQ2Full <= 0;
        numGraphNodeQ3Full <= 0;
        numGraphNodeQ4_0Full <= 0;
        numGraphNodeQ4Full <= 0;
        numResIncQFull <= 0;
        numCasCxt2_0Full <= 0;
	numGraphEXCHQFull <= 0;
        
        numCASIssued  <= 0;
        
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
        return extend(numCASRetried);
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
    
    interface workIn = toPut(workInQ);
    interface workOut = toGet(workOutQ);
    interface graphNodeReqs = map(toGet, graphNodeReqQs);
    interface graphPartialNodeReqs = map(toGet, graphPartialNodeReqQs);
    interface graphEdgeReqs = map(toGet, graphEdgeReqQs);
    interface graphCASReqs = map(toGet, graphCASReqQs);
    interface graphEXCHReqs = map(toGet, graphEXCHReqQs);
    interface graphNodeResps = map(toPut, graphNodeRespQs);
    interface graphPartialNodeResps = map(toPut, graphPartialNodeRespQs);
    interface graphEdgeResps = map(toPut, graphEdgeRespQs);
    interface graphCASResps = map(toPut, graphCASRespQs);
    interface graphEXCHResps = map(toPut, graphEXCHRespQs);
endmodule

endpackage
