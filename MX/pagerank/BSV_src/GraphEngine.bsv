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
//import GraphLanePipe::*;
import GraphNodePipe::*;
import GraphPartialNodePipe::*;
import GraphEdgePipe::*;
import GraphCASPipe::*;
import GraphEXCHPipe::*;

interface GraphEngineReq;
    interface Vector#(1, Put#(GraphNodeReq)) nodeReq;
    interface Vector#(2, Put#(GraphPartialNodeReq)) partialnodeReq;
    interface Vector#(1, Put#(GraphEdgeReq)) edgeReq;
    interface Vector#(1, Put#(GraphCASReq)) casReq;
    interface Vector#(1, Put#(GraphEXCHReq)) exchReq;
endinterface

interface GraphEngineResp;
    interface Vector#(1, Get#(GraphNodeResp)) nodeResp;
    interface Vector#(2, Get#(GraphPartialNodeResp)) partialnodeResp;
    interface Vector#(1, Get#(GraphEdgeResp)) edgeResp;
    interface Vector#(1, Get#(GraphCASResp)) casResp;
    interface Vector#(1, Get#(GraphEXCHResp)) exchResp;
endinterface

interface GraphEngine;
    interface Vector#(`NUM_ENGINES, GraphEngineReq) req;
    interface Vector#(`NUM_ENGINES, GraphEngineResp) resp;
    interface Vector#(16, Get#(MemReq)) memReq;
    interface Vector#(16, Put#(MemResp)) memResp;

    method Action init(BC_AEId fpgaId, BC_Addr nodePtr, BC_Addr edgePtr);
endinterface

(* synthesize *)
module mkGraphEngine(GraphEngine);
    Reg#(Bool) started <- mkReg(False);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(BC_Addr) nodePtr <- mkRegU;
    Reg#(BC_Addr) edgePtr <- mkRegU;
    Vector#(`NUM_ENGINES, Reg#(BC_AEId)) fpgaId_staging <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Reg#(BC_Addr)) nodePtr_staging <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Reg#(BC_Addr)) edgePtr_staging <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Reg#(BC_AEId)) fpgaId_staging2 <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Reg#(BC_Addr)) nodePtr_staging2 <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Reg#(BC_Addr)) edgePtr_staging2 <- replicateM(mkRegU);

    Vector#(16, FIFOF#(MemReq)) memReqQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) memRespQ <- replicateM(mkFIFOF);

    Vector#(`NUM_ENGINES, Vector#(1, GraphNodeIfc)) nodePipes = ?;
    Vector#(`NUM_ENGINES, Vector#(2, GraphPartialNodeIfc)) partialnodePipes = ?;
    Vector#(`NUM_ENGINES, Vector#(1, GraphEdgeIfc)) edgePipes = ?;
    Vector#(`NUM_ENGINES, Vector#(1, GraphCASIfc)) casPipes = ?;
    Vector#(`NUM_ENGINES, Vector#(1, GraphEXCHIfc)) exchPipes = ?;
    for(Integer i = 0; i < `NUM_ENGINES; i= i+1) begin
        nodePipes[i][0] <- mkGraphNodePipe(0, 1, 2);
        partialnodePipes[i][0] <- mkGraphPartialNodePipe(3, 8);
        partialnodePipes[i][1] <- mkGraphPartialNodePipe(4, 16);
        edgePipes[i][0] <- mkGraphEdgePipe(5);
        exchPipes[i][0]  <- mkGraphEXCHPipe(6);
        casPipes[i][0]  <- mkGraphCASPipe(7);
    end
    
    Vector#(`NUM_ENGINES, Vector#(1, Put#(GraphNodeReq))) nodeReq_tmp = ?;
    Vector#(`NUM_ENGINES, Vector#(2, Put#(GraphPartialNodeReq))) partialnodeReq_tmp = ?;
    Vector#(`NUM_ENGINES, Vector#(1, Put#(GraphEdgeReq))) edgeReq_tmp = ?;
    Vector#(`NUM_ENGINES, Vector#(1, Put#(GraphCASReq))) casReq_tmp = ?;
    Vector#(`NUM_ENGINES, Vector#(1, Put#(GraphEXCHReq))) exchReq_tmp = ?;
    for(int i = 0; i < `NUM_ENGINES; i=i+1) begin
        for(int j = 0; j < 1; j=j+1)
            nodeReq_tmp[i][j] = nodePipes[i][j].req;
        for(int j = 0; j < 2; j=j+1)
            partialnodeReq_tmp[i][j] = partialnodePipes[i][j].req;
        for(int j = 0; j < 1; j=j+1)
            edgeReq_tmp[i][j] = edgePipes[i][j].req;
        for(int j = 0; j < 1; j=j+1)
            casReq_tmp[i][j] = casPipes[i][j].req;
        for(int j = 0; j < 1; j=j+1)
            exchReq_tmp[i][j] = exchPipes[i][j].req;
    end
    
    function GraphEngineReq genReq(Integer i);
        return interface GraphEngineReq;
        interface nodeReq = nodeReq_tmp[i];
	interface partialnodeReq = partialnodeReq_tmp[i];
        interface edgeReq = edgeReq_tmp[i];
        interface casReq = casReq_tmp[i];
	interface exchReq = exchReq_tmp[i];
        endinterface;
    endfunction
    /*
    rule print;
        function Bool reqEmptyF(Integer x) = !memReqQ[x].notEmpty;
        function Bool reqFullF(Integer x) = !memReqQ[x].notFull;
        function Bool respEmptyF(Integer x) = !memRespQ[x].notEmpty;
        function Bool respFullF(Integer x) = !memRespQ[x].notFull;
        Vector#(16, Bool) reqEmpty = genWith(reqEmptyF);
        Vector#(16, Bool) respEmpty = genWith(respEmptyF);
        Vector#(16, Bool) reqFull = genWith(reqFullF);
        Vector#(16, Bool) respFull = genWith(respFullF);
        
        let cycle <- cur_cycle;
        if(cycle == 100000) $display("%0d: graphEngine[%0d] memReqQ empty:%b memReqQ full:%b memRespQ empty:%b memRespQ full:%b", cur_cycle, fpgaId,
           reqEmpty, reqFull, respEmpty, respFull);
    endrule
    */
    Vector#(`NUM_ENGINES, Vector#(1, Get#(GraphNodeResp))) nodeResp_tmp = ?;
    Vector#(`NUM_ENGINES, Vector#(2, Get#(GraphPartialNodeResp))) partialnodeResp_tmp = ?;
    Vector#(`NUM_ENGINES, Vector#(1, Get#(GraphEdgeResp))) edgeResp_tmp = ?;
    Vector#(`NUM_ENGINES, Vector#(1, Get#(GraphCASResp))) casResp_tmp = ?;
    Vector#(`NUM_ENGINES, Vector#(1, Get#(GraphEXCHResp))) exchResp_tmp = ?;
    for(int i = 0; i < `NUM_ENGINES; i=i+1) begin
        for(int j = 0; j < 1; j=j+1)
            nodeResp_tmp[i][j] = nodePipes[i][j].resp;
        for(int j = 0; j < 2; j=j+1)
            partialnodeResp_tmp[i][j] = partialnodePipes[i][j].resp;
        for(int j = 0; j < 1; j=j+1)
            edgeResp_tmp[i][j] = edgePipes[i][j].resp;
        for(int j = 0; j < 1; j=j+1)
            casResp_tmp[i][j] = casPipes[i][j].resp;
        for(int j = 0; j < 1; j=j+1)
            exchResp_tmp[i][j] = exchPipes[i][j].resp;
    end
    
    function GraphEngineResp genResp(Integer i);
        return interface GraphEngineResp;
        interface nodeResp = nodeResp_tmp[i];
	interface partialnodeResp = partialnodeResp_tmp[i];
        interface edgeResp = edgeResp_tmp[i];
        interface casResp = casResp_tmp[i];
        interface exchResp = exchResp_tmp[i];
        endinterface;
    endfunction
    
    //Vector#(4, Reg#(UInt#(3))) curIdx <- replicateM(mkRegU);
    Vector#(16, Reg#(UInt#(3))) whoCanReq <- replicateM(mkRegU);
    Vector#(32, FIFOF#(MemReq)) memInterReqQ <- replicateM(mkFIFOF);
    Vector#(16, Reg#(Bit#(1))) curIdx <- replicateM(mkRegU);
    for(Integer i = 0; i < `NUM_ENGINES; i=i+1) begin
        //(* descending_urgency = "pipesToMemPartialNode1, pipesToMemPartialNode0, pipesToMemEXCH0, pipesToMemNode0_2, pipesToMemNode0_1, pipesToMemNode0_0" *)
        rule pipesToMemNode0_0;
            let pkt <- nodePipes[i][0].memReqs[0].get();
            memInterReqQ[i*8].enq(pkt);
        endrule
        rule pipesToMemNode0_1;
            let pkt <- nodePipes[i][0].memReqs[1].get();
            memInterReqQ[i*8+1].enq(pkt);
        endrule
	rule pipesToMemNode0_2;
	    let pkt <- nodePipes[i][0].memReqs[2].get();
	    memInterReqQ[i*8+2].enq(pkt);
	endrule
        rule pipesToMemPartialNode0;
            let pkt <- partialnodePipes[i][0].memReq.get();
            memInterReqQ[i*8+3].enq(pkt);
        endrule
        rule pipesToMemPartialNode1;
            let pkt <- partialnodePipes[i][1].memReq.get();
            memInterReqQ[i*8+4].enq(pkt);
        endrule
        rule pipesToMemEdge0;
            let pkt <- edgePipes[i][0].memReq.get();
            memInterReqQ[i*8+5].enq(pkt);
        endrule
        rule pipesToMemEXCH0;
            let pkt <- exchPipes[i][0].memReq.get();
            memInterReqQ[i*8+6].enq(pkt);
        endrule 
        rule pipesToMemCAS0;
            let pkt <- casPipes[i][0].memReq.get();
            memInterReqQ[i*8+7].enq(pkt);
        endrule 
    end

    for (Integer i = 0; i < 4; i = i + 1) begin
        for (Integer j = 0; j < 4; j = j + 1) begin
            rule memInterReq;
	        if (curIdx[i*4+j] == 0) begin
	            if (memInterReqQ[i*8+j].notEmpty) begin
	    	        MemReq pkt = memInterReqQ[i*8+j].first();
	    	        memInterReqQ[i*8+j].deq();
	    	        memReqQ[i*4+j].enq(pkt);
	            end
		    else if (memInterReqQ[i*8+4+j].notEmpty) begin
	    	        MemReq pkt = memInterReqQ[i*8+4+j].first();
	    	        memInterReqQ[i*8+4+j].deq();
	    	        memReqQ[i*4+j].enq(pkt);
		    end 
	    	end
		else begin
		    if (memInterReqQ[i*8+4+j].notEmpty) begin
	    	        MemReq pkt = memInterReqQ[i*8+4+j].first();
	    	        memInterReqQ[i*8+4+j].deq();
	    	        memReqQ[i*4+j].enq(pkt);
	            end
	            else if (memInterReqQ[i*8+j].notEmpty) begin
	    	        MemReq pkt = memInterReqQ[i*8+j].first();
	    	        memInterReqQ[i*8+j].deq();
	    	        memReqQ[i*4+j].enq(pkt);
	            end
		end
	    endrule
	end
    end

    for (Integer i = 0; i < 16; i = i + 1) begin
        rule toggle(started);
	    curIdx[i] <= curIdx[i] + 1;
	endrule
    end

	    
    //for (Integer i = 0; i < 4; i = i + 1) begin
    //    rule memInterReq0;
    //        UInt#(3) idx = curIdx;
    //        UInt#(3) inc = 0;
    //        for (Integer j = 0; j < 8; j = j + 1) begin
    //            if (memInterReqQ[i*8+idx].notEmpty && inc < 4) begin
    //    	    whoCanReq[i*4+inc] = idx;
    //    	    idx = idx + 1;
    //    	    inc = inc + 1;
    //    	end
    //        end
    //        curIdx <= idx;
    //    endrule
    //end
       
    for (Integer i = 0; i < 4; i=i+1) begin
        rule memToPipes0;
            MemResp resp = memRespQ[i*4].first();
            memRespQ[i*4].deq();
            
            if(resp.gaddr.addr == 0)
                nodePipes[i][0].memResps[0].put(resp);
            else if(resp.gaddr.addr == 4)
	        partialnodePipes[i][1].memResp.put(resp);
            
        endrule
        rule memToPipes1;
            MemResp resp = memRespQ[i*4+1].first();
            memRespQ[i*4+1].deq();
            
            if(resp.gaddr.addr == 1)
                nodePipes[i][0].memResps[1].put(resp);
            else if(resp.gaddr.addr == 5)
                edgePipes[i][0].memResp.put(resp);
        endrule

        rule memToPipes2;
            MemResp resp = memRespQ[i*4+2].first();
            memRespQ[i*4+2].deq();
        
            if(resp.gaddr.addr == 2)
                nodePipes[i][0].memResps[2].put(resp);
            else if(resp.gaddr.addr == 6)
	        exchPipes[i][0].memResp.put(resp);
        endrule
                    
        rule memToPipes3;
            MemResp resp = memRespQ[i*4+3].first();
            memRespQ[i*4+3].deq();

	    if(resp.gaddr.addr == 3)
	        partialnodePipes[i][0].memResp.put(resp);
	    else if(resp.gaddr.addr == 7)
                casPipes[i][0].memResp.put(resp);
        endrule
    end

    let fsm <- mkFSM(
       seq
           action
               for(Integer i = 0; i < `NUM_ENGINES; i = i+1) action
                   action
                     fpgaId_staging[i] <= fpgaId;
                     nodePtr_staging[i] <= nodePtr;
                     edgePtr_staging[i] <= edgePtr;
                   endaction
               endaction
           endaction
           action
               for(Integer i = 0; i < `NUM_ENGINES; i = i+1) action
                   action
                     fpgaId_staging2[i] <= fpgaId_staging[i];
                     nodePtr_staging2[i] <= nodePtr_staging[i];
                     edgePtr_staging2[i] <= edgePtr_staging[i];
                   endaction
               endaction
           endaction
           action
               for(Integer i = 0; i < `NUM_ENGINES; i = i+1) action
                   action
                     for(Integer j = 0; j < 1; j=j+1)
                         nodePipes[i][j].init(fpgaId_staging2[i], fromInteger(i), nodePtr_staging2[i]);
                     for(Integer j = 0; j < 2; j=j+1)
                         partialnodePipes[i][j].init(fpgaId_staging2[i], fromInteger(i), nodePtr_staging2[i]);
                     for(Integer j = 0; j < 1; j=j+1)
                         edgePipes[i][j].init(fpgaId_staging2[i], fromInteger(i), edgePtr_staging2[i]);
                     for(Integer j = 0; j < 1; j=j+1)
                         casPipes[i][j].init(fpgaId_staging2[i], fromInteger(i), nodePtr_staging2[i]);
                     for(Integer j = 0; j < 1; j=j+1)
                         exchPipes[i][j].init(fpgaId_staging2[i], fromInteger(i), nodePtr_staging2[i]);
                         
                   endaction
               endaction
           endaction
       
           action
               started <= True;
           endaction
       endseq
    );
    
    
    method Action init(BC_AEId fpgaid, BC_Addr nodeptr, BC_Addr edgeptr);
        $display("%0d: ~~~~ mkGraphEngine[%0d]: init nodePtr = %0x, edgePtr = %0x", cur_cycle, fpgaid, nodeptr, edgeptr);
        fpgaId <= fpgaid;
        nodePtr <= nodeptr;
        edgePtr <= edgeptr;
	//for (Integer i = 0; i < 8; i = i + 1) begin
	//    turns[i] <= False;
	//end
	for (Integer i = 0; i < 16; i = i + 1) begin
	    curIdx[i] <= 0;
	end
        
        fsm.start();
       endmethod
    
    
    
    interface req = genWith(genReq);
    interface resp = genWith(genResp);
    interface memReq = map(toGet, memReqQ);
    interface memResp = map(toPut, memRespQ);
endmodule

endpackage
