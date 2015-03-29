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
import GraphLanePipe::*;
import GraphNodePipe::*;
import GraphEdgePipe::*;
import GraphCASPipe::*;

interface GraphEngineReq;
    interface Vector#(2, Put#(GraphNodeReq)) nodeReq;
    interface Vector#(1, Put#(GraphEdgeReq)) edgeReq;
    interface Vector#(1, Put#(GraphCASReq)) casReq;
endinterface

interface GraphEngineResp;
    interface Vector#(2, Get#(GraphNodeResp)) nodeResp;
    interface Vector#(1, Get#(GraphEdgeResp)) edgeResp;
    interface Vector#(1, Get#(GraphCASResp)) casResp;
endinterface

interface GraphEngine;
    interface Vector#(`GRAPH_PORTS, GraphEngineReq) req;
    interface Vector#(`GRAPH_PORTS, GraphEngineResp) resp;
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
    Vector#(16, Reg#(BC_AEId)) fpgaId_staging <- replicateM(mkRegU);
    Vector#(16, Reg#(BC_Addr)) nodePtr_staging <- replicateM(mkRegU);
    Vector#(16, Reg#(BC_Addr)) edgePtr_staging <- replicateM(mkRegU);
    Vector#(16, Reg#(BC_AEId)) fpgaId_staging2 <- replicateM(mkRegU);
    Vector#(16, Reg#(BC_Addr)) nodePtr_staging2 <- replicateM(mkRegU);
    Vector#(16, Reg#(BC_Addr)) edgePtr_staging2 <- replicateM(mkRegU);

    Vector#(`GRAPH_PORTS, FIFOF#(GraphResp)) respQ <- replicateM(mkSizedFIFOF(`GRAPH_NUM_IN_FLIGHT));
    Vector#(16, FIFOF#(MemReq)) memReqQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) memRespQ <- replicateM(mkFIFOF);

    Vector#(16, Vector#(2, GraphNodeIfc)) nodePipes = ?;
    Vector#(16, Vector#(1, GraphEdgeIfc)) edgePipes = ?;
    Vector#(16, Vector#(1, GraphCASIfc)) casPipes = ?;
    for(Integer i = 0; i < 16; i= i+1) begin
        nodePipes[i][0] <- mkGraphNodePipe(0, 1);
        nodePipes[i][1] <- mkGraphNodePipe(2, 3);
        edgePipes[i][0] <- mkGraphEdgePipe(4);
        casPipes[i][0]  <- mkGraphCASPipe(5);
    end
    
    Vector#(16, Vector#(2, Put#(GraphNodeReq))) nodeReq_tmp = ?;
    Vector#(16, Vector#(1, Put#(GraphEdgeReq))) edgeReq_tmp = ?;
    Vector#(16, Vector#(1, Put#(GraphCASReq))) casReq_tmp = ?;
    for(int i = 0; i < 16; i=i+1) begin
        for(int j = 0; j < 2; j=j+1)
            nodeReq_tmp[i][j] = nodePipes[i][j].req;
        for(int j = 0; j < 1; j=j+1)
            edgeReq_tmp[i][j] = edgePipes[i][j].req;
        for(int j = 0; j < 1; j=j+1)
            casReq_tmp[i][j] = casPipes[i][j].req;
    end
    
    function GraphEngineReq genReq(Integer i);
        return interface GraphEngineReq;
        interface nodeReq = nodeReq_tmp[i];
        interface edgeReq = edgeReq_tmp[i];
        interface casReq = casReq_tmp[i];
        endinterface;
    endfunction
    
    
    Vector#(16, Vector#(2, Get#(GraphNodeResp))) nodeResp_tmp = ?;
    Vector#(16, Vector#(1, Get#(GraphEdgeResp))) edgeResp_tmp = ?;
    Vector#(16, Vector#(1, Get#(GraphCASResp))) casResp_tmp = ?;
    for(int i = 0; i < 16; i=i+1) begin
        for(int j = 0; j < 2; j=j+1)
            nodeResp_tmp[i][j] = nodePipes[i][j].resp;
        for(int j = 0; j < 1; j=j+1)
            edgeResp_tmp[i][j] = edgePipes[i][j].resp;
        for(int j = 0; j < 1; j=j+1)
            casResp_tmp[i][j] = casPipes[i][j].resp;
    end
    
    function GraphEngineResp genResp(Integer i);
        return interface GraphEngineResp;
        interface nodeResp = nodeResp_tmp[i];
        interface edgeResp = edgeResp_tmp[i];
        interface casResp = casResp_tmp[i];
        endinterface;
    endfunction
    
    for(Integer i = 0; i < 16; i=i+1) begin
        (* descending_urgency = "pipesToMemNode0, pipesToMemNode1, pipesToMemEdge0, pipesToMemCAS0" *)
        rule pipesToMemNode0;
            let pkt <- nodePipes[i][0].memReq.get();
            memReqQ[i].enq(pkt);
        endrule
        rule pipesToMemNode1;
            let pkt <- nodePipes[i][1].memReq.get();
            memReqQ[i].enq(pkt);
        endrule
        rule pipesToMemEdge0;
            let pkt <- edgePipes[i][0].memReq.get();
            memReqQ[i].enq(pkt);
        endrule
        rule pipesToMemCAS0;
            let pkt <- casPipes[i][0].memReq.get();
            memReqQ[i].enq(pkt);
        endrule
        
        rule memToPipes;
            MemResp resp = memRespQ[i].first();
            memRespQ[i].deq();
            
            if(resp.gaddr.addr == 0)
                nodePipes[i][0].memResps[0].put(resp);
            else if(resp.gaddr.addr == 1)
                nodePipes[i][0].memResps[1].put(resp);
            else if(resp.gaddr.addr == 2)
                nodePipes[i][1].memResps[0].put(resp);
            else if(resp.gaddr.addr == 3)
                nodePipes[i][1].memResps[1].put(resp);
            else if(resp.gaddr.addr == 4)
                edgePipes[i][0].memResp.put(resp);
            else if(resp.gaddr.addr == 5)
                casPipes[i][0].memResp.put(resp);
        endrule
    end
    
    let fsm <- mkFSM(
       seq
           action
               for(Integer i = 0; i < `GRAPH_PORTS; i = i+1) action
                   action
                     fpgaId_staging[i] <= fpgaId;
                     nodePtr_staging[i] <= nodePtr;
                     edgePtr_staging[i] <= edgePtr;
                   endaction
               endaction
           endaction
           action
               for(Integer i = 0; i < `GRAPH_PORTS; i = i+1) action
                   action
                     fpgaId_staging2[i] <= fpgaId_staging[i];
                     nodePtr_staging2[i] <= nodePtr_staging[i];
                     edgePtr_staging2[i] <= edgePtr_staging[i];
                   endaction
               endaction
           endaction
           action
               for(Integer i = 0; i < `GRAPH_PORTS; i = i+1) action
                   action
                     for(Integer j = 0; j < 2; j=j+1)
                         nodePipes[i][j].init(fpgaId_staging2[i], fromInteger(i), nodePtr_staging2[i]);
                     for(Integer j = 0; j < 1; j=j+1)
                         edgePipes[i][j].init(fpgaId_staging2[i], fromInteger(i), edgePtr_staging2[i]);
                     for(Integer j = 0; j < 1; j=j+1)
                         casPipes[i][j].init(fpgaId_staging2[i], fromInteger(i), nodePtr_staging2[i]);
                         
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
        
        fsm.start();
       endmethod
    
    
    
    interface req = genWith(genReq);
    interface resp = genWith(genResp);
    interface memReq = map(toGet, memReqQ);
    interface memResp = map(toPut, memRespQ);
endmodule

endpackage