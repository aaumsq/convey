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

interface GraphEngine;
    interface Vector#(`GRAPH_PORTS, Put#(GraphReq)) req;
    interface Vector#(`GRAPH_PORTS, Get#(GraphResp)) resp;
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
    
    Vector#(16, GraphLane) pipes <- replicateM(mkGraphLanePipe);

    function Put#(GraphReq) genReq(Integer i) = pipes[i].req;
    function Get#(GraphResp) genResp(Integer i) = pipes[i].resp;
    function Get#(MemReq) genMemReq(Integer i) = pipes[i].memReq;
    function Put#(MemResp) genMemResp(Integer i) = pipes[i].memResp;


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
                     pipes[i].init(fpgaId_staging2[i], fromInteger(i), nodePtr_staging2[i], edgePtr_staging2[i]);
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
    interface memReq = genWith(genMemReq);
    interface memResp = genWith(genMemResp);
endmodule

endpackage