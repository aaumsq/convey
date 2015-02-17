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
import GraphLane::*;

interface GraphEngine;
    interface Vector#(`GRAPH_PORTS, Put#(GraphReq)) req;
    interface Vector#(`GRAPH_PORTS, Get#(GraphResp)) resp;
    interface Vector#(16, Get#(BC_MC_REQ)) memReq;
    interface Vector#(16, Put#(BC_MC_RSP)) memResp;

    method Action init(BC_AEId fpgaId, BC_Addr nodePtr, BC_Addr edgePtr);
endinterface

(* synthesize *)
module mkGraphEngine(GraphEngine);
    Reg#(Bool) started <- mkReg(False);
    
    Vector#(`GRAPH_PORTS, FIFOF#(GraphResp)) respQ <- replicateM(mkSizedFIFOF(`GRAPH_NUM_IN_FLIGHT));

    Vector#(16, FIFOF#(BC_MC_REQ)) memReqQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(BC_MC_RSP)) memRespQ <- replicateM(mkFIFOF);
    
    Vector#(16, GraphLane) pipes <- replicateM(mkGraphLane);

    function Put#(GraphReq) genReq(Integer i) = pipes[i].req;
    function Get#(GraphResp) genResp(Integer i) = pipes[i].resp;
    function Get#(BC_MC_REQ) genMemReq(Integer i) = pipes[i].memReq;
    function Put#(BC_MC_RSP) genMemResp(Integer i) = pipes[i].memResp;
    
    method Action init(BC_AEId fpgaid, BC_Addr nodeptr, BC_Addr edgeptr);
        $display("%0d: ~~~~ mkGraphEngine[%0d]: init nodePtr = %0x, edgePtr = %0x", cur_cycle, fpgaid, nodeptr, edgeptr);

        for(Integer i = 0; i < `GRAPH_PORTS; i = i+1) begin
            pipes[i].init(fpgaid, nodeptr, edgeptr);
        end
        started <= True;
    endmethod
    
    
    
    interface req = genWith(genReq);
    interface resp = genWith(genResp);
    interface memReq = genWith(genMemReq);
    interface memResp = genWith(genMemResp);
endmodule

endpackage