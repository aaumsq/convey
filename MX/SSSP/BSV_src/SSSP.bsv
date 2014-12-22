
package SSSP;

import Vector           :: *;
import FIFOF            :: *;
import GetPut           :: *;
import ClientServer     :: *;
import Connectable      :: *;
import StmtFSM          :: *;
import FShow            :: *;
import BRAMFIFO         :: *;

// ----------------
// BC library imports

import BC_Utils           :: *;
import BC_HW_IFC          :: *;
import BC_Transactors     :: *;

//import App_HW::*;

/*
interface App_Ifc;
    method Action start(BC_AEId fpga_id, BC_Data param_block_addr);
    method Action waitTillDone;
    
    interface Vector#(16, BC_MC_Client) mc_ifcs;
endinterface
*/

interface BC_HW2_IFC;
   method Action start (BC_AEId fpga_id, BC_Data param_block_addr);
   method Action waitTillDone;

   interface Vector #(16, BC_MC_Client) mc_ifcs;
endinterface: BC_HW2_IFC


Integer param_graphPtr = 0;
Integer param_worklistPtr = 1;
Integer param_worklistSize = 2;
Integer param_outPtr = 3;

(* synthesize *)
module mkSSSP(BC_HW2_IFC);
    Reg#(BC_AEId) fpgaId <- mkRegU;
    Reg#(BC_Addr) paramPtr <- mkRegU;
    Reg#(Bit#(32)) paramGraphPtr <- mkRegU;
    Reg#(Bit#(32)) paramWorklistPtr <- mkRegU;
    Reg#(Bit#(32)) paramOutPtr <- mkRegU;
    
    Vector#(16, FIFOF#(BC_MC_REQ)) memReqQ  <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(BC_MC_RSP)) memRespQ <- replicateM(mkFIFOF);
    /*
    for(Integer mc = 0; mc < 16; mc = mc + 1) begin
        mkConnection();
    end
    */
    
    rule dan;
        $display("DAN");
    endrule
    
    
    function BC_MC_Client fn_mkMC_Client(Integer mc);
        return interface BC_MC_Client;
            interface req_rsp = interface Client;
                interface request = toGet(memReqQ[mc]);
                interface response = toPut(memRespQ[mc]);
            endinterface;
        endinterface;
    endfunction
    
    interface mc_ifcs = genWith(fn_mkMC_Client);
endmodule

endpackage