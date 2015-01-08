
package WorklistFIFO;

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

//import CoalescingCounter::*;
import WLEngine::*;
import GaloisTypes::*;
`include "GaloisDefs.bsv"

interface Worklist;
    interface Vector#(`WL_ENGINE_PORTS, Put#(WLEntry)) enq;
    interface Vector#(`WL_ENGINE_PORTS, Get#(WLEntry)) deq;
    
    interface Vector#(`WL_ENGINE_PORTS, Get#(BC_MC_REQ)) memReq;
    interface Vector#(`WL_ENGINE_PORTS, Put#(BC_MC_RSP)) memResp;
    
    method Action init(BC_AEId fpgaId, BC_Addr lockLoc, BC_Addr headPtrLoc, BC_Addr tailPtrLoc, BC_Addr maxSize, BC_Addr bufferLoc);
    
endinterface


(* synthesize *)
module mkWorklistFIFO(Worklist);
    
    Vector#(`WL_ENGINE_PORTS, FIFOF#(WLEntry)) engineQ <- replicateM(mkSizedBRAMFIFOF(1024));
    
    WLEngine engine <- mkWLEngine();
    
    for(Integer i = 0; i < `WL_ENGINE_PORTS; i = i + 1) begin
        
        rule processFill;
            let pkt <- engine.streamOut[i].get;
            engineQ[i].enq(pkt);
            $display("WorklistFIFO filling ",fshow(pkt));
        endrule
    end

    function Put#(WLEntry) mkEnqF(Integer i);
        return interface Put#(WLEntry);
            method Action put(WLEntry pkt);
                if(engineQ[i].notFull)
                    engineQ[i].enq(pkt);
                else
                    engine.streamIn[i].put(pkt);
            endmethod
        endinterface;
    endfunction

    function Get#(WLEntry) mkDeqF(Integer i);
        return interface Get#(WLEntry);
            method ActionValue#(WLEntry) get();
                if(engineQ[i].notEmpty) begin
                    let pkt = engineQ[i].first();
                    engineQ[i].deq();
                    return pkt;
                end
                else begin
                    Integer stealIdx = ?;
                    if(i == 0) begin
                        let pkt = engineQ[`WL_ENGINE_PORTS-1].first();
                        engineQ[`WL_ENGINE_PORTS-1].deq();
                        return pkt;
                    end
                    else begin    
                        let pkt = engineQ[i-1].first();
                        engineQ[i-1].deq();
                        return pkt;
                    end 
                end
            endmethod
        endinterface;
    endfunction
    
    method Action init(BC_AEId fpgaid, BC_Addr lockloc, BC_Addr headptrloc, BC_Addr tailptrloc, BC_Addr maxsize, BC_Addr bufferloc);
        $display("%0d: mkWorklistFIFO[%0d]: INIT", cur_cycle, fpgaid);
        engine.init(fpgaid, lockloc, headptrloc, tailptrloc, maxsize, bufferloc);
    endmethod
    
    interface enq = genWith(mkEnqF);
    interface deq = genWith(mkDeqF);
    interface memReq = map(toGet, engine.memReq);
    interface memResp = map(toPut, engine.memResp);
endmodule

endpackage
