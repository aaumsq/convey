
package SSSPEngine;

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


interface SSSPEngineIfc;
    method Action start();
    method ActionValue#(Bit#(64)) result;
    
    
endinterface


module mkSSSPEngine(SSSPEngineIfc);
    Reg#(BC_AEId) fpgaId <-mkRegU;
    
    
endmodule

endpackage
