package WLEngine;

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

import BufBRAMFIFOF::*;
import CoalescingCounter::*;
import GaloisTypes::*;
`include "GaloisDefs.bsv"

interface WLEngine;
    interface Vector#(`NUM_ENGINES, Put#(WLEntry)) streamIn;
    interface Vector#(`NUM_ENGINES, Get#(WLEntry)) streamOut;

    interface Vector#(16, Get#(MemReq)) memReq;
    interface Vector#(16, Put#(MemResp)) memResp;

    interface Vector#(`NUM_ENGINES, Reg#(Bit#(3))) priority_ifc;
    interface Reg#(Bool) cur_pri_ifc;

    method Action init(BC_AEId fpgaId, BC_Addr lockLoc, BC_Addr headPtrLoc, BC_Addr tailPtrLoc, BC_Addr maxSize, BC_Addr bufferLoc, BC_Addr offsetLoc);
    method Action stop();
    method Bool isDone();
    method Bit#(64) getOffset();
endinterface


`define WLENGINE_BUFOUT_SIZE 1024
`define WLENGINE_BUFIN_SIZE 1024
`define WLENTRY_SIZE 8
`define LG_WLENTRY_SIZE 3
`define WLENGINE_MAX_WRITES 16384
`define WLENGINE_BACKOFF 128
`define WRITEFSM_TIMEOUT 2048

(* synthesize *)
module mkWLEngine(WLEngine);
    // memOffset[0]: lock bit
    // memOffset[1]: headPtr
    // memOffset[2]: tailPtr
    // memOffset[3]: workList size
    // memOffset[4]: start of data
    
    Reg#(BC_AEId) fpgaId <- mkReg(0);
    Reg#(BC_Addr) lockLoc_w <- mkRegU;
    Reg#(BC_Addr) lockLoc_r <- mkRegU;
    Reg#(Bit#(64)) rg_offset_r <- mkRegU;
    Reg#(Bit#(64)) rg_offset_w <- mkRegU;
    Reg#(Bit#(64)) rg_offset_buf <- mkRegU;
    Reg#(Bit#(64)) rg_offset_buf2 <- mkRegU;
    Reg#(Bit#(64)) rg_preoffset_r <- mkRegU;
    Reg#(Bit#(64)) rg_preoffset_w <- mkRegU;
    Reg#(BC_Addr) offsetLoc <- mkRegU;
    Reg#(Bool) rg_move_engine <- mkRegU;
    Reg#(Bool) rg_cur_pri <- mkRegU;
    Reg#(Bool) cur_pri <- mkRegU;
    //Reg#(BC_Addr) headPtr <- mkRegU;
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) headPtr_w <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) headPtr_r <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) headPtrLoc_w <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) headPtrLoc_r <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) headPtrBaseLoc <- replicateM(mkRegU);
    
    //Reg#(BC_Addr) tailPtr <- mkRegU;
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) tailPtr_w <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) tailPtr_r <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) tailPtrLoc_w <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) tailPtrLoc_r <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) tailPtrBaseLoc <- replicateM(mkRegU);
    
    Reg#(BC_Addr) maxSize <- mkRegU;
    Reg#(BC_Addr) maxSize_mask <- mkRegU;
    //Reg#(BC_Addr) bufferLoc <- mkRegU;
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) bufferBaseLoc <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) bufferLoc_w <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) bufferLoc_r <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Vector#(2, Reg#(Bit#(3)))) pri_buffer <- replicateM(replicateM(mkRegU));
    Vector#(`NUM_ENGINES, Reg#(Bit#(3))) pri_respQ <- replicateM(mkRegU);
    
    Reg#(Bool) started <- mkReg(False);
    Reg#(Bool) done <- mkReg(False);
    Reg#(Bool) turn <- mkReg(False);
    Reg#(Bit#(1)) curBufOut <- mkRegU;
    
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) reqQ <- replicateM(mkFIFOF);
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) respQ <- replicateM(mkFIFOF);
    Vector#(`NUM_ENGINES, Vector#(2, Reg#(Bit#(16)))) bufOutTotalLen <- replicateM(replicateM(mkRegU));
    Vector#(`NUM_ENGINES, Vector#(8, Vector#(2, Reg#(Bit#(16))))) bufOutLen <- replicateM(replicateM(replicateM(mkRegU)));

    Vector#(16, FIFOF#(MemReq)) writeMemReqQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemReq)) readMemReqQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) writeMemRespQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) readMemRespQ <- replicateM(mkFIFOF);
    
    Vector#(16, FIFOF#(MemReq)) memReqQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) memRespQ <- replicateM(mkFIFOF);
    
    Vector#(`NUM_ENGINES, Vector#(2, FIFOF#(WLEntry))) doubleBufIn <- replicateM(replicateM(mkSizedBufBRAMFIFOF(`WLENGINE_BUFIN_SIZE)));
    Vector#(`NUM_ENGINES, Reg#(Bit#(1))) curBufIn <- replicateM(mkRegU);

    Vector#(`NUM_ENGINES, Vector#(2, FIFOF#(WLEntry))) doubleBufOut <- replicateM(replicateM(mkSizedBufBRAMFIFOF(`WLENGINE_BUFOUT_SIZE)));

    for (Integer i = 0; i < 16; i = i + 1) begin
        (* descending_urgency = "readToMem, writeToMem" *)
        rule readToMem;
	    MemReq req = readMemReqQ[i].first();
	    readMemReqQ[i].deq();
	    memReqQ[i].enq(req);
	endrule

        rule writeToMem;
	    MemReq req = writeMemReqQ[i].first();
	    writeMemReqQ[i].deq();
	    memReqQ[i].enq(req);
	endrule

	rule memToFSM;
	    MemResp pkt = memRespQ[i].first();
	    memRespQ[i].deq();

	    if (pkt.gaddr.addr == 0)
	        readMemRespQ[i].enq(pkt);
	    else if (pkt.gaddr.addr == 1)
	        writeMemRespQ[i].enq(pkt);
	endrule
    end

	    
    function BC_Addr getWLSize(BC_Addr head, BC_Addr tail, BC_Addr max);
        // Get number of entries in FIFO
        BC_Addr size = 0;
        if(head < tail) begin
            size = tail - head;
        end
        else if(head > tail) begin
            size = (max - tail) + head;
        end
        return size;
    endfunction
                                                          
    // Lock FSM: gets global FSM lock and updates head/tail pointers
    
    Reg#(Bit#(32)) lock_lockData_w <- mkRegU;
    Reg#(Bit#(32)) lock_lockData_r <- mkRegU;
    Reg#(Bit#(16)) lockFSM_backOff_w <- mkRegU;
    Reg#(Bit#(16)) lockFSM_backOff_r <- mkRegU;
    let lockFSM_w <- mkFSM(
        seq
            action
                lock_lockData_w <= 32'b1;
            endaction
            
            // Obtain global worklist lock
            while(lock_lockData_w == 32'd1) seq
                action
                    //if (`DEBUG) $display("%0d: mkWLEngine[%0d]: Reading lock bit at addr: %0x...", cur_cycle, fpgaId, lockLoc);
                    writeMemReqQ[0].enq(tagged MemCAS32{addr: lockLoc_w, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: 1}, cmpVal: 0, swapVal: 1});
                endaction
            
                action
                    MemResp rsp = writeMemRespQ[0].first();
                    writeMemRespQ[0].deq();
                    lock_lockData_w <= truncate(rsp.data);
                    if (`DEBUG) $display("%0d: mkWLEngine[%0d]: old lock bit = %0d", cur_cycle, fpgaId, rsp.data);
                    // Data is the old data, so if 1 then it failed
                    if(truncate(rsp.data) == 32'd1) begin
                        //if(`DEBUG) $display("%0d: mkWLEngine[%0d]:  Worklist is locked, retry...", cur_cycle, fpgaId);
                    end
                endaction

                if(lock_lockData_w == 32'd1) seq
                    if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM nothing to read, stalling for %0d cycles", cur_cycle, fpgaId, `WLENGINE_BACKOFF);
                    lockFSM_backOff_w <= 0;
                    while(lockFSM_backOff_w < 128) seq
                        action
                          lockFSM_backOff_w <= lockFSM_backOff_w + 1;
                        endaction
                    endseq
                endseq
            endseq
           
	    // Get offset
            action
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: 1};
	        writeMemReqQ[0].enq(tagged MemRead64{addr: offsetLoc, gaddr: gaddr});
	    endaction
	
            action
                //$display("%0d: mkWLEngine[%0d]: getting updated head/tail ptrs", cur_cycle, fpgaId);
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: 1};
	        MemResp offsetRsp = writeMemRespQ[0].first();
	        writeMemRespQ[0].deq();
                rg_offset_w <= offsetRsp.data;
	    endaction

	    action
		//$display("%0d: offset is %0d", cur_cycle, rg_offset_w);
	        if (rg_offset_w[2:0] == 3'd0) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_w[i] <= headPtrBaseLoc[i];
	                tailPtrLoc_w[i] <= tailPtrBaseLoc[i];
	                bufferLoc_w[i] <= bufferBaseLoc[i];
	            end
	        end
	        else if (rg_offset_w[2:0] == 3'd1) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_w[i] <= headPtrBaseLoc[(i+7)%8];
	                tailPtrLoc_w[i] <= tailPtrBaseLoc[(i+7)%8];
	                bufferLoc_w[i] <= bufferBaseLoc[(i+7)%8];
	            end
	        end
	        else if (rg_offset_w[2:0] == 3'd2) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_w[i] <= headPtrBaseLoc[(i+6)%8];
	                tailPtrLoc_w[i] <= tailPtrBaseLoc[(i+6)%8];
	                bufferLoc_w[i] <= bufferBaseLoc[(i+6)%8];
	            end
	        end
	        else if (rg_offset_w[2:0] == 3'd3) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_w[i] <= headPtrBaseLoc[(i+5)%8];
	                tailPtrLoc_w[i] <= tailPtrBaseLoc[(i+5)%8];
	                bufferLoc_w[i] <= bufferBaseLoc[(i+5)%8];
	            end
	        end
	        else if (rg_offset_w[2:0] == 3'd4) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_w[i] <= headPtrBaseLoc[(i+4)%8];
	                tailPtrLoc_w[i] <= tailPtrBaseLoc[(i+4)%8];
	                bufferLoc_w[i] <= bufferBaseLoc[(i+4)%8];
	            end
	        end
	        else if (rg_offset_w[2:0] == 3'd5) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_w[i] <= headPtrBaseLoc[(i+3)%8];
	                tailPtrLoc_w[i] <= tailPtrBaseLoc[(i+3)%8];
	                bufferLoc_w[i] <= bufferBaseLoc[(i+3)%8];
	            end
	        end
	        else if (rg_offset_w[2:0] == 3'd6) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_w[i] <= headPtrBaseLoc[(i+2)%8];
	                tailPtrLoc_w[i] <= tailPtrBaseLoc[(i+2)%8];
	                bufferLoc_w[i] <= bufferBaseLoc[(i+2)%8];
	            end
	        end
	        else begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_w[i] <= headPtrBaseLoc[(i+1)%8];
	                tailPtrLoc_w[i] <= tailPtrBaseLoc[(i+1)%8];
	                bufferLoc_w[i] <= bufferBaseLoc[(i+1)%8];
	            end
	        end
	    endaction

            // Get updated head and tail pointers
	    action
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: 1};
                for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
                    writeMemReqQ[i].enq(tagged MemRead64{addr: headPtrLoc_w[i], gaddr: gaddr});
                end
                for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
                    writeMemReqQ[i+8].enq(tagged MemRead64{addr: tailPtrLoc_w[i], gaddr: gaddr});
                end
            endaction
            
            action
                for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
                    MemResp headRsp = writeMemRespQ[i].first();
                    writeMemRespQ[i].deq();
                    headPtr_w[i] <= truncate(pack(headRsp.data));
                end
                for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
                    MemResp tailRsp = writeMemRespQ[i+8].first();
                    writeMemRespQ[i+8].deq();
                    tailPtr_w[i] <= truncate(pack(tailRsp.data));
                end
                //if(`DEBUG) $display("%0d: mkWLEngine[%0d]: headPtr: %0x, tailPtr: %0x", cur_cycle, fpgaId, headRsp.data, tailRsp.data);
            endaction

        endseq
    );

    let lockFSM_r <- mkFSM(
        seq
            action
                lock_lockData_r <= 32'b1;
            endaction
            
            // Obtain global worklist lock
            while(lock_lockData_r == 32'd1) seq
                action
                    //if (`DEBUG) $display("%0d: mkWLEngine[%0d]: Reading lock bit at addr: %0x...", cur_cycle, fpgaId, lockLoc);
                    readMemReqQ[4].enq(tagged MemCAS32{addr: lockLoc_r, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: 0}, cmpVal: 0, swapVal: 1});
                endaction
            
                action
                    MemResp rsp = readMemRespQ[4].first();
                    readMemRespQ[4].deq();
                    lock_lockData_r <= truncate(rsp.data);
                    if (`DEBUG) $display("%0d: mkWLEngine[%0d]: old lock bit = %0d", cur_cycle, fpgaId, rsp.data);
                    // Data is the old data, so if 1 then it failed
                    if(truncate(rsp.data) == 32'd1) begin
                        //if(`DEBUG) $display("%0d: mkWLEngine[%0d]:  Worklist is locked, retry...", cur_cycle, fpgaId);
                    end
                endaction

                if(lock_lockData_r == 32'd1) seq
                    if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM nothing to read, stalling for %0d cycles", cur_cycle, fpgaId, `WLENGINE_BACKOFF);
                    lockFSM_backOff_r <= 0;
                    while(lockFSM_backOff_r < 128) seq
                        action
                          lockFSM_backOff_r <= lockFSM_backOff_r + 1;
                        endaction
                    endseq
                endseq
            endseq
           
	    // Get offset
            action
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: 0};
	        readMemReqQ[0].enq(tagged MemRead64{addr: offsetLoc, gaddr: gaddr});
	    endaction
	
            action
                //$display("%0d: mkWLEngine[%0d]: getting updated head/tail ptrs", cur_cycle, fpgaId);
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: 0};
	        MemResp offsetRsp = readMemRespQ[0].first();
	        readMemRespQ[0].deq();
                rg_offset_r <= offsetRsp.data;
	    endaction

	    action
		//$display("%0d: offset is %0d", cur_cycle, rg_offset_r);
	        if (rg_offset_r[2:0] == 3'd0) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_r[i] <= headPtrBaseLoc[i];
	                tailPtrLoc_r[i] <= tailPtrBaseLoc[i];
	                bufferLoc_r[i] <= bufferBaseLoc[i];
	            end
	        end
	        else if (rg_offset_r[2:0] == 3'd1) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_r[i] <= headPtrBaseLoc[(i+7)%8];
	                tailPtrLoc_r[i] <= tailPtrBaseLoc[(i+7)%8];
	                bufferLoc_r[i] <= bufferBaseLoc[(i+7)%8];
	            end
	        end
	        else if (rg_offset_r[2:0] == 3'd2) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_r[i] <= headPtrBaseLoc[(i+6)%8];
	                tailPtrLoc_r[i] <= tailPtrBaseLoc[(i+6)%8];
	                bufferLoc_r[i] <= bufferBaseLoc[(i+6)%8];
	            end
	        end
	        else if (rg_offset_r[2:0] == 3'd3) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_r[i] <= headPtrBaseLoc[(i+5)%8];
	                tailPtrLoc_r[i] <= tailPtrBaseLoc[(i+5)%8];
	                bufferLoc_r[i] <= bufferBaseLoc[(i+5)%8];
	            end
	        end
	        else if (rg_offset_r[2:0] == 3'd4) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_r[i] <= headPtrBaseLoc[(i+4)%8];
	                tailPtrLoc_r[i] <= tailPtrBaseLoc[(i+4)%8];
	                bufferLoc_r[i] <= bufferBaseLoc[(i+4)%8];
	            end
	        end
	        else if (rg_offset_r[2:0] == 3'd5) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_r[i] <= headPtrBaseLoc[(i+3)%8];
	                tailPtrLoc_r[i] <= tailPtrBaseLoc[(i+3)%8];
	                bufferLoc_r[i] <= bufferBaseLoc[(i+3)%8];
	            end
	        end
	        else if (rg_offset_r[2:0] == 3'd6) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_r[i] <= headPtrBaseLoc[(i+2)%8];
	                tailPtrLoc_r[i] <= tailPtrBaseLoc[(i+2)%8];
	                bufferLoc_r[i] <= bufferBaseLoc[(i+2)%8];
	            end
	        end
	        else begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc_r[i] <= headPtrBaseLoc[(i+1)%8];
	                tailPtrLoc_r[i] <= tailPtrBaseLoc[(i+1)%8];
	                bufferLoc_r[i] <= bufferBaseLoc[(i+1)%8];
	            end
	        end
	    endaction

            // Get updated head and tail pointers
	    action
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: 0};
                for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
                    readMemReqQ[i].enq(tagged MemRead64{addr: headPtrLoc_r[i], gaddr: gaddr});
                end
                for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
                    readMemReqQ[i+8].enq(tagged MemRead64{addr: tailPtrLoc_r[i], gaddr: gaddr});
                end
            endaction
            
            action
                for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
                    MemResp headRsp = readMemRespQ[i].first();
                    readMemRespQ[i].deq();
                    headPtr_r[i] <= truncate(pack(headRsp.data));
                end
                for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
                    MemResp tailRsp = readMemRespQ[i+8].first();
                    readMemRespQ[i+8].deq();
                    tailPtr_r[i] <= truncate(pack(tailRsp.data));
                end
                //if(`DEBUG) $display("%0d: mkWLEngine[%0d]: headPtr: %0x, tailPtr: %0x", cur_cycle, fpgaId, headRsp.data, tailRsp.data);
            endaction

       endseq
       );

   
    // Write FSM: Writes all full buffers
    Reg#(Bit#(`LG_NUM_ENGINES)) writeFSM_curIdx <- mkRegU;
    Reg#(Bit#(1)) writeFSM_curBufIdx <- mkRegU;
    //Reg#(Bit#(16)) writeFSM_totalWrites <- mkRegU;
    Reg#(Bit#(1)) newBufIdx <- mkRegU;
    Reg#(Bool) writeFSM_done <- mkRegU;
    Vector#(`NUM_ENGINES, Vector#(`NUM_PRIORITIES, Reg#(BC_Addr))) writeFSM_tails <- replicateM(replicateM(mkRegU));
    Vector#(`NUM_ENGINES, FIFOF#(Bit#(0))) writeFSM_outstandingWrites <- replicateM(mkSizedFIFOF(256));
    Vector#(`NUM_PRIORITIES, Reg#(Bit#(16))) writeFSM_numTotalEntries <- replicateM(mkRegU);
    Reg#(Bool) spillTo0_0 <- mkReg(False);
    Reg#(Bool) spillTo0_1 <- mkReg(False);
    
    for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
        rule writeFSM_catchWriteAcks(writeFSM_outstandingWrites[i].notEmpty);
            writeMemRespQ[4*i].deq();
            writeFSM_outstandingWrites[i].deq();
        endrule
    end

    let writeFSM <- mkFSM(
        seq
            action
                if(`DEBUG) $display("%0d: mkWLEngine: WriteFSM START", cur_cycle);
                lockFSM_w.start();
                //writeFSM_totalWrites <= 0;
                writeFSM_done <= False;
		//$display("%0d: mkWLEngine[%0d]: writeFSM_curBufIdx is %0d", cur_cycle, fpgaId, writeFSM_curBufIdx);
            endaction
            
            action
                //if(`DEBUG) $display("%0d: mkWLEngine: WriteFSM lock + head/tailPtr obtained! headPtr=%0d, tailPtr=%0d", cur_cycle, headPtr, tailPtr);
                lockFSM_w.waitTillDone();
		turn <= True;
		//$display("%0d: mkWLEngine[%0d]: writeFSM_curBufIdx is %0d", cur_cycle, fpgaId,  writeFSM_curBufIdx);
            endaction

            action
                if (rg_offset_w != rg_preoffset_w) begin
                    //$display("First buffer spill to priority 0");
                    spillTo0_0 <= True;
                    rg_preoffset_w <= rg_offset_w;
                end
	        curBufOut <= writeFSM_curBufIdx + 1;
            endaction

	    action
		if (spillTo0_0 || spillTo0_1) begin
		    Bit#(16) writeFSM_numTotalEntries_partial = bufOutTotalLen[0][writeFSM_curBufIdx] + bufOutTotalLen[1][writeFSM_curBufIdx];
		    Bit#(16) writeFSM_numTotalEntries_partial2 = bufOutTotalLen[2][writeFSM_curBufIdx] + bufOutTotalLen[3][writeFSM_curBufIdx];
		    writeFSM_tails[0][0] <= 0;
		    writeFSM_tails[1][0] <= extend(bufOutTotalLen[0][writeFSM_curBufIdx]);
		    writeFSM_tails[2][0] <= extend(writeFSM_numTotalEntries_partial);
		    writeFSM_tails[3][0] <= extend(writeFSM_numTotalEntries_partial + bufOutTotalLen[2][writeFSM_curBufIdx]);
		    writeFSM_numTotalEntries[0] <= writeFSM_numTotalEntries_partial + writeFSM_numTotalEntries_partial2;
		    for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
		        bufOutTotalLen[i][writeFSM_curBufIdx] <= 0;
		    end
		end
		else begin
		    Vector#(8, Bit#(16)) writeFSM_numEntries_partial;
		    Vector#(8, Bit#(16)) writeFSM_numEntries_partial2;
		    for (Integer i = 0; i < 8; i = i + 1) begin
		        writeFSM_numEntries_partial[i] = bufOutLen[0][i][writeFSM_curBufIdx] + bufOutLen[1][i][writeFSM_curBufIdx];
		        writeFSM_numEntries_partial2[i] = bufOutLen[2][i][writeFSM_curBufIdx] + bufOutLen[3][i][writeFSM_curBufIdx];
		        //$display("%0d: mkWLEngine[%0d]: partial[%0d]: %0d, partial2[%0d]: %0d", cur_cycle, fpgaId, i, writeFSM_numEntries_partial[i], i, writeFSM_numEntries_partial2[i]);
		        writeFSM_tails[0][i] <= 0;
		        writeFSM_tails[1][i] <= extend(bufOutLen[0][i][writeFSM_curBufIdx]);
		        writeFSM_tails[2][i] <= extend(writeFSM_numEntries_partial[i]);
		        writeFSM_tails[3][i] <= extend(writeFSM_numEntries_partial[i] + bufOutLen[2][i][writeFSM_curBufIdx]);
			writeFSM_numTotalEntries[i] <= writeFSM_numEntries_partial[i] + writeFSM_numEntries_partial2[i];
		    end
		    for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
		        for (Integer j = 0; j < 8; j = j + 1) begin
		            bufOutLen[i][j][writeFSM_curBufIdx] <= 0;
		        end
		        bufOutTotalLen[i][writeFSM_curBufIdx] <= 0;
		    end
		end
		
		//Clear the buffer length
		turn <= False;
            endaction

	    action
		for (Integer i = 0; i < `NUM_ENGINES; i = i+1) begin
		    for (Integer j = 0; j < `NUM_PRIORITIES; j = j + 1) begin
		        writeFSM_tails[i][j] <= writeFSM_tails[i][j] + tailPtr_w[j];
		    end
		end

		if (spillTo0_0 || spillTo0_1) begin
		    tailPtr_w[0] <= tailPtr_w[0] + extend(writeFSM_numTotalEntries[0]);
		end
		else begin
		    for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
		        tailPtr_w[i] <= tailPtr_w[i] + extend(writeFSM_numTotalEntries[i]);
			//$display("%0d: mkWLEngine[%0d]: writeFSM_numTotalEntries[%0d]: %0d", cur_cycle, fpgaId, i, writeFSM_numTotalEntries[i]);
		    end
		end
	    endaction
          
            while(!writeFSM_done) seq
                action
                    //if(`DEBUG) $display("%0d: mkWLEngine WriteFSM checking doubleBufOut[%0d][%0d], writeFSM_done: %0d, notEmpty: %0d", cur_cycle, writeFSM_curIdx, writeFSM_curBufIdx, writeFSM_done, doubleBufOut[writeFSM_curIdx][writeFSM_curBufIdx].notEmpty);
		    for (Integer i = 0; i < `NUM_ENGINES; i = i+1) begin
                        if(doubleBufOut[i][writeFSM_curBufIdx].notEmpty) begin
                            WLEntry entry = doubleBufOut[i][writeFSM_curBufIdx].first();
		            Bit#(3) pri = ?;
         	            if (spillTo0_0 || spillTo0_1) begin
		                pri = 0;
         	                //$display("%0d: writeFSM forced to spill to priority 0", cur_cycle);
         	            end 
		            else begin
                                pri = truncate(tpl_1(entry));
		            end
                            //$display("%0d: mkWLEngine spilling to priority %0d", cur_cycle, pri);
                            doubleBufOut[i][writeFSM_curBufIdx].deq();
                            BC_Addr addr = bufferLoc_w[pri] + (writeFSM_tails[i][pri] << `LG_WLENTRY_SIZE);
                            writeMemReqQ[4*i].enq(tagged MemWrite64{addr: addr, gaddr: GaloisAddress{mod: MK_WORKLIST, addr: 1}, data: pack(entry)});
                            writeFSM_outstandingWrites[i].enq(?);
			    writeFSM_tails[i][pri] <= (writeFSM_tails[i][pri] + 1) & maxSize_mask;
			end
                    end

		    function Bool bufOutEmptyF(Integer x) = doubleBufOut[x][writeFSM_curBufIdx].notEmpty;
                    Vector#(`NUM_ENGINES, Bool) bufOutEmpties = genWith(bufOutEmptyF);
                    function Bool isTrueF(Bool x) = x;
                    if (!any(isTrueF, bufOutEmpties)) begin
                        if(`DEBUG) $display("%0d: mkWLEngine: WriteFSM writeFSM done!", cur_cycle);
                        writeFSM_done <= True;
		    end
                endaction
            endseq
            
            while(writeFSM_outstandingWrites[0].notEmpty || writeFSM_outstandingWrites[1].notEmpty || writeFSM_outstandingWrites[2].notEmpty || writeFSM_outstandingWrites[3].notEmpty) seq
                action
                    //$display("%0d: WriteFSM: waiting for %0d writes to complete", cur_cycle, writeFSM_numWrites.getVal());
                    noAction;
                endaction
            endseq

	    action
	        turn <= True;
	        Bit#(16) writeFSM_totalWrites = 0;
		if (spillTo0_0 || spillTo0_1) begin
		    writeFSM_totalWrites = writeFSM_numTotalEntries[0];
		end
		else begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
		        writeFSM_totalWrites = writeFSM_totalWrites + writeFSM_numTotalEntries[i];
		    end
		end
                if(writeFSM_totalWrites > 0) 
                    $display("%0d: mkWLEngine[%0d]: WriteFSM wrote %0d entries", cur_cycle, fpgaId, writeFSM_totalWrites);
            endaction

            if (fpgaId == 0) action
                Bool move = True;
                for (Integer i = 0; i < 7; i = i + 1) begin
                    if (headPtr_w[i] != tailPtr_w[i]) move = False;
                end
                if ((headPtr_w[7] == tailPtr_w[7])) move = False;
		//$display("move is %b, rg_move_engine is %b, rg_cur_pri is %b", move, rg_move_engine, rg_cur_pri);
                if (spillTo0_0) begin
                    spillTo0_0 <= False;
         	    spillTo0_1 <= False;
		    //rg_move_count <= 0;
                end
		else if (rg_move_engine && rg_cur_pri) begin
		    rg_move_engine <= False;
                    rg_offset_w <= rg_offset_w + 1;
                    //spillTo0_1 <= True;
         	    curBufOut <= writeFSM_curBufIdx;
         	    //$display("%0d: FPGA[0] set offset to %0d", cur_cycle, rg_offset_w+1);
		end
		else if (move) begin
		    rg_move_engine <= True;
		end
		turn <= False;
            endaction
            else action
                if (spillTo0_0) begin
                    spillTo0_0 <= False;
         	    spillTo0_1 <= True;
         	    curBufOut <= writeFSM_curBufIdx;
                end
                else if (spillTo0_1) begin
                    spillTo0_1 <= False;
                end
		turn <= False;
            endaction
           
            // Write head and tailPtrs
            action
                //if(`DEBUG) $display("%0d: mkWLEngine[%0d]: readFSM writing new head/tail ptrs, headPtr=%0d, tailPtr=%0d", cur_cycle, fpgaId, headPtr, tailPtr);
                //rg_offset_buf <= rg_offset_w;
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: 1};
                writeMemReqQ[12].enq(tagged MemWrite64{addr: offsetLoc, gaddr: gaddr, data: rg_offset_w});
                //memReqQ[4].enq(tagged MemWrite64{addr: headPtrLoc, gaddr: gaddr, data: extend(headPtr)});
                for (Integer i = 0; i < 8; i = i + 1) begin
                    writeMemReqQ[i].enq(tagged MemWrite64{addr: tailPtrLoc_w[i], gaddr: gaddr, data: extend(tailPtr_w[i])});
		    //$display("%0d: mkWLEngine[%0d]: write tailPtr[%0d]: %0d", cur_cycle, fpgaId, i, tailPtr_w[i]);
                end
            endaction

            // When head/tailPtr writes complete, unlock
            action
                writeMemRespQ[12].deq();
                for (Integer i = 0; i < 8; i = i + 1) begin
                    writeMemRespQ[i].deq();
                end
                writeMemReqQ[12].enq(tagged MemWrite32{addr: lockLoc_w, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: 1}, data: 0});
                writeFSM_curBufIdx <= writeFSM_curBufIdx + 1;
		//$display("%0d: mkWLEngine[%0d]: Change writeFSM_curBufIdx from %0d to %0d", cur_cycle, fpgaId, writeFSM_curBufIdx, writeFSM_curBufIdx+1);
            endaction
            
            action
                writeMemRespQ[12].deq();
                if(`DEBUG) $display("%0d: mkWLEngine[%0d]: WriteFSM done, unlocking!", cur_cycle, fpgaId);
            endaction
       endseq
       );
    
    // Read FSM: Fills one buffer
    Reg#(BC_Addr) readFSM_numEntries <- mkRegU;
    Vector#(`NUM_ENGINES, Reg#(BC_Addr)) readFSM_curEntry <- replicateM(mkRegU);
    Vector#(`NUM_ENGINES, Reg#(BC_Addr)) headPtr_out <- replicateM(mkRegU);
    Reg#(Bit#(10)) readFSM_backOff <- mkRegU;
    Reg#(Bit#(1)) readFSM_buf <- mkRegU;
    //Reg#(Bool) readFSM_success <- mkRegU;
    Reg#(BC_Addr) headPtr_buf <- mkRegU;
    Vector#(`NUM_ENGINES, FIFOF#(Bit#(0))) readFSM_outstandingReads <- replicateM(mkSizedFIFOF(256));
    Reg#(BC_Addr) tailPtr_buf <- mkRegU;
    Reg#(BC_Addr) bufferLoc_buf <- mkRegU;
    Reg#(BC_Addr) headPtrLoc_buf <- mkRegU;
    Reg#(Bit#(3)) rg_pri <- mkRegU;
    //Reg#(Bool) next <- mkRegU;
    //Reg#(Bit#(64)) totalNum <- mkReg(0);
    //Reg#(Bit#(64)) numTrue <- mkReg(0);
    
    for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
        rule readFSM_processReads(readFSM_outstandingReads[i].notEmpty);
            MemResp rsp = readMemRespQ[2+i*4].first();
            readMemRespQ[2+i*4].deq();
            readFSM_outstandingReads[i].deq();
            
            WLEntry entry = unpack(rsp.data);
            //if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM entry priority: %0x, graphId: %0x", cur_cycle, fpgaId, tpl_1(entry), tpl_2(entry));
            doubleBufIn[i][readFSM_buf].enq(entry);
        endrule
    end
    
    let readFSM <- mkFSM(
        seq
            action
                //readFSM_success <= False;
                //next <= False;
                lockFSM_r.start();
                //if (`DEBUG) $display("%0d: mkWLEngine[%0d]: Starting readFSM, filling buf %0d idx %0d...", cur_cycle, fpgaId, readFSM_buf, readFSM_bufIdx);
            endaction
            
            action
                lockFSM_r.waitTillDone();
            endaction

            action
                if (headPtr_r[0] != tailPtr_r[0]) begin
                    rg_pri <= 0;
                    headPtr_buf <= headPtr_r[0];
                    tailPtr_buf <= tailPtr_r[0];
                    bufferLoc_buf <= bufferLoc_r[0];
                    headPtrLoc_buf <= headPtrLoc_r[0];
                end
                else if (headPtr_r[1] != tailPtr_r[1]) begin
                    rg_pri <= 1;
                    headPtr_buf <= headPtr_r[1];
                    tailPtr_buf <= tailPtr_r[1];
                    bufferLoc_buf <= bufferLoc_r[1];
                    headPtrLoc_buf <= headPtrLoc_r[1];
                end
                else if (headPtr_r[2] != tailPtr_r[2]) begin
                    rg_pri <= 2;
                    headPtr_buf <= headPtr_r[2];
                    tailPtr_buf <= tailPtr_r[2];
                    bufferLoc_buf <= bufferLoc_r[2];
                    headPtrLoc_buf <= headPtrLoc_r[2];
                end
                else if (headPtr_r[3] != tailPtr_r[3]) begin
                    rg_pri <= 3;
                    headPtr_buf <= headPtr_r[3];
                    tailPtr_buf <= tailPtr_r[3];
                    bufferLoc_buf <= bufferLoc_r[3];
                    headPtrLoc_buf <= headPtrLoc_r[3];
                end
                else if (headPtr_r[4] != tailPtr_r[4]) begin
                    rg_pri <= 4;
                    headPtr_buf <= headPtr_r[4];
                    tailPtr_buf <= tailPtr_r[4];
                    bufferLoc_buf <= bufferLoc_r[4];
                    headPtrLoc_buf <= headPtrLoc_r[4];
                end
                else if (headPtr_r[5] != tailPtr_r[5]) begin
                    rg_pri <= 5;
                    headPtr_buf <= headPtr_r[5];
                    tailPtr_buf <= tailPtr_r[5];
                    bufferLoc_buf <= bufferLoc_r[5];
                    headPtrLoc_buf <= headPtrLoc_r[5];
                end
                else if (headPtr_r[6] != tailPtr_r[6]) begin
                    rg_pri <= 6;
                    headPtr_buf <= headPtr_r[6];
                    tailPtr_buf <= tailPtr_r[6];
                    bufferLoc_buf <= bufferLoc_r[6];
                    headPtrLoc_buf <= headPtrLoc_r[6];
                end
                else if (headPtr_r[7] != tailPtr_r[7]) begin
                    rg_pri <= 7;
                    headPtr_buf <= headPtr_r[7];
                    tailPtr_buf <= tailPtr_r[7];
                    bufferLoc_buf <= bufferLoc_r[7];
                    headPtrLoc_buf <= headPtrLoc_r[7];
                end
                else begin
                    rg_pri <= 0;
         	    headPtr_buf <= 0;
         	    tailPtr_buf <= 0;
                end
            endaction

            action
                // Get number of entries in FIFO
                BC_Addr size = 0;
                if(headPtr_buf < tailPtr_buf) begin
                    size = tailPtr_buf - headPtr_buf;
                end
                else if(headPtr_buf > tailPtr_buf) begin
                    size = (maxSize - tailPtr_buf) + headPtr_buf;
                end
                
                // Determine number of entries to read
                BC_Addr entries = ?;
                if(size > (`NUM_ENGINES*`WLENGINE_BUFIN_SIZE)) begin
                    entries = (`NUM_ENGINES*`WLENGINE_BUFIN_SIZE);
                end
                else begin
                    entries = size;
                end
                if (`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM headPtr: %0d, tailPtr: %0d, WL size: %0d, reading %0d entries", cur_cycle, fpgaId, headPtr_buf, tailPtr_buf, size, entries);
                readFSM_numEntries <= entries;
                for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
                    readFSM_curEntry[i] <= fromInteger(i);
                end

                if (rg_preoffset_r != rg_offset_r) begin
                    for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
         	       pri_buffer[i][0] <= 0;
         	       pri_buffer[i][1] <= 0;
         	    end
         	    rg_preoffset_r <= rg_offset_r;
                end
                else if (entries > 0) begin
		    for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
                        pri_buffer[i][readFSM_buf] <= rg_pri;
	            end
                end
            endaction

            // Read entries and send to buffer
            if(readFSM_numEntries > 0) seq
                // Write head and tailPtrs
                action
                    GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: 0};
	    	    BC_Addr newHeadPtr = headPtr_buf + readFSM_numEntries;
                    readMemReqQ[2].enq(tagged MemWrite64{addr: headPtrLoc_buf, gaddr: gaddr, data: extend(newHeadPtr)});
                    //$display("%0d: mkWLEngine[%0d]: ReadFSM writing new head/tail ptrs, headPtr=%0d, tailPtr=%0d", cur_cycle, fpgaId, newHeadPtr, tailPtr);
                endaction

                // When head/tailPtr writes complete, unlock
                action
                    readMemRespQ[2].deq();
                    readMemReqQ[9].enq(tagged MemWrite64{addr: lockLoc_r, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: 0}, data: 0});
		    for (Integer i = 0; i < `NUM_ENGINES; i = i+1) begin
		        headPtr_out[i] <= headPtr_buf + fromInteger(i);
		    end
                    //memRespQ[3].deq();
                endaction

                while(readFSM_curEntry[`NUM_ENGINES-1] < readFSM_numEntries) seq
                    action
                        //if (`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM Reading entry %0d of %0d, writing to buf%0d[%0d]...", cur_cycle, fpgaId, readFSM_curEntry, readFSM_numEntries, readFSM_bufIdx, readFSM_buf);
			for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
			    BC_Addr head = headPtr_out[i] & maxSize_mask;
                            BC_Addr addr = bufferLoc_buf + (head << `LG_WLENTRY_SIZE);
                            readMemReqQ[2+i*4].enq(tagged MemRead64{addr: addr, gaddr: GaloisAddress{mod: MK_WORKLIST, addr: 0}});
                            readFSM_outstandingReads[i].enq(?);
                        
                            readFSM_curEntry[i] <= readFSM_curEntry[i] + `NUM_ENGINES;
			    headPtr_out[i] <= headPtr_out[i] + `NUM_ENGINES;
			end
                    endaction
                endseq

		action
		    if (readFSM_curEntry[0] < readFSM_numEntries) begin
		        BC_Addr head = headPtr_out[0] & maxSize_mask;
                        BC_Addr addr = bufferLoc_buf + ((head) << `LG_WLENTRY_SIZE);
                        readMemReqQ[2].enq(tagged MemRead64{addr: addr, gaddr: GaloisAddress{mod: MK_WORKLIST, addr: 0}});
                        readFSM_outstandingReads[0].enq(?);
			headPtr_buf <= headPtr_out[0] & maxSize_mask;
		    end
		endaction
                
		action
		    if (readFSM_curEntry[1] < readFSM_numEntries) begin
		        BC_Addr head = headPtr_out[1] & maxSize_mask;
                        BC_Addr addr = bufferLoc_buf + ((head) << `LG_WLENTRY_SIZE);
                        readMemReqQ[6].enq(tagged MemRead64{addr: addr, gaddr: GaloisAddress{mod: MK_WORKLIST, addr: 0}});
                        readFSM_outstandingReads[1].enq(?);
			headPtr_buf <= headPtr_out[1] & maxSize_mask;
		    end
		endaction
                
		action
		    if (readFSM_curEntry[2] < readFSM_numEntries) begin
		        BC_Addr head = headPtr_out[2] & maxSize_mask;
                        BC_Addr addr = bufferLoc_buf + ((head) << `LG_WLENTRY_SIZE);
                        readMemReqQ[10].enq(tagged MemRead64{addr: addr, gaddr: GaloisAddress{mod: MK_WORKLIST, addr: 0}});
                        readFSM_outstandingReads[2].enq(?);
			headPtr_buf <= headPtr_out[2] & maxSize_mask;
		    end
		endaction
                
                while(readFSM_outstandingReads[0].notEmpty || readFSM_outstandingReads[1].notEmpty || readFSM_outstandingReads[2].notEmpty || readFSM_outstandingReads[3].notEmpty) seq
                    action
                        //$display("%0d: ReadFSM: waiting for %0d reads to complete", cur_cycle, readFSM_numReads.getVal());
                        noAction;
                    endaction
                endseq
                
                // Write head and tailPtrs
                action
                    headPtr_r[rg_pri] <= headPtr_buf;
                    if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM done, unlocking!", cur_cycle, fpgaId);
                    $display("%0d: mkWLEngine[%0d]: ReadFSM read %0d entries for idx buf[%0d]", cur_cycle, fpgaId, readFSM_numEntries, readFSM_buf);
                    readMemRespQ[9].deq();
                    //readFSM_success <= True;
                endaction
            endseq
            else seq
                // No data, unlock and stall avoid needless contention
                action
                    readMemReqQ[8].enq(tagged MemWrite32{addr: lockLoc_r, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: 0}, data: 0});
                    //$display("%0d: mkWLEngine[%0d]: ReadFSM done with nothing read, unlocking!", cur_cycle, fpgaId);
                endaction
                
                action
                    readMemRespQ[8].deq();
                endaction
                
                //if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM nothing to read, stalling for %0d cycles", cur_cycle, fpgaId, `WLENGINE_BACKOFF);
                //readFSM_backOff <= 0;
                //while(readFSM_backOff < `WLENGINE_BACKOFF) seq
                //    action
                //        readFSM_backOff <= readFSM_backOff + 1;
                //    endaction
                //endseq
                
                action
                    noAction;
                endaction

            endseq
        endseq
    );
    
    rule startRead(started && readFSM.done);
        function Bool bufInEmptyF0(Integer x) = !doubleBufIn[x][0].notEmpty;
        function Bool bufInEmptyF1(Integer x) = !doubleBufIn[x][1].notEmpty;
        function Bool isTrueF(Bool x) = x;
        Vector#(`NUM_ENGINES, Bool) bufInEmpties0 = genWith(bufInEmptyF0);
        Vector#(`NUM_ENGINES, Bool) bufInEmpties1 = genWith(bufInEmptyF1);
        //let elem0 = findElem(True, bufInEmpties0);
        //let elem1 = findElem(True, bufInEmpties1);
	let empty0 = all(isTrueF, bufInEmpties0);
	let empty1 = all(isTrueF, bufInEmpties1);
        if(empty0) begin
            //Bit#(`LG_NUM_ENGINES) idx = pack(fromMaybe(?, elem0));
            //$display("BufInEmpties: %b, Empty idx: %0d", bufInEmpties0, idx);
            //readFSM_bufIdx <= idx;
            readFSM_buf <= 0;
            readFSM.start();
        end
        else if(empty1) begin
            //Bit#(`LG_NUM_ENGINES) idx = pack(fromMaybe(?, elem1));
            //$display("BufInEmpties: %b, Empty idx: %0d", idx);
            //readFSM_bufIdx <= idx;
            readFSM_buf <= 1;
            readFSM.start();
        end
    endrule
    
    
    Reg#(Bit#(16)) triggerWriteFSM_timeout <- mkRegU;
    Reg#(Bit#(1)) triggerWriteFSM_lastIdx <- mkRegU;
    rule triggerWriteFSM(started && writeFSM.done);
        function Bool bufOutFullF(Integer x) = !doubleBufOut[x][writeFSM_curBufIdx].notFull;
        function Bool bufOutEmptyF(Integer x) = doubleBufOut[x][writeFSM_curBufIdx].notEmpty;
        Vector#(`NUM_ENGINES, Bool) bufOutFulls = genWith(bufOutFullF);
        Vector#(`NUM_ENGINES, Bool) bufOutEmpties = genWith(bufOutEmptyF);
        function Bool isTrueF(Bool x) = x;
        if(any(isTrueF, bufOutEmpties)) begin
            //$display("%0d: WLEngine[%0d] triggerWriteFSM buffer:%0d notEmpty: %0b", cur_cycle, fpgaId, writeFSM_curBufIdx, bufOutEmpties);
            writeFSM.start();
            triggerWriteFSM_timeout <= 0;
	    rg_cur_pri <= cur_pri;
        end
        else begin
            //$display("WLEngine triggerWriteFSM curBuf: %0d full: %b", writeFSM_curBufIdx, bufOutFulls);
            if(triggerWriteFSM_timeout > `WRITEFSM_TIMEOUT) begin
                triggerWriteFSM_timeout <= 0;
                //triggerWriteFSM_lastIdx <= triggerWriteFSM_lastIdx + 1;
                //writeFSM_curBufIdx <= triggerWriteFSM_lastIdx;
                writeFSM.start();
		rg_cur_pri <= cur_pri;
                //$display("%0d: Triggering WriteFSM...", cur_cycle);
            end
            else begin
                writeFSM_curBufIdx <= writeFSM_curBufIdx + 1;
		//$display("%0d: Change writeFSM_curBufIdx from %0d to %0d", cur_cycle, writeFSM_curBufIdx, writeFSM_curBufIdx+1);
                triggerWriteFSM_timeout <= triggerWriteFSM_timeout + 1;
		spillTo0_0 <= False;
		//rg_offset_buf <= rg_offset_w;
            end
        end
    endrule
    
    rule setSpillTo0_1(started && writeFSM.done);
        function Bool bufOutEmptyF(Integer x) = !doubleBufOut[x][writeFSM_curBufIdx].notEmpty;
        function Bool isTrueF(Bool x) = x;
        Vector#(`NUM_ENGINES, Bool) bufOutEmpties = genWith(bufOutEmptyF);
	if (all(isTrueF, bufOutEmpties)) begin
	    spillTo0_1 <= False;
	end
    endrule
    
    for(Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
        
        rule setCurBufIn(started);
            if(((curBufIn[i] == 0) && !doubleBufIn[i][0].notEmpty && doubleBufIn[i][1].notEmpty) ||
               ((curBufIn[i] == 1) && !doubleBufIn[i][1].notEmpty && doubleBufIn[i][0].notEmpty)) begin
               // Flip bit
               curBufIn[i] <= 1 + curBufIn[i];
            end
            
            if((curBufIn[i] == 0) && doubleBufIn[i][0].notEmpty) begin
                WLEntry pkt = doubleBufIn[i][0].first();
                if(`DEBUG) $display("%0d: mkWLEngine curBufIn[0] packet %x streaming to WorkListFIFOF", cur_cycle, pkt);
                doubleBufIn[i][0].deq();
                respQ[i].enq(pkt);
		pri_respQ[i] <= pri_buffer[i][0];
            end
            if((curBufIn[i] == 1) && doubleBufIn[i][1].notEmpty) begin
                WLEntry pkt = doubleBufIn[i][1].first();
                if(`DEBUG) $display("%0d: mkWLEngine curBufIn[1] packet %x streaming to WorkListFIFOF", cur_cycle, pkt);
                doubleBufIn[i][1].deq();
                respQ[i].enq(pkt);
		pri_respQ[i] <= pri_buffer[i][1];
            end
        endrule
        
        rule streamToBuf(!turn);
            WLEntry entry = reqQ[i].first();
            Bit#(3) pri = truncate(tpl_1(entry));
            if (curBufOut == 0) begin
	        if (doubleBufOut[i][0].notFull) begin
                    reqQ[i].deq();
                    //$display("%0d: mkWLEngine[%0d] reqQ->doubleBuf[%0d][0], id: %0d, bufLen[%0d][%0d][0]: %0d", cur_cycle, fpgaId, i, tpl_2(entry), i, pri, bufOutLen[i][pri][0]);
                    doubleBufOut[i][0].enq(entry);
		    bufOutLen[i][pri][0] <= bufOutLen[i][pri][0] + 1;
		    bufOutTotalLen[i][0] <= bufOutTotalLen[i][0] + 1;
		end
		//else if (doubleBufOut[i][1].notFull) begin
		//    reqQ[i].deq();
		//    doubleBufOut[i][1].enq(entry);
		//end
            end
	    else begin
	        if (doubleBufOut[i][1].notFull) begin
	            reqQ[i].deq();
                    //$display("%0d: mkWLEngine[%0d] reqQ->doubleBuf[%0d][1], id: %0d, bufLen[%0d][%0d][1]: %0d", cur_cycle, fpgaId, i, tpl_2(entry), i, pri, bufOutLen[i][pri][1]);
		    doubleBufOut[i][1].enq(entry);
		    bufOutLen[i][pri][1] <= bufOutLen[i][pri][1] + 1;
		    bufOutTotalLen[i][1] <= bufOutTotalLen[i][1] + 1;
	        end
                //else begin
                //    reqQ[i].deq();
                //    //$display("mkWLEngine reqQ->doubleBuf[%0d][%0d]", i, curBufOut+1);
                //    doubleBufOut[i][0].enq(entry);
		//end
            end
        endrule        
    end

    rule drainFIFOs(!started && readFSM.done && writeFSM.done);
        for (Integer i = 0; i < `NUM_ENGINES; i = i+1) begin
	    if (doubleBufIn[i][0].notEmpty)
	        doubleBufIn[i][0].deq();
	    if (doubleBufIn[i][1].notEmpty)
	        doubleBufIn[i][1].deq();
	    if (doubleBufOut[i][0].notEmpty)
	        doubleBufOut[i][0].deq();
	    if (doubleBufOut[i][1].notEmpty)
	        doubleBufOut[i][1].deq();
	    if (reqQ[i].notEmpty)
	        reqQ[i].deq();
	end
        for (Integer i = 0; i < 16; i = i+1) begin
	    if (readMemRespQ[i].notEmpty)
	        readMemRespQ[i].deq();
	    if (writeMemRespQ[i].notEmpty)
	        writeMemRespQ[i].deq();
	end
    endrule

    Reg#(Bool) readIsDone <- mkRegU;
    Reg#(Bool) writeIsDone <- mkRegU;
    rule calcDone(started);
        // No readFSM.done since it's constantly triggered whenever any input buffers are empty!
        let cycle <- cur_cycle;
        Bool isDone = True;
        for(Integer i = 0; i < `NUM_ENGINES; i=i+1) begin
            if(reqQ[i].notEmpty || respQ[i].notEmpty || doubleBufIn[i][0].notEmpty || doubleBufIn[i][1].notEmpty || doubleBufOut[i][0].notEmpty || doubleBufOut[i][1].notEmpty || readFSM_outstandingReads[i].notEmpty) begin
                isDone = False;
            end
            //if(cycle % 8192 == 0) $display("%0d: Lane %0d notEmpties: reqQ:%b respQ:%b bufIn0:%b bufIn1:%b, doubBufOut0:%b, doubBufOut1:%b, memReq:%b, memResp:%b", cur_cycle, i, reqQ[i].notEmpty, respQ[i].notEmpty, doubleBufIn[i][0].notEmpty, doubleBufIn[i][1].notEmpty, doubleBufOut[i][0].notEmpty, doubleBufOut[i][1].notEmpty,  memReqQ[i].notEmpty, memRespQ[i].notEmpty);
        end
     
        Bool isDone2 = True;
        for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
            if (headPtr_r[i] != tailPtr_r[i]) begin
                isDone2 = False;
            end
        end
	readIsDone <= isDone2;
        
        Bool isDone3 = True;
        for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
            if (headPtr_w[i] != tailPtr_w[i]) begin
                isDone3 = False;
            end
        end
	writeIsDone <= isDone3;
        
        done <= readIsDone && writeIsDone && isDone;
	//$display("WLEngine[%0d]: ptr done is %0d, empty done is %0d", fpgaId, (headPtr == tailPtr), isDone);
        //if(cycle == 10000000) $display("%0d: wlEngine done calculation: HeadPtr:%0d, tailPtr:%0d, writeDone:%0b, readDone:%0b, isDone:%0b", cur_cycle, headPtr, tailPtr, writeFSM.done, readFSM.done, isDone);
        //if (cycle == 10000) begin
        //    for (Integer i = 0; i < 8; i = i + 1) begin
        //        $display("bufferLoc[%0d] is %0x", i, bufferLoc[i]);
        //    end
        //end
    endrule
    
    method isDone();
        return done;
    endmethod
    
    method Action init(BC_AEId fpgaid, BC_Addr lockloc, BC_Addr headptrloc, BC_Addr tailptrloc, BC_Addr maxsize, BC_Addr bufferloc, BC_Addr offsetloc);
        //$display("%0d: mkWLEngine[%0d]: INIT, fpgaid: %0x, lockLoc:%0x, headPtrLoc: %0x, tailPtrLoc: %0x, maxSize: %0d, bufferLoc: %0x", cur_cycle, fpgaid, fpgaid, lockloc, headptrloc, tailptrloc, maxsize, bufferloc);
        fpgaId <= fpgaid;
        lockLoc_w <= lockloc;
        lockLoc_r <= lockloc + 8;
	offsetLoc <= offsetloc;
	rg_offset_r <= 0;
	rg_offset_w <= 0;
        rg_preoffset_r <= 0;
        rg_preoffset_w <= 0;
	//rg_move_read <= 0;
	rg_move_engine <= False;
	turn <= False;
	readIsDone <= False;
	writeIsDone <= False;
	done <= False;
        //headPtrLoc <= headptrloc;
        //tailPtrLoc <= tailptrloc;
	//headPtr <= 0;
	//tailPtr <= 1;
        maxSize <= maxsize >> `LG_NUM_PRIORITIES;
	maxSize_mask <= maxsize - 1;
        let incr = (maxsize >> `LG_NUM_PRIORITIES) << 3;
        let incr_2 = incr << 1;
        let incr_4 = incr << 2;
        let incr_8 = incr << 3;
        bufferBaseLoc[0] <= bufferloc;
        bufferBaseLoc[1] <= bufferloc + incr;
        bufferBaseLoc[2] <= bufferloc + incr_2;
        bufferBaseLoc[3] <= bufferloc + incr_2 + incr;
        bufferBaseLoc[4] <= bufferloc + incr_4;
        bufferBaseLoc[5] <= bufferloc + incr_4 + incr;
        bufferBaseLoc[6] <= bufferloc + incr_4 + incr_2;
        bufferBaseLoc[7] <= bufferloc + incr_8 - incr;
        for (Integer i = 0; i < `NUM_PRIORITIES; i=i+1) begin
            //let incr = (maxsize >> `LG_NUM_PRIORITIES) << 3;
            //let buffLoc = bufferloc + incr * fromInteger(i);
            //bufferLoc[i] <= buffLoc;
            //$display("bufferLoc[%0d] is %0x", i, buffLoc);
            headPtrBaseLoc[i] <= headptrloc + 8 * fromInteger(i);
            tailPtrBaseLoc[i] <= tailptrloc + 8 * fromInteger(i);
            headPtr_r[i] <= 0;
            if (i == 0) tailPtr_r[i] <= 1;
            else tailPtr_r[i] <= 0;
        end
        started <= True;
        
        
        writeFSM_curIdx <= 0;
        writeFSM_curBufIdx <= 0;
        triggerWriteFSM_timeout <= 0;
	curBufOut <= 0;
        
        for(Integer i = 0; i < `NUM_ENGINES; i=i+1) begin
            curBufIn[i] <= 0;
	    bufOutTotalLen[i][0] <= 0;
	    bufOutTotalLen[i][1] <= 0;
	    for (Integer j = 0; j < `NUM_PRIORITIES; j=j+1) begin
	        bufOutLen[i][j][0] <= 0;
		bufOutLen[i][j][1] <= 0;
	    end
        end
    endmethod

    method Action stop;
        started <= False;
        //$display("numFalse is %0d, numTrue is %0d", totalNum-numTrue, numTrue);
    endmethod

    method Bit#(64) getOffset();
        return max(rg_offset_w, rg_offset_r);
    endmethod

    interface streamIn = map(toPut, reqQ);
    interface streamOut = map(toGet, respQ);
    interface memReq = map(toGet, memReqQ);
    interface memResp = map(toPut, memRespQ);
    interface priority_ifc = pri_respQ;
    interface cur_pri_ifc = cur_pri;
endmodule


endpackage
