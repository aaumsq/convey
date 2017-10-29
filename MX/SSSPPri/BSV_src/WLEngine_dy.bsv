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
`define WLENGINE_BACKOFF 2047
`define WRITEFSM_TIMEOUT 128

(* synthesize *)
module mkWLEngine(WLEngine);
    // memOffset[0]: lock bit
    // memOffset[1]: headPtr
    // memOffset[2]: tailPtr
    // memOffset[3]: workList size
    // memOffset[4]: start of data
    
    Reg#(BC_AEId) fpgaId <- mkReg(0);
    Reg#(BC_Addr) lockLoc <- mkRegU;
    Reg#(Bit#(64)) rg_offset <- mkRegU;
    Reg#(Bit#(64)) rg_offset_w <- mkRegU;
    Reg#(Bit#(64)) rg_preoffset_r <- mkRegU;
    Reg#(Bit#(64)) rg_preoffset_w <- mkRegU;
    Reg#(BC_Addr) offsetLoc <- mkRegU;
    //Reg#(Bit#(3)) rg_move_count <- mkRegU;
    Reg#(Bit#(3)) rg_move_read <- mkRegU;
    Reg#(Bool) rg_move_engine <- mkRegU;
    //Vector#(`NUM_ENGINES, Reg#(Bit#(3))) rg_move <- replicateM(mkRegU);
    Reg#(Bool) rg_cur_pri <- mkRegU;
    Reg#(Bool) cur_pri <- mkRegU;

    //Reg#(BC_Addr) headPtr <- mkRegU;
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) headPtr <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) headPtrLoc <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) headPtrBaseLoc <- replicateM(mkRegU);
    
    //Reg#(BC_Addr) tailPtr <- mkRegU;
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) tailPtr <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) tailPtrLoc <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) tailPtrBaseLoc <- replicateM(mkRegU);
    
    Reg#(BC_Addr) maxSize <- mkRegU;
    //Reg#(BC_Addr) bufferLoc <- mkRegU;
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) bufferLoc <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) bufferBaseLoc <- replicateM(mkRegU);

    Vector#(`NUM_ENGINES, Vector#(2, Reg#(Bit#(3)))) pri_buffer <- replicateM(replicateM(mkRegU));
    Vector#(`NUM_ENGINES, Reg#(Bit#(3))) pri_respQ <- replicateM(mkRegU);
    
    Reg#(Bool) started <- mkReg(False);
    Reg#(Bool) done <- mkReg(False);
    
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) reqQ <- replicateM(mkFIFOF);
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) respQ <- replicateM(mkFIFOF);
    
    Vector#(16, FIFOF#(MemReq)) memReqQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) memRespQ <- replicateM(mkFIFOF);

    Vector#(16, FIFOF#(MemReq)) lockMemReqQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) lockMemRespQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemReq)) rwMemReqQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) rwMemRespQ <- replicateM(mkFIFOF);
    
    Vector#(`NUM_ENGINES, Vector#(2, FIFOF#(WLEntry))) doubleBufIn <- replicateM(replicateM(mkSizedBufBRAMFIFOF(`WLENGINE_BUFIN_SIZE)));
    Vector#(`NUM_ENGINES, Reg#(Bit#(1))) curBufIn <- replicateM(mkRegU);

    Vector#(`NUM_ENGINES, Vector#(2, FIFOF#(WLEntry))) doubleBufOut <- replicateM(replicateM(mkSizedBufBRAMFIFOF(`WLENGINE_BUFOUT_SIZE)));
    Vector#(`NUM_ENGINES, Reg#(Bit#(1))) curBufOut <- replicateM(mkRegU);

    for (Integer i = 0; i < 16; i = i + 1) begin
        (* descending_urgency = "lockToMem, rwToMem" *)
        rule lockToMem;
	    MemReq req = lockMemReqQ[i].first();
	    lockMemReqQ[i].deq();
	    memReqQ[i].enq(req);
	endrule

        rule rwToMem;
	    MemReq req = rwMemReqQ[i].first();
	    rwMemReqQ[i].deq();
	    memReqQ[i].enq(req);
	endrule

	rule memToFSM;
	    MemResp pkt = memRespQ[i].first();
	    memRespQ[i].deq();

	    if (pkt.gaddr.addr == 0)
	        lockMemRespQ[i].enq(pkt);
	    else if (pkt.gaddr.addr == 1)
	        rwMemRespQ[i].enq(pkt);
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
    
    Reg#(Bit#(32)) lock_lockData <- mkRegU;
    Reg#(Bit#(16)) lockFSM_backOff <- mkRegU;
    let lockFSM <- mkFSM(
        seq
            action
                lock_lockData <= 32'b1;
            endaction
            
            // Obtain global worklist lock
            while(lock_lockData == 32'd1) seq
                action
                    if (`DEBUG) $display("%0d: mkWLEngine[%0d]: Reading lock bit at addr: %0x...", cur_cycle, fpgaId, lockLoc);
                    lockMemReqQ[0].enq(tagged MemCAS32{addr: lockLoc, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: 0}, cmpVal: 0, swapVal: 1});
                endaction
            
                action
                    MemResp rsp = lockMemRespQ[0].first();
                    lockMemRespQ[0].deq();
                    lock_lockData <= truncate(rsp.data);
                    if (`DEBUG) $display("%0d: mkWLEngine[%0d]: old lock bit = %0d", cur_cycle, fpgaId, rsp.data);
                    // Data is the old data, so if 1 then it failed
                    if(truncate(rsp.data) == 32'd1) begin
                        //if(`DEBUG) $display("%0d: mkWLEngine[%0d]:  Worklist is locked, retry...", cur_cycle, fpgaId);
                    end
                endaction

                if(lock_lockData == 32'd1) seq
                    if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM nothing to read, stalling for %0d cycles", cur_cycle, fpgaId, `WLENGINE_BACKOFF);
                    lockFSM_backOff <= 0;
                    while(lockFSM_backOff < 64) seq
                        action
                          lockFSM_backOff <= lockFSM_backOff + 1;
                        endaction
                    endseq
                endseq

            endseq
           
	    // Get offset
            action
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: 0};
	        lockMemReqQ[0].enq(tagged MemRead64{addr: offsetLoc, gaddr: gaddr});
	    endaction
	
            // Get updated head and tail pointers
            action
                //$display("%0d: mkWLEngine[%0d]: getting updated head/tail ptrs", cur_cycle, fpgaId);
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: 0};
	        MemResp offsetRsp = lockMemRespQ[0].first();
	        lockMemRespQ[0].deq();
                rg_offset <= offsetRsp.data;
	    endaction

	    action
		$display("%0d: offset is %0d", cur_cycle, rg_offset);
	        if (rg_offset[2:0] == 3'd0) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc[i] <= headPtrBaseLoc[i];
	                tailPtrLoc[i] <= tailPtrBaseLoc[i];
	                bufferLoc[i] <= bufferBaseLoc[i];
	            end
	        end
	        else if (rg_offset[2:0] == 3'd1) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc[i] <= headPtrBaseLoc[(i+7)%8];
	                tailPtrLoc[i] <= tailPtrBaseLoc[(i+7)%8];
	                bufferLoc[i] <= bufferBaseLoc[(i+7)%8];
	            end
	        end
	        else if (rg_offset[2:0] == 3'd2) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc[i] <= headPtrBaseLoc[(i+6)%8];
	                tailPtrLoc[i] <= tailPtrBaseLoc[(i+6)%8];
	                bufferLoc[i] <= bufferBaseLoc[(i+6)%8];
	            end
	        end
	        else if (rg_offset[2:0] == 3'd3) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc[i] <= headPtrBaseLoc[(i+5)%8];
	                tailPtrLoc[i] <= tailPtrBaseLoc[(i+5)%8];
	                bufferLoc[i] <= bufferBaseLoc[(i+5)%8];
	            end
	        end
	        else if (rg_offset[2:0] == 3'd4) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc[i] <= headPtrBaseLoc[(i+4)%8];
	                tailPtrLoc[i] <= tailPtrBaseLoc[(i+4)%8];
	                bufferLoc[i] <= bufferBaseLoc[(i+4)%8];
	            end
	        end
	        else if (rg_offset[2:0] == 3'd5) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc[i] <= headPtrBaseLoc[(i+3)%8];
	                tailPtrLoc[i] <= tailPtrBaseLoc[(i+3)%8];
	                bufferLoc[i] <= bufferBaseLoc[(i+3)%8];
	            end
	        end
	        else if (rg_offset[2:0] == 3'd6) begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc[i] <= headPtrBaseLoc[(i+2)%8];
	                tailPtrLoc[i] <= tailPtrBaseLoc[(i+2)%8];
	                bufferLoc[i] <= bufferBaseLoc[(i+2)%8];
	            end
	        end
	        else begin
	            for (Integer i = 0; i < 8; i = i + 1) begin
	                headPtrLoc[i] <= headPtrBaseLoc[(i+1)%8];
	                tailPtrLoc[i] <= tailPtrBaseLoc[(i+1)%8];
	                bufferLoc[i] <= bufferBaseLoc[(i+1)%8];
	            end
	        end
	    endaction

	    action
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: 0};
                for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
                    lockMemReqQ[i].enq(tagged MemRead64{addr: headPtrLoc[i], gaddr: gaddr});
                end
                for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
                    lockMemReqQ[i+8].enq(tagged MemRead64{addr: tailPtrLoc[i], gaddr: gaddr});
                end
            endaction
            
            action
                for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
                    MemResp headRsp = lockMemRespQ[i].first();
                    lockMemRespQ[i].deq();
                    headPtr[i] <= truncate(pack(headRsp.data));
                end
                for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
                    MemResp tailRsp = lockMemRespQ[i+8].first();
                    lockMemRespQ[i+8].deq();
                    tailPtr[i] <= truncate(pack(tailRsp.data));
                end
                //if(`DEBUG) $display("%0d: mkWLEngine[%0d]: headPtr: %0x, tailPtr: %0x", cur_cycle, fpgaId, headRsp.data, tailRsp.data);
            endaction
  
        endseq
    );
    
    // Write FSM: Writes all full buffers
    FIFOF#(Bit#(5)) writeFSM_writeQ <- mkSizedFIFOF(16);
    Reg#(Bit#(`LG_NUM_ENGINES)) writeFSM_curIdx <- mkRegU;
    Reg#(Bit#(1)) writeFSM_curBufIdx <- mkRegU;
    Reg#(Bit#(1)) newBufIdx <- mkRegU;
    Reg#(Bit#(16)) writeFSM_totalWrites <- mkRegU;
    Reg#(Bool) writeFSM_done <- mkRegU;
    Vector#(`NUM_PRIORITIES, Reg#(BC_Addr)) writeFSM_wlSize <- replicateM(mkRegU);
    Vector#(`NUM_PRIORITIES, Reg#(Bool)) writeFSM_wlFull <- replicateM(mkRegU);
    //Reg#(BC_Addr) writeFSM_maxSizeMinusOne <- mkRegU;
    Reg#(BC_Addr) writeFSM_maxSizeMinusTwo <- mkRegU;
    FIFOF#(Bit#(0)) writeFSM_outstandingWrites <- mkSizedFIFOF(128);
    Reg#(Bool) spillTo0_0 <- mkReg(False);
    Reg#(Bool) spillTo0_1 <- mkReg(False);
    
    rule writeFSM_catchWriteAcks(writeFSM_outstandingWrites.notEmpty);
        rwMemRespQ[12].deq();
        writeFSM_outstandingWrites.deq();
    endrule

    let writeFSM <- mkFSM(
        seq
            action
                $display("%0d: mkWLEngine[%0d]: WriteFSM START", cur_cycle, fpgaId);
                lockFSM.start();
                writeFSM_curIdx <= 0;
                writeFSM_totalWrites <= 0;
                writeFSM_done <= False;
            endaction
            
            action
                //if(`DEBUG) $display("%0d: mkWLEngine: WriteFSM lock + head/tailPtr obtained! headPtr=%0d, tailPtr=%0d", cur_cycle, headPtr, tailPtr);
                lockFSM.waitTillDone();
            endaction
            
            //action
            //    let writeFSM_maxsizeminustwo = maxSize - 2;
            //    for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
            //        let wlsize = getWLSize(headPtr[i], tailPtr[i], maxSize);
            //        writeFSM_wlSize[i] <= wlsize;
            //        writeFSM_wlFull[i] <= (wlsize == writeFSM_maxsizeminustwo);
            //    end
            //    //writeFSM_maxSizeMinusOne <= maxSize-1;
            //    writeFSM_maxSizeMinusTwo <= writeFSM_maxsizeminustwo;
            //endaction
            action
            if (rg_offset != rg_preoffset_w) begin
                $display("First buffer spill to priority 0");
                spillTo0_0 <= True;
                rg_preoffset_w <= rg_offset;
            end
            endaction
            
            while(!writeFSM_done) seq
                action
                    if(`DEBUG) $display("%0d: mkWLEngine WriteFSM checking doubleBufOut[%0d][%0d], writeFSM_done: %0d, notEmpty: %0d", cur_cycle, writeFSM_curIdx, writeFSM_curBufIdx, writeFSM_done, doubleBufOut[writeFSM_curIdx][writeFSM_curBufIdx].notEmpty);
                    if(writeFSM_totalWrites > `WLENGINE_MAX_WRITES) begin
                        writeFSM_done <= True;
                    end
                    else if(doubleBufOut[writeFSM_curIdx][writeFSM_curBufIdx].notEmpty) begin
                        WLEntry entry = doubleBufOut[writeFSM_curIdx][writeFSM_curBufIdx].first();
			Bit#(3) pri = ?;
         	        if (spillTo0_0 || spillTo0_1) begin
			    pri = 0;
         		    $display("%0d: writeFSM spill to priority 0", cur_cycle);
         	        end 
			else begin
                            pri = truncate(tpl_1(entry));
			end
                        //$display("%0d: mkWLEngine spilling to priority %0d", cur_cycle, pri);
                        //if(writeFSM_wlSize[pri] < writeFSM_maxSizeMinusOne) begin
                        //if(!writeFSM_wlFull[pri]) begin
                            doubleBufOut[writeFSM_curIdx][writeFSM_curBufIdx].deq();
                           
                            //BC_Addr addr = bufferLoc + (tailPtr << `LG_WLENTRY_SIZE);
                            BC_Addr addr = bufferLoc[pri] + (tailPtr[pri] << `LG_WLENTRY_SIZE);
                            rwMemReqQ[12].enq(tagged MemWrite64{addr: addr, gaddr: GaloisAddress{mod: MK_WORKLIST, addr: 1}, data: pack(entry)});
                            writeFSM_outstandingWrites.enq(?);
                            writeFSM_totalWrites <= writeFSM_totalWrites + 1;
                           
                            //if(`DEBUG) $display("mkWLEngine: WriteFSM headPtr=%0d, tailPtr=%0d, packet: %x", headPtr, tailPtr, entry);
                            if (tailPtr[pri] == (maxSize - 1))
                                tailPtr[pri] <= 0;
                            else
                                tailPtr[pri] <= tailPtr[pri] + 1;
                            //tailPtr_incr(entry);
                            //writeFSM_wlSize[pri] <= writeFSM_wlSize[pri] + 1;
                            //writeFSM_wlFull[pri] <= (writeFSM_wlSize[pri] == writeFSM_maxSizeMinusTwo);
                        //end
                        //else begin
                        //    if(`DEBUG) $display("mkWLEngine WorkList is FULL! Breaking...");
                        //    writeFSM_done <= True;
                        //end
                    end
                    else begin
                        if(`DEBUG) $display("mkWLEngine WriteFSM done with idx %0d", writeFSM_curIdx);
                        if(writeFSM_curIdx != fromInteger(`NUM_ENGINES-1)) begin
                            writeFSM_curIdx <= writeFSM_curIdx + 1;
                            if(`DEBUG) $display("mkWLEngine WriteFSM incrementing writeFSM_curIdx: %0d", writeFSM_curIdx+1);
                        end
                        else begin
                            if(`DEBUG) $display("%0d: mkWLEngine: WriteFSM writeFSM done!", cur_cycle);
                            writeFSM_done <= True;
                        end
                    end
                endaction
            endseq
            
            while(writeFSM_outstandingWrites.notEmpty) seq
                action
                    //$display("%0d: WriteFSM: waiting for %0d writes to complete", cur_cycle, writeFSM_numWrites.getVal());
                    noAction;
                endaction
            endseq

            if (fpgaId == 0) action
                Bool move = True;
                for (Integer i = 0; i < 7; i = i + 1) begin
                    if (headPtr[i] != tailPtr[i]) move = False;
                end
                if ((headPtr[7] == tailPtr[7])) move = False;
		$display("move is %b, rg_move_engine is %b, rg_cur_pri is %b", move, rg_move_engine, rg_cur_pri);
                if (spillTo0_0) begin
                    spillTo0_0 <= False;
         	    spillTo0_1 <= False;
		    //rg_move_count <= 0;
                end
		else if (rg_move_engine && rg_cur_pri) begin
		    //if (rg_move_count == 2) begin
		    rg_move_engine <= False;
                    rg_offset <= rg_offset + 1;
                    spillTo0_1 <= True;
         	    newBufIdx <= writeFSM_curBufIdx;
		    //rg_move_count <= 0;
         	    $display("%0d: FPGA[0] set offset to %0d", cur_cycle, rg_offset+1);
	            //end
		    //else begin
		    //    rg_move_count <= rg_move_count + 1;
		    //    $display("%0d: FPGA[0] move is 1, rg_move_count is %0d", cur_cycle, rg_move_count);
		    //end
		end
		else if (move) begin
		    rg_move_engine <= True;
		end
            endaction
            else action
                if (spillTo0_0) begin
                    spillTo0_0 <= False;
         	   spillTo0_1 <= True;
         	   newBufIdx <= writeFSM_curBufIdx;
                end
                else if (spillTo0_1) begin
                    spillTo0_1 <= False;
                end
            endaction
            
            // Write head and tailPtrs
            action
                //if(`DEBUG) $display("%0d: mkWLEngine[%0d]: readFSM writing new head/tail ptrs, headPtr=%0d, tailPtr=%0d", cur_cycle, fpgaId, headPtr, tailPtr);
                rg_offset_w <= rg_offset;
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: 1};
                //memReqQ[4].enq(tagged MemWrite64{addr: headPtrLoc, gaddr: gaddr, data: extend(headPtr)});
                rwMemReqQ[4].enq(tagged MemWrite64{addr: offsetLoc, gaddr: gaddr, data: rg_offset});
                for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
                    rwMemReqQ[8+i].enq(tagged MemWrite64{addr: tailPtrLoc[i], gaddr: gaddr, data: extend(tailPtr[i])});
                end
            endaction
            
            // When head/tailPtr writes complete, unlock
            action
                //memRespQ[4].deq();
                rwMemRespQ[4].deq();
                for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
                    rwMemRespQ[8+i].deq();
                end
                rwMemReqQ[12].enq(tagged MemWrite32{addr: lockLoc, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: 1}, data: 0});
                $display("%0d: mkWLEngine[%0d]: WriteFSM done, unlocking! offset is %d", cur_cycle, fpgaId, rg_offset_w);
            endaction
            
            action
                if(writeFSM_totalWrites > 0) 
                    $display("%0d: mkWLEngine[%0d]: WriteFSM wrote %0d entries", cur_cycle, fpgaId, writeFSM_totalWrites);
                rwMemRespQ[12].deq();
                writeFSM_curBufIdx <= writeFSM_curBufIdx + 1;
            endaction
        endseq
    );
    
    // Read FSM: Fills one buffer
    Reg#(BC_Addr) readFSM_numEntries <- mkRegU;
    Reg#(BC_Addr) readFSM_curEntry <- mkRegU;
    Reg#(Bit#(11)) readFSM_backOff <- mkRegU;
    Reg#(Bit#(`LG_NUM_ENGINES)) readFSM_bufIdx <- mkRegU;
    Reg#(Bit#(1)) readFSM_buf <- mkRegU;
    Reg#(Bool) readFSM_success <- mkRegU;
    FIFOF#(Bit#(0)) readFSM_outstandingReads <- mkSizedFIFOF(128);
    Reg#(BC_Addr) headPtr_r <- mkRegU;
    Reg#(BC_Addr) tailPtr_r <- mkRegU;
    Reg#(BC_Addr) bufferLoc_r <- mkRegU;
    Reg#(BC_Addr) headPtrLoc_r <- mkRegU;
    Reg#(Bit#(3)) rg_pri <- mkRegU;
    
    rule readFSM_processReads(readFSM_outstandingReads.notEmpty);
        MemResp rsp = rwMemRespQ[8].first();
        rwMemRespQ[8].deq();
        readFSM_outstandingReads.deq();
        
        WLEntry entry = unpack(rsp.data);
        if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM entry priority: %0x, graphId: %0x", cur_cycle, fpgaId, tpl_1(entry), tpl_2(entry));
        doubleBufIn[readFSM_bufIdx][readFSM_buf].enq(entry);
    endrule
    
    let readFSM <- mkFSM(
       seq
           action
               readFSM_success <= False;
               $display("%0d: mkWLEngine[%0d]: Starting readFSM, filling buf %0d idx %0d...", cur_cycle, fpgaId, readFSM_buf, readFSM_bufIdx);
               lockFSM.start();
           endaction
           
           action
               lockFSM.waitTillDone();
           endaction

           action
               if (headPtr[0] != tailPtr[0]) begin
                   rg_pri <= 0;
                   headPtr_r <= headPtr[0];
                   tailPtr_r <= tailPtr[0];
                   bufferLoc_r <= bufferLoc[0];
                   headPtrLoc_r <= headPtrLoc[0];
		   rg_move_read <= 0;
               end
               else if (headPtr[1] != tailPtr[1]) begin
                   rg_pri <= 1;
                   headPtr_r <= headPtr[1];
                   tailPtr_r <= tailPtr[1];
                   bufferLoc_r <= bufferLoc[1];
                   headPtrLoc_r <= headPtrLoc[1];
		   rg_move_read <= 0;
               end
               else if (headPtr[2] != tailPtr[2]) begin
                   rg_pri <= 2;
                   headPtr_r <= headPtr[2];
                   tailPtr_r <= tailPtr[2];
                   bufferLoc_r <= bufferLoc[2];
                   headPtrLoc_r <= headPtrLoc[2];
		   rg_move_read <= 0;
               end
               else if (headPtr[3] != tailPtr[3]) begin
                   rg_pri <= 3;
                   headPtr_r <= headPtr[3];
                   tailPtr_r <= tailPtr[3];
                   bufferLoc_r <= bufferLoc[3];
                   headPtrLoc_r <= headPtrLoc[3];
		   rg_move_read <= 0;
               end
               else if (headPtr[4] != tailPtr[4]) begin
                   rg_pri <= 4;
                   headPtr_r <= headPtr[4];
                   tailPtr_r <= tailPtr[4];
                   bufferLoc_r <= bufferLoc[4];
                   headPtrLoc_r <= headPtrLoc[4];
		   rg_move_read <= 0;
               end
               else if (headPtr[5] != tailPtr[5]) begin
                   rg_pri <= 5;
                   headPtr_r <= headPtr[5];
                   tailPtr_r <= tailPtr[5];
                   bufferLoc_r <= bufferLoc[5];
                   headPtrLoc_r <= headPtrLoc[5];
		   rg_move_read <= 0;
               end
               else if (headPtr[6] != tailPtr[6]) begin
                   rg_pri <= 6;
                   headPtr_r <= headPtr[6];
                   tailPtr_r <= tailPtr[6];
                   bufferLoc_r <= bufferLoc[6];
                   headPtrLoc_r <= headPtrLoc[6];
		   rg_move_read <= 0;
               end
               else if (headPtr[7] != tailPtr[7]) begin
                   rg_pri <= 7;
                   headPtr_r <= headPtr[7];
                   tailPtr_r <= tailPtr[7];
                   bufferLoc_r <= bufferLoc[7];
                   headPtrLoc_r <= headPtrLoc[7];
		   rg_move_read <= rg_move_read + 1;
               end
	       else begin
	           rg_pri <= 0;
		   headPtr_r <= 0;
		   tailPtr_r <= 0;
		   if (rg_move_read != 0) rg_move_read <= rg_move_read + 1;
	       end
           endaction

           action
               // Get number of entries in FIFO
               BC_Addr size = 0;
               if(headPtr_r < tailPtr_r) begin
                   size = tailPtr_r - headPtr_r;
               end
               else if(headPtr_r > tailPtr_r) begin
                   size = (maxSize - tailPtr_r) + headPtr_r;
               end
               
               // Determine number of entries to read
               BC_Addr entries = ?;
               if(size > `WLENGINE_BUFIN_SIZE) begin
                   entries = `WLENGINE_BUFIN_SIZE;
               end
               else begin
                   entries = size;
               end
               $display("%0d: mkWLEngine[%0d]: ReadFSM headPtr: %0d, tailPtr: %0d, WL size: %0d, reading %0d entries, pri: %0d, headPtrLoc: %x, bufferLoc: %x", cur_cycle, fpgaId, headPtr_r, tailPtr_r, size, entries, rg_pri, headPtrLoc_r, bufferLoc_r);
               readFSM_numEntries <= entries;
               readFSM_curEntry <= 0;

               if (rg_preoffset_r != rg_offset) begin
	           for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
		       pri_buffer[i][0] <= 0;
		       pri_buffer[i][1] <= 0;
		   end
		   rg_preoffset_r <= rg_offset;
	       end
	       else if (entries > 0) begin
	           pri_buffer[readFSM_bufIdx][readFSM_buf] <= rg_pri;
	       end
           endaction
           
           // Read entries and send to buffer
           if(readFSM_numEntries > 0) seq
               while(readFSM_curEntry < readFSM_numEntries) seq
                   action
                       if (`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM Reading entry %0d of %0d, writing to buf%0d[%0d]...", cur_cycle, fpgaId, readFSM_curEntry, readFSM_numEntries, readFSM_bufIdx, readFSM_buf);
                       BC_Addr addr = bufferLoc_r + (headPtr_r << `LG_WLENTRY_SIZE);
                       rwMemReqQ[8].enq(tagged MemRead64{addr: addr, gaddr: GaloisAddress{mod: MK_WORKLIST, addr: 1}});
                       readFSM_outstandingReads.enq(?);
                       
                       readFSM_curEntry <= readFSM_curEntry + 1;
                       
                       if(headPtr_r == (maxSize-1)) begin
                         headPtr_r <= 0;
                       end
                       else begin
                         headPtr_r <= headPtr_r + 1;
                       end
                   endaction
               endseq
               
               while(readFSM_outstandingReads.notEmpty) seq
                   action
                       //$display("%0d: ReadFSM: waiting for %0d reads to complete", cur_cycle, readFSM_numReads.getVal());
                       noAction;
                   endaction
               endseq
               
               // Write head and tailPtrs
               action
                   //if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM writing new head/tail ptrs, headPtr=%0d, tailPtr=%0d", cur_cycle, fpgaId, headPtr, tailPtr);
                   GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: 1};
                   rwMemReqQ[8].enq(tagged MemWrite64{addr: headPtrLoc_r, gaddr: gaddr, data: extend(headPtr_r)});
                   headPtr[rg_pri] <= headPtr_r;
                   //memReqQ[3].enq(tagged MemWrite64{addr: tailPtrLoc, gaddr: gaddr, data: extend(tailPtr)});
               endaction
               
               // When head/tailPtr writes complete, unlock
               action
                   rwMemRespQ[8].deq();
                   //memRespQ[3].deq();
                   rwMemReqQ[8].enq(tagged MemWrite32{addr: lockLoc, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: 1}, data: 0});
               endaction
               
               action
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM done, unlocking!", cur_cycle, fpgaId);
                   $display("%0d: mkWLEngine[%0d]: ReadFSM read %0d entries for idx %0d[%0d]", cur_cycle, fpgaId, readFSM_numEntries, readFSM_bufIdx, readFSM_buf);
                   rwMemRespQ[8].deq();
                   readFSM_success <= True;
               endaction

               //readFSM_backOff <= 0;
               //while(readFSM_backOff < `WLENGINE_BACKOFF) seq
               //    action
               //        readFSM_backOff <= readFSM_backOff + 1;
               //    endaction
               //endseq

           endseq
           else seq
               // No data, unlock and stall avoid needless contention
               action
                   rwMemReqQ[8].enq(tagged MemWrite32{addr: lockLoc, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: 1}, data: 0});
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM done, unlocking!", cur_cycle, fpgaId);
               endaction
               
               action
                   rwMemRespQ[8].deq();
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
        Vector#(`NUM_ENGINES, Bool) bufInEmpties0 = genWith(bufInEmptyF0);
        Vector#(`NUM_ENGINES, Bool) bufInEmpties1 = genWith(bufInEmptyF1);
        let elem0 = findElem(True, bufInEmpties0);
        let elem1 = findElem(True, bufInEmpties1);
        if(isValid(elem0)) begin
            Bit#(`LG_NUM_ENGINES) idx = pack(fromMaybe(?, elem0));
            //$display("BufInEmpties: %b, Empty idx: %0d", bufInEmpties0, idx);
            readFSM_bufIdx <= idx;
            readFSM_buf <= 0;
            readFSM.start();
        end
        else if(isValid(elem1)) begin
            Bit#(`LG_NUM_ENGINES) idx = pack(fromMaybe(?, elem1));
            //$display("BufInEmpties: %b, Empty idx: %0d", idx);
            readFSM_bufIdx <= idx;
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
        if(any(isTrueF, bufOutFulls)) begin
            $display("WLEngine triggerWriteFSM EngineQs[%0d] full: %0b", writeFSM_curIdx, bufOutFulls);
            writeFSM.start();
            triggerWriteFSM_timeout <= 0;
	    rg_cur_pri <= cur_pri;
        end
        else begin
            //$display("WLEngine triggerWriteFSM curBuf: %0d full: %b", writeFSM_curBufIdx, bufOutFulls);
            if(triggerWriteFSM_timeout > `WRITEFSM_TIMEOUT) begin //&& any(isTrueF, bufOutEmpties)) begin
                triggerWriteFSM_timeout <= 0;
                triggerWriteFSM_lastIdx <= triggerWriteFSM_lastIdx + 1;
                writeFSM_curBufIdx <= triggerWriteFSM_lastIdx;
                writeFSM.start();
		rg_cur_pri <= cur_pri;
                $display("Triggering WriteFSM...");
            end
            else begin
	        //$display("WriteFSM waiting");
                writeFSM_curBufIdx <= writeFSM_curBufIdx + 1;
                triggerWriteFSM_timeout <= triggerWriteFSM_timeout + 1;
		spillTo0_0 <= False;
		rg_offset_w <= rg_offset;
            end
        end
    endrule

    rule setSpillTo0_1(started && writeFSM.done);
        function Bool bufOutEmptyF(Integer x) = doubleBufOut[x][writeFSM_curBufIdx].notEmpty;
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

        rule streamToBuf0(started);
            WLEntry entry = reqQ[i].first();
            reqQ[i].deq();

	    //if (tpl_1(entry) == 7) begin
	    //    rg_move[i] <= rg_move[i] + 1;
	    //end
	    //else begin
	    //    rg_move[i] <= 0;
	    //end
            
            Bit#(1) curBuf = curBufOut[i];
            if (doubleBufOut[i][curBuf].notFull) begin
                //$display("mkWLEngine reqQ->doubleBuf[%0d][%0d]", i, curBuf);
                doubleBufOut[i][curBuf].enq(entry);
            end
            else begin
                //$display("mkWLEngine reqQ->doubleBuf[%0d][%0d]", i, curBuf+1);
                doubleBufOut[i][curBuf+1].enq(entry);
                curBufOut[i] <= curBuf + 1;
            end
        endrule        
    end

    //rule calcMove(started);
    //    let cycle <- cur_cycle;
    //    function Bool isTrueF(Bool x) = x;
    //    function Bool readyToMove(Integer x) = (rg_move[x] == 2);

    //    Vector#(`NUM_ENGINES, Bool) move_all = genWith(readyToMove);

    //    rg_move_read <= any(isTrueF, move_all);
    //    if (cycle % 1024 == 0) $display("rg_move is %d%d%d%d", rg_move[0], rg_move[1], rg_move[2], rg_move[3]);
    //endrule
        
    
    rule calcDone(started);
        // No readFSM.done since it's constantly triggered whenever any input buffers are empty!
        let cycle <- cur_cycle;
        Bool isDone = True;
        for(Integer i = 0; i < `NUM_ENGINES; i=i+1) begin
            if(reqQ[i].notEmpty || respQ[i].notEmpty || doubleBufIn[i][0].notEmpty || doubleBufIn[i][1].notEmpty || doubleBufOut[i][0].notEmpty || doubleBufOut[i][1].notEmpty) begin
                isDone = False;
            end
            //if(cycle == 10000000) $display("%0d: Lane %0d notEmpties: reqQ:%b respQ:%b bufIn0:%b bufIn1:%b, doubBufOut0:%b, doubBufOut1:%b, memReq:%b, memResp:%b", cur_cycle, i, reqQ[i].notEmpty, respQ[i].notEmpty, bufIn0[i].notEmpty, bufIn1[i].notEmpty, doubleBufOut[i][0].notEmpty, doubleBufOut[i][1].notEmpty,  memReqQ[i].notEmpty, memRespQ[i].notEmpty);
        end
     
        Bool isDone2 = True;
        for (Integer i = 0; i < `NUM_PRIORITIES; i = i + 1) begin
            if (headPtr[i] != tailPtr[i]) begin
                isDone2 = False;
            end
        end
        
        done <= isDone2 && isDone && (!readFSM_outstandingReads.notEmpty);
	//$display("WLEngine[%0d]: ptr done is %0d, empty done is %0d", fpgaId, (headPtr == tailPtr), isDone);
        //if(cycle == 10000000) $display("%0d: wlEngine done calculation: HeadPtr:%0d, tailPtr:%0d, writeDone:%0b, readDone:%0b, isDone:%0b", cur_cycle, headPtr, tailPtr, writeFSM.done, readFSM.done, isDone);
        //if (cycle == 10000) begin
        //    for (Integer i = 0; i < 8; i = i + 1) begin
        //        $display("bufferLoc[%0d] is %0x", i, bufferLoc[i]);
        //    end
        //end
    endrule
    
    //function GetS#(item_t) fifofToGetS( FIFOF#(item_t) f) provisos ( );
    //    return
    //    (
    //        interface GetS
    //            method item_t first();
    //    	    return f.first();
    //    	endmethod
    //    
    //            method Action deq();
    //    	    f.deq();
    //    	endmethod
    //        endinterface
    //    );
    //endfunction

    method isDone();
        return done;
    endmethod

    method getOffset();
        return rg_offset_w;
    endmethod
    
    method Action init(BC_AEId fpgaid, BC_Addr lockloc, BC_Addr headptrloc, BC_Addr tailptrloc, BC_Addr maxsize, BC_Addr bufferloc, BC_Addr offsetloc);
        //$display("%0d: mkWLEngine[%0d]: INIT, fpgaid: %0x, lockLoc:%0x, headPtrLoc: %0x, tailPtrLoc: %0x, maxSize: %0d, bufferLoc: %0x", cur_cycle, fpgaid, fpgaid, lockloc, headptrloc, tailptrloc, maxsize, bufferloc);
        fpgaId <= fpgaid;
        lockLoc <= lockloc;
	offsetLoc <= offsetloc;
	rg_offset <= 0;
	rg_offset_w <= 0;
        rg_preoffset_r <= 0;
        rg_preoffset_w <= 0;
	rg_move_read <= 0;
	rg_move_engine <= False;
        //headPtrLoc <= headptrloc;
        //tailPtrLoc <= tailptrloc;
	//headPtr <= 0;
	//tailPtr <= 1;
        maxSize <= maxsize >> `LG_NUM_PRIORITIES;
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
            headPtr[i] <= 0;
            if (i == 0) tailPtr[i] <= 1;
            else tailPtr[i] <= 0;
        end
        started <= True;
        
        
        writeFSM_curIdx <= 0;
        writeFSM_curBufIdx <= 0;
        triggerWriteFSM_timeout <= 0;
        
        for(Integer i = 0; i < `NUM_ENGINES; i=i+1) begin
            curBufIn[i] <= 0;
            curBufOut[i] <= 0;
	    //rg_move[i] <= 0;
        end
    endmethod

    method Action stop;
        started <= False;
    endmethod

    interface streamIn = map(toPut, reqQ);
    interface streamOut = map(toGet, respQ);
    interface memReq = map(toGet, memReqQ);
    interface memResp = map(toPut, memRespQ);
    interface priority_ifc = pri_respQ;
    interface cur_pri_ifc = cur_pri;
endmodule


endpackage
