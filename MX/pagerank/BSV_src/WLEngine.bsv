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

    method Action init(BC_AEId fpgaId, BC_Addr lockLoc, BC_Addr headPtrLoc, BC_Addr tailPtrLoc, BC_Addr tailPtrLoc_w, BC_Addr commitHeadPtrLoc, BC_Addr commitTailPtrLoc, BC_Addr maxSize, BC_Addr bufferLoc, BC_Addr tailPtr);
    method Action stop();
    method Bool isDone();
endinterface


`define WLENGINE_BUFOUT_SIZE 1024
`define WLENGINE_BUFIN_SIZE 1024
`define WLENTRY_SIZE 8
`define LG_WLENTRY_SIZE 3
`define WLENGINE_MAX_WRITES 16384
`define WLENGINE_BACKOFF 128
`define WRITEFSM_TIMEOUT 128
`define WRITEFSM_TIMEOUT0 2048

(* synthesize *)
module mkWLEngine(WLEngine);
    // memOffset[0]: lock bit
    // memOffset[1]: headPtr
    // memOffset[2]: tailPtr
    // memOffset[3]: workList size
    // memOffset[4]: start of data
    
    Reg#(BC_AEId) fpgaId <- mkReg(0);
    Reg#(BC_Addr) lockLoc <- mkRegU;
    Reg#(BC_Addr) lockLoc_r <- mkRegU;
    Reg#(BC_Addr) headPtr <- mkRegU;
    Reg#(BC_Addr) headPtr_w <- mkRegU;
    Reg#(BC_Addr) headPtrLoc <- mkRegU;
    
    Reg#(BC_Addr) tailPtr <- mkRegU;
    Reg#(BC_Addr) tailPtr_w <- mkRegU;
    Reg#(BC_Addr) tailPtrLoc <- mkRegU;
    Reg#(BC_Addr) tailPtrLoc_w <- mkRegU;
    Reg#(BC_Addr) commitHeadPtrLoc <- mkRegU;
    Reg#(BC_Addr) commitTailPtrLoc <- mkRegU;
    
    Reg#(BC_Addr) maxSize <- mkRegU;
    Reg#(BC_Addr) maxSize_mask <- mkRegU;
    Reg#(BC_Addr) bufferLoc <- mkRegU;
    
    Reg#(Bool) started <- mkReg(False);
    Reg#(Bool) done <- mkReg(False);
    Reg#(Bool) turn <- mkReg(False);
    
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) reqQ <- replicateM(mkFIFOF);
    Vector#(`NUM_ENGINES, FIFOF#(WLEntry)) respQ <- replicateM(mkFIFOF);
    Vector#(`NUM_ENGINES, Vector#(2, Reg#(Bit#(16)))) bufOutLen <- replicateM(replicateM(mkRegU));
    
    Vector#(16, FIFOF#(MemReq)) memReqQ <- replicateM(mkFIFOF);
    Vector#(16, FIFOF#(MemResp)) memRespQ <- replicateM(mkFIFOF);
    
    Vector#(`NUM_ENGINES, Vector#(2, FIFOF#(WLEntry))) doubleBufIn <- replicateM(replicateM(mkSizedBufBRAMFIFOF(`WLENGINE_BUFIN_SIZE)));
    Vector#(`NUM_ENGINES, Reg#(Bit#(1))) curBufIn <- replicateM(mkRegU);

    Vector#(`NUM_ENGINES, Vector#(2, FIFOF#(WLEntry))) doubleBufOut <- replicateM(replicateM(mkSizedBufBRAMFIFOF(`WLENGINE_BUFOUT_SIZE)));
    Reg#(Bit#(1)) curBufOut <- mkRegU;

    
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
    Reg#(Bit#(16)) lockFSM_backOff_w <- mkRegU;
    let lockFSM_w <- mkFSM(
       seq
           action
               lock_lockData_w <= 32'b1;
           endaction
           
           // Obtain global worklist lock
           while(lock_lockData_w == 32'd1) seq
               action
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: Reading lock bit at addr: %0x...", cur_cycle, fpgaId, lockLoc);
                   memReqQ[3].enq(tagged MemCAS32{addr: lockLoc, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: ?}, cmpVal: 0, swapVal: 1});
               endaction
           
               action
                   MemResp rsp = memRespQ[3].first();
                   memRespQ[3].deq();
                   lock_lockData_w <= truncate(rsp.data);
                   //$display("%0d: mkWLEngine[%0d]: old lock bit = %0d", cur_cycle, fpgaId, rsp.data);
                   // Data is the old data, so if 1 then it failed
                   if(truncate(rsp.data) == 32'd1) begin
                       //$display("%0d: mkWLEngine[%0d]:  Worklist is locked, retry...", cur_cycle, fpgaId);
                   end
               endaction
            endseq
       endseq
       );
    
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
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: Reading lock bit at addr: %0x...", cur_cycle, fpgaId, lockLoc_r);
                   memReqQ[11].enq(tagged MemCAS32{addr: lockLoc_r, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: ?}, cmpVal: 0, swapVal: 1});
               endaction
           
               action
                   MemResp rsp = memRespQ[11].first();
                   memRespQ[11].deq();
                   lock_lockData <= truncate(rsp.data);
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: old lock bit = %0d", cur_cycle, fpgaId, rsp.data);
                   // Data is the old data, so if 1 then it failed
                   if(truncate(rsp.data) == 32'd1) begin
                       //if(`DEBUG) $display("%0d: mkWLEngine[%0d]:  Worklist is locked, retry...", cur_cycle, fpgaId);
                   end
               endaction
/*
               if(lock_lockData == 32'd1) seq
                   if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM nothing to read, stalling for %0d cycles", cur_cycle, fpgaId, `WLENGINE_BACKOFF);
                   lockFSM_backOff <= 0;
                   while(lockFSM_backOff < `WLENGINE_BACKOFF) seq
                       action
                         lockFSM_backOff <= lockFSM_backOff + 1;
                       endaction
                   endseq
               endseq
*/
          endseq
           
       endseq
       );

    // Write FSM: Writes all full buffers
    Reg#(Bit#(`LG_NUM_ENGINES)) writeFSM_curIdx <- mkRegU;
    Reg#(Bit#(1)) writeFSM_curBufIdx <- mkRegU;
    Reg#(Bit#(16)) writeFSM_totalWrites <- mkRegU;
    Reg#(Bit#(16)) writeFSM_numEntries <- mkRegU;
    Vector#(`NUM_ENGINES_HALF, Reg#(Bit#(16))) writeFSM_numEntries_partial <- replicateM(mkRegU);
    Reg#(Bool) writeFSM_done <- mkRegU;
    //Reg#(Bool) fullWrite <- mkRegU;
    Reg#(BC_Addr) writeFSM_wlSize <- mkRegU;
    Reg#(BC_Addr) writeFSM_maxSizeMinusOne <- mkRegU;
    Reg#(BC_Addr) rg_commitHead <- mkRegU;
    Reg#(BC_Addr) rg_commitTail <- mkRegU;
    Vector#(`NUM_ENGINES, FIFOF#(Bit#(0))) writeFSM_outstandingWrites <- replicateM(mkSizedFIFOF(256));
    Vector#(`NUM_ENGINES, Reg#(BC_Addr)) writeFSM_tails <- replicateM(mkRegU);
    
    for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
        rule writeFSM_catchWriteAcks(writeFSM_outstandingWrites[i].notEmpty);
	    Integer ii;
	    if (i < 2) ii = i;
	    else ii = i + 2;
            memRespQ[ii].deq();
            writeFSM_outstandingWrites[i].deq();
        endrule
    end
    
    //rule monitorWrite(started);
    //    if (!(memReqQ[0].notFull && memReqQ[1].notFull && memReqQ[2].notFull && memReqQ[3].notFull))
    //        $display("%0d: mkWLEngine[%0d]: memReqNotFull: %b%b%b%b", cur_cycle, fpgaId, memReqQ[0].notFull, memReqQ[1].notFull, memReqQ[2].notFull, memReqQ[3].notFull);
    //endrule
    
    function Bool writeOutstandingEmpty(Integer x) = writeFSM_outstandingWrites[x].notEmpty;
    function Bool isTrue(Bool x) = x;
    Vector#(`NUM_ENGINES, Bool) writeOutstandingEmpties = genWith(writeOutstandingEmpty);
    let writeFSM <- mkFSM(
        seq
            action
                if(`DEBUG) $display("%0d: mkWLEngine: WriteFSM START", cur_cycle);
                lockFSM_w.start();
                writeFSM_curIdx <= 0;
                writeFSM_totalWrites <= 0;
                writeFSM_done <= False;
            endaction
            
            action
                if(`DEBUG) $display("%0d: mkWLEngine: WriteFSM lock + head/tailPtr obtained! headPtr=%0d, tailPtr=%0d", cur_cycle, headPtr, tailPtr_w);
                lockFSM_w.waitTillDone();
		turn <= True;
            endaction

            // Get updated head, tail pointers and the tail pointer of the commitQueue
            action
                //$display("%0d: mkWLEngine[%0d]: getting updated head/tail ptrs", cur_cycle, fpgaId);
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: ?};
		memReqQ[3].enq(tagged MemRead64{addr: commitTailPtrLoc, gaddr: gaddr});
                memReqQ[4].enq(tagged MemRead64{addr: headPtrLoc, gaddr: gaddr});
                memReqQ[5].enq(tagged MemRead64{addr: tailPtrLoc_w, gaddr: gaddr});
	        curBufOut <= writeFSM_curBufIdx + 1;
		//Bit#(16) numEntries = 0;
		//Calculate the size of the space to be allocate by adding up the length of the buffers
		//for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
		//    numEntries = numEntries + bufOutLen[i][writeFSM_curBufIdx];
		    //$display("%0d: mkWLEngine[%0d]: bufOutLen[%0d][%0d]: %0d", cur_cycle, fpgaId, i, writeFSM_curBufIdx, bufOutLen[i][writeFSM_curBufIdx]);
		//end
		for (Integer i = 0; i < `NUM_ENGINES_HALF; i = i + 1) begin
		    writeFSM_numEntries_partial[i] <= bufOutLen[2*i][writeFSM_curBufIdx] + bufOutLen[2*i+1][writeFSM_curBufIdx];
		end
	    endaction

	    action
		//Clear the buffer length
		Bit#(16) numEntries = 0;
		for (Integer i = 0; i < `NUM_ENGINES_HALF; i = i + 1) begin
		    writeFSM_tails[2*i] <= extend(numEntries);
		    writeFSM_tails[2*i+1] <= extend(numEntries + bufOutLen[2*i][writeFSM_curBufIdx]);
		    numEntries = numEntries + writeFSM_numEntries_partial[i];
		end
                writeFSM_numEntries<= numEntries;
		
		for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
		    bufOutLen[i][writeFSM_curBufIdx] <= 0;
		end
		turn <= False;
            endaction

	    //Enqueue the commitQueue
            action
	        MemResp commitTailRsp = memRespQ[3].first();
                MemResp headRsp = memRespQ[4].first();
                MemResp tailRsp = memRespQ[5].first();
		memRespQ[3].deq();
                memRespQ[4].deq();
                memRespQ[5].deq();
                $display("%0d: mkWLEngine[%0d]: headPtr: %0d, tailPtr: %0d, commitTail: %0d", cur_cycle, fpgaId, headRsp.data, tailRsp.data, commitTailRsp.data);
                headPtr_w <= truncate(pack(headRsp.data));
                tailPtr_w <= truncate(pack(tailRsp.data));
		rg_commitTail <= truncate(commitTailRsp.data + 1);
            endaction

	    action
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: ?};
	        BC_Addr newTailPtr = (tailPtr_w + extend(writeFSM_numEntries)) & maxSize_mask;
                memReqQ[3].enq(tagged MemWrite64{addr: tailPtrLoc_w, gaddr: gaddr, data: extend(newTailPtr)});
	        memReqQ[4].enq(tagged MemWrite64{addr: commitTailPtrLoc, gaddr: gaddr, data: extend(rg_commitTail)});
		for (Integer i = 0; i < `NUM_ENGINES; i = i+1) begin
		    writeFSM_tails[i] <= writeFSM_tails[i] + tailPtr_w;
		end
		tailPtr_w <= newTailPtr;
	    endaction

            //Release the lock
	    action
		//$display("%0d: mkWLEngine[%0d]: unlock write worklist", cur_cycle, fpgaId);
		$display("%0d: mkWLEngine[%0d]: newTailPtr: %0d, tail0: %0d, tail1: %0d, tail2: %0d, tail3: %0d", cur_cycle, fpgaId, tailPtr_w, writeFSM_tails[0], writeFSM_tails[1], writeFSM_tails[2], writeFSM_tails[3]);
	        memRespQ[3].deq();
                memReqQ[5].enq(tagged MemWrite64{addr: lockLoc, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: ?}, data: 0});
	        memRespQ[4].deq();
	    endaction

	    action
	        memRespQ[5].deq();
            endaction

            while(!writeFSM_done) seq
                action
                    //if(`DEBUG) $display("%0d: mkWLEngine WriteFSM checking doubleBufOut[%0d][%0d], writeFSM_done: %0d, notEmpty: %0d", cur_cycle, writeFSM_curIdx, writeFSM_curBufIdx, writeFSM_done, doubleBufOut[writeFSM_curIdx][writeFSM_curBufIdx].notEmpty);
         	    for (Integer i = 0; i < `NUM_ENGINES; i = i+1) begin
		        Integer ii;
		        if (i < 2) ii = i;
			else ii = i + 2;
         	        if (doubleBufOut[i][writeFSM_curBufIdx].notEmpty) begin
                            WLEntry entry = doubleBufOut[i][writeFSM_curBufIdx].first();
                            doubleBufOut[i][writeFSM_curBufIdx].deq();
                            BC_Addr addr = bufferLoc + ((writeFSM_tails[i] & maxSize_mask) << `LG_WLENTRY_SIZE);
                            memReqQ[ii].enq(tagged MemWrite64{addr: addr, gaddr: GaloisAddress{mod: MK_WORKLIST, addr: ?}, data: pack(entry)});
                            writeFSM_outstandingWrites[i].enq(?);
                            $display("%0d: mkWLEngine[%0d][%0d]: WriteFSM tailPtr=%0d, packet: %0d", cur_cycle, fpgaId, i, writeFSM_tails[i], tpl_2(entry));
                            writeFSM_tails[i] <= writeFSM_tails[i] + 1;
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
            
            //while(writeFSM_outstandingWrites[0].notEmpty || writeFSM_outstandingWrites[1].notEmpty || writeFSM_outstandingWrites[2].notEmpty || writeFSM_outstandingWrites[3].notEmpty) seq
	    while(any(isTrue, writeOutstandingEmpties)) seq
                action
                    //$display("%0d: WriteFSM: waiting for %0d writes to complete", cur_cycle, writeFSM_numWrites.getVal());
                    noAction;
                endaction
            endseq
            
	    action
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: ?};
	        memReqQ[3].enq(tagged MemRead64{addr: commitHeadPtrLoc, gaddr: gaddr});
	    endaction

	    action
	        MemResp commitHeadRsp = memRespQ[3].first();
	        memRespQ[3].deq();
	        rg_commitHead <= truncate(commitHeadRsp.data);
	    endaction

            //Check whether it's on top of the commitQueue
	    while (rg_commitHead != rg_commitTail) seq
	        action
	            //$display("%0d: mkWLEngine[%0d]: commit waiting, commitHead: %0d, tail: %0d", cur_cycle, fpgaId, rg_commitHead, rg_commitTail);
                    GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: ?};
	            memReqQ[3].enq(tagged MemRead64{addr: commitHeadPtrLoc, gaddr: gaddr});
	        endaction

	        action
	            MemResp commitHeadRsp = memRespQ[3].first();
	    	    memRespQ[3].deq();
	    	    rg_commitHead <= truncate(commitHeadRsp.data);
	        endaction
	    endseq

            //Dequeue it from the commitQueue and commit the real tail pointer
            action
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: ?};
                //if(writeFSM_totalWrites > 0) 
                    //$display("%0d: mkWLEngine[%0d]: WriteFSM wrote %0d entries", cur_cycle, fpgaId, writeFSM_totalWrites);
	        memReqQ[3].enq(tagged MemWrite64{addr: tailPtrLoc, gaddr: gaddr, data: extend(tailPtr_w)});
	        memReqQ[4].enq(tagged MemWrite64{addr: commitHeadPtrLoc, gaddr: gaddr, data: extend(rg_commitHead+1)});
                if(`DEBUG) $display("%0d: mkWLEngine[%0d]: WriteFSM done, unlocking!", cur_cycle, fpgaId);
            endaction

	    action
	        memRespQ[3].deq();
	        memRespQ[4].deq();
	    endaction
        endseq
       );
    
    // Read FSM: Fills one buffer
    Reg#(BC_Addr) readFSM_numEntries <- mkRegU;
    Vector#(`NUM_ENGINES, Reg#(BC_Addr)) readFSM_curEntry <- replicateM(mkRegU);
    Reg#(Bit#(10)) readFSM_backOff <- mkRegU;
    Reg#(Bit#(`LG_NUM_ENGINES)) readFSM_bufIdx <- mkRegU;
    Reg#(Bit#(1)) readFSM_buf <- mkRegU;
    Reg#(Bool) readFSM_success <- mkRegU;
    Vector#(`NUM_ENGINES, FIFOF#(Bit#(0))) readFSM_outstandingReads <- replicateM(mkSizedFIFOF(256));
    Vector#(`NUM_ENGINES, Reg#(BC_Addr)) headPtr_r <- replicateM(mkRegU);
    Reg#(BC_Addr) tailPtr_r <- mkRegU;
    Reg#(Bit#(4)) rg_i <- mkRegU;
    
    for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
        rule readFSM_processReads(readFSM_outstandingReads[i].notEmpty);
	    Integer ii;
	    if (i < 2) ii = i + 8;
	    else ii = i + 10;
            MemResp rsp = memRespQ[ii].first();
            memRespQ[ii].deq();
            readFSM_outstandingReads[i].deq();
            
            WLEntry entry = unpack(rsp.data);
            //$display("%0d: mkWLEngine[%0d][%0d]: ReadFSM entry priority: %0d, graphId: %0d", cur_cycle, fpgaId, i, tpl_1(entry), tpl_2(entry));
            doubleBufIn[i][readFSM_buf].enq(entry);
        endrule
    end

    function Bool readOutstandingEmpty(Integer x) = readFSM_outstandingReads[x].notEmpty;
    Vector#(`NUM_ENGINES, Bool) readOutstandingEmpties = genWith(readOutstandingEmpty);
    let readFSM <- mkFSM(
        seq
            action
                readFSM_success <= False;
                lockFSM.start();
                if(`DEBUG) $display("%0d: mkWLEngine[%0d]: Starting readFSM, filling buf %0d idx %0d...", cur_cycle, fpgaId, readFSM_buf, readFSM_bufIdx);
            endaction
            
            action
                lockFSM.waitTillDone();
            endaction

            // Get updated head and tail pointers
            action
                //$display("%0d: mkWLEngine[%0d]: getting updated head/tail ptrs", cur_cycle, fpgaId);
                GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: ?};
                memReqQ[10].enq(tagged MemRead64{addr: headPtrLoc, gaddr: gaddr});
                memReqQ[14].enq(tagged MemRead64{addr: tailPtrLoc, gaddr: gaddr});
            endaction
            
            action
                MemResp headRsp = memRespQ[10].first();
                MemResp tailRsp = memRespQ[14].first();
                memRespQ[10].deq();
                memRespQ[14].deq();
                if(`DEBUG) $display("%0d: mkWLEngine[%0d]: headPtr: %0x, tailPtr: %0x", cur_cycle, fpgaId, headRsp.data, tailRsp.data);
                headPtr <= truncate(pack(headRsp.data));
                tailPtr <= truncate(pack(tailRsp.data));
            endaction

            action
                // Get number of entries in FIFO
                BC_Addr size = 0;
                if(headPtr < tailPtr) begin
                    size = tailPtr - headPtr;
                end
                else if(headPtr > tailPtr) begin
                    size = (maxSize - tailPtr) + headPtr;
                end
                
                // Determine number of entries to read
                BC_Addr entries = ?;
                if(size > (`NUM_ENGINES*`WLENGINE_BUFIN_SIZE)) begin
                    entries = (`NUM_ENGINES*`WLENGINE_BUFIN_SIZE);
                end
                else begin
                    entries = size;
                end
                //$display("%0d: mkWLEngine[%0d]: ReadFSM headPtr: %0d, tailPtr: %0d, WL size: %0d, reading %0d entries", cur_cycle, fpgaId, headPtr, tailPtr, size, entries);
                readFSM_numEntries <= entries;
                for (Integer i = 0; i < `NUM_ENGINES; i = i + 1) begin
                    readFSM_curEntry[i] <= fromInteger(i);
                end
            endaction
            
            // Read entries and send to buffer
            if(readFSM_numEntries > 0) seq
                // Write head and tailPtrs
                action
                    GaloisAddress gaddr = GaloisAddress{mod: MK_WORKLIST, addr: ?};
	    	    BC_Addr newHeadPtr = headPtr + readFSM_numEntries;
                    memReqQ[10].enq(tagged MemWrite64{addr: headPtrLoc, gaddr: gaddr, data: extend(newHeadPtr)});
                    //$display("%0d: mkWLEngine[%0d]: ReadFSM writing new head/tail ptrs, headPtr=%0d, tailPtr=%0d", cur_cycle, fpgaId, newHeadPtr, tailPtr);
                    //memReqQ[3].enq(tagged MemWrite64{addr: tailPtrLoc, gaddr: gaddr, data: extend(tailPtr)});
                endaction

                // When head/tailPtr writes complete, unlock
                action
                    memRespQ[10].deq();
                    memReqQ[9].enq(tagged MemWrite64{addr: lockLoc_r, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: ?}, data: 0});
		    for (Integer i = 0; i < `NUM_ENGINES; i = i+1) begin
		        headPtr_r[i] <= headPtr + fromInteger(i);
		    end
                    //memRespQ[3].deq();
                endaction

                action
                    if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM done, unlocking!", cur_cycle, fpgaId);
                    if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM read %0d entries for idx %0d[%0d]", cur_cycle, fpgaId, readFSM_numEntries, readFSM_bufIdx, readFSM_buf);
                    memRespQ[9].deq();
                    //readFSM_success <= True;
                endaction

                while(readFSM_curEntry[`NUM_ENGINES-1] < readFSM_numEntries) seq
                    action
                        //if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM Reading entry %0d of %0d, writing to buf%0d[%0d]...", cur_cycle, fpgaId, readFSM_curEntry, readFSM_numEntries, readFSM_bufIdx, readFSM_buf);
            	        for (Integer i = 0; i < `NUM_ENGINES; i = i+1) begin
	                    Integer ii;
	                    if (i < 2) ii = i + 8;
	                    else ii = i + 10;
			    BC_Addr head = headPtr_r[i] & maxSize_mask;
                            BC_Addr addr = bufferLoc + (head << `LG_WLENTRY_SIZE);
                            memReqQ[ii].enq(tagged MemRead64{addr: addr, gaddr: GaloisAddress{mod: MK_WORKLIST, addr: ?}});
	    		    //$display("%0d: mkWLEngine[%0d][%0d]: readFSM read headPtr: %0d", cur_cycle, fpgaId, i, headPtr + fromInteger(i));
                            readFSM_outstandingReads[i].enq(?);
                            
                            readFSM_curEntry[i] <= readFSM_curEntry[i] + `NUM_ENGINES;
			    headPtr_r[i] <= headPtr_r[i] + `NUM_ENGINES;
            	        end
                    endaction
                endseq

                for (rg_i <= 0; rg_i < `NUM_ENGINES-1; rg_i <= rg_i+1) seq
		    action
		        Bit#(4) ii;
		        if (rg_i < 2) ii = rg_i + 8;
			else ii = rg_i + 10;
                        if (readFSM_curEntry[rg_i] < readFSM_numEntries) begin
		            BC_Addr head = headPtr_r[rg_i] & maxSize_mask;
                            BC_Addr addr = bufferLoc + ((head) << `LG_WLENTRY_SIZE);
                            memReqQ[ii].enq(tagged MemRead64{addr: addr, gaddr: GaloisAddress{mod: MK_WORKLIST, addr: ?}});
	    	            //$display("%0d: mkWLEngine[%0d][%0d]: readFSM read headPtr: %0d", cur_cycle, fpgaId, i, headPtr + fromInteger(i));
                            readFSM_outstandingReads[rg_i].enq(?);
		            headPtr <= headPtr_r[rg_i] & maxSize_mask;
                        end 
		    endaction
		endseq

                //while(readFSM_outstandingReads[0].notEmpty || readFSM_outstandingReads[1].notEmpty || readFSM_outstandingReads[2].notEmpty || readFSM_outstandingReads[3].notEmpty) seq
		while(any(isTrue, readOutstandingEmpties)) seq
                    action
                        //$display("%0d: ReadFSM: waiting for %0d reads to complete", cur_cycle, readFSM_numReads.getVal());
                        noAction;
                    endaction
                endseq
            endseq
            else seq
                // No data, unlock and stall avoid needless contention
                action
                    memReqQ[9].enq(tagged MemWrite64{addr: lockLoc_r, gaddr: GaloisAddress{mod:MK_WORKLIST, addr: ?}, data: 0});
                    if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM done, unlocking!", cur_cycle, fpgaId);
                endaction
                
                action
                    memRespQ[9].deq();
                endaction
                
                if(`DEBUG) $display("%0d: mkWLEngine[%0d]: ReadFSM nothing to read, stalling for %0d cycles", cur_cycle, fpgaId, `WLENGINE_BACKOFF);
                readFSM_backOff <= 0;
                while(readFSM_backOff < `WLENGINE_BACKOFF) seq
                    action
                        readFSM_backOff <= readFSM_backOff + 1;
                    endaction
                endseq
                
                action
                    noAction;
                endaction

            endseq
        endseq
    );

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
	end
        for (Integer i = 0; i < 16; i = i+1) begin
	    if (memRespQ[i].notEmpty)
	        memRespQ[i].deq();
	end
    endrule
    
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
        //if(isValid(elem0)) begin
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
        if(any(isTrueF, bufOutFulls)) begin
            //$display("WLEngine triggerWriteFSM EngineQs[%0d], writeFSMIdx: %0d, full: %0b", fpgaId, writeFSM_curIdx, bufOutFulls);
            writeFSM.start();
            triggerWriteFSM_timeout <= 0;
	    //fullWrite <= True;
        end
        else begin
            //$display("WLEngine triggerWriteFSM curBuf: %0d full: %b", writeFSM_curBufIdx, bufOutFulls);
	    //fullWrite <= False;
            if ((triggerWriteFSM_timeout > `WRITEFSM_TIMEOUT && any(isTrueF, bufOutEmpties)) || triggerWriteFSM_timeout > `WRITEFSM_TIMEOUT0) begin
                triggerWriteFSM_timeout <= 0;
                //triggerWriteFSM_lastIdx <= triggerWriteFSM_lastIdx + 1;
                //writeFSM_curBufIdx <= triggerWriteFSM_lastIdx;
                writeFSM.start();
                //$display("mkWLEngine[%0d]: Triggering WriteFSM...", fpgaId);
            end
            else begin
                writeFSM_curBufIdx <= writeFSM_curBufIdx + 1;
                triggerWriteFSM_timeout <= triggerWriteFSM_timeout + 1;
            end
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
                //$display("%0d: mkWLEngine curBufIn[0] packet %0d streaming to WorkListFIFOF", cur_cycle, tpl_2(pkt));
                doubleBufIn[i][0].deq();
                respQ[i].enq(pkt);
            end
            if((curBufIn[i] == 1) && doubleBufIn[i][1].notEmpty) begin
                WLEntry pkt = doubleBufIn[i][1].first();
                //$display("%0d: mkWLEngine curBufIn[1] packet %0d streaming to WorkListFIFOF", cur_cycle, tpl_2(pkt));
                doubleBufIn[i][1].deq();
                respQ[i].enq(pkt);
            end
        endrule
        
        rule streamToBuf(!turn);
            WLEntry entry = reqQ[i].first();
            if (curBufOut == 0) begin
	        if (doubleBufOut[i][0].notFull) begin
                    reqQ[i].deq();
                    //$display("%0d: mkWLEngine[%0d] reqQ->doubleBuf[%0d][0], id: %0d, bufLen[0]: %0d", cur_cycle, fpgaId, i, tpl_2(entry), bufOutLen[i][0]);
                    doubleBufOut[i][0].enq(entry);
		    bufOutLen[i][0] <= bufOutLen[i][0] + 1;
		end
		//else if (doubleBufOut[i][1].notFull) begin
		//    reqQ[i].deq();
		//    doubleBufOut[i][1].enq(entry);
		//end
            end
	    else begin
	        if (doubleBufOut[i][1].notFull) begin
	            reqQ[i].deq();
                    //$display("%0d: mkWLEngine[%0d] reqQ->doubleBuf[%0d][1], id: %0d, bufLen[1]: %0d", cur_cycle, fpgaId, i, tpl_2(entry), bufOutLen[i][1]);
		    doubleBufOut[i][1].enq(entry);
		    bufOutLen[i][1] <= bufOutLen[i][1] + 1;
	        end
                //else begin
                //    reqQ[i].deq();
                //    //$display("mkWLEngine reqQ->doubleBuf[%0d][%0d]", i, curBufOut+1);
                //    doubleBufOut[i][0].enq(entry);
		//end
            end
        endrule        
    end
    
    rule calcDone(started);
        // No readFSM.done since it's constantly triggered whenever any input buffers are empty!
        let cycle <- cur_cycle;
        Bool isDone = True;
        for(Integer i = 0; i < `NUM_ENGINES; i=i+1) begin
            if(reqQ[i].notEmpty || respQ[i].notEmpty || doubleBufIn[i][0].notEmpty || doubleBufIn[i][1].notEmpty || doubleBufOut[i][0].notEmpty || doubleBufOut[i][1].notEmpty || readFSM_outstandingReads[i].notEmpty || writeFSM_outstandingWrites[i].notEmpty) begin
                isDone = False;
            end
            if(cycle % 16384 == 0) $display("%0d: Lane[%0d][%0d] notEmpties: reqQ:%b respQ:%b doubleBufIn0:%b doubleBufIn1:%b, doubBufOut0:%b, doubBufOut1:%b, memReq:%b, memResp:%b", cur_cycle, fpgaId, i, reqQ[i].notEmpty, respQ[i].notEmpty, doubleBufIn[i][0].notEmpty, doubleBufIn[i][1].notEmpty, doubleBufOut[i][0].notEmpty, doubleBufOut[i][1].notEmpty,  memReqQ[i].notEmpty, memRespQ[i].notEmpty);
        end
        
        done <= (headPtr == tailPtr) && (headPtr_w == tailPtr_w) && isDone;
	//$display("WLEngine[%0d]: ptr done is %0d, empty done is %0d", fpgaId, (headPtr == tailPtr), isDone);
        //if(cycle == 10000000) $display("%0d: wlEngine done calculation: HeadPtr:%0d, tailPtr:%0d, writeDone:%0b, readDone:%0b, isDone:%0b", cur_cycle, headPtr, tailPtr, writeFSM.done, readFSM.done, isDone);
    endrule
    
    method isDone();
        return done;
    endmethod
    
    method Action init(BC_AEId fpgaid, BC_Addr lockloc, BC_Addr headptrloc, BC_Addr tailptrloc, BC_Addr tailptrloc_w, BC_Addr commitheadptrloc, BC_Addr committailptrloc, BC_Addr maxsize, BC_Addr bufferloc, BC_Addr tailptr);
        $display("%0d: mkWLEngine[%0d]: INIT, fpgaid: %0x, lockLoc:%0x, headPtrLoc: %0x, tailPtrLoc: %0x, maxSize: %0d, bufferLoc: %0x", cur_cycle, fpgaid, fpgaid, lockloc, headptrloc, tailptrloc, maxsize, bufferloc);
        fpgaId <= fpgaid;
        lockLoc <= lockloc;
	lockLoc_r <= lockloc + 8;
        headPtrLoc <= headptrloc;
        tailPtrLoc <= tailptrloc;
	tailPtrLoc_w <= tailptrloc_w;
	commitHeadPtrLoc <= commitheadptrloc;
	commitTailPtrLoc <= committailptrloc;
	headPtr <= 0;
	tailPtr <= tailptr;
        maxSize <= maxsize;
	maxSize_mask <= maxsize - 1;
        bufferLoc <= bufferloc;
        started <= True;
	turn <= False;
        
        writeFSM_curIdx <= 0;
        writeFSM_curBufIdx <= 0;
        triggerWriteFSM_timeout <= 0;
        
        for(Integer i = 0; i < `NUM_ENGINES; i=i+1) begin
            curBufIn[i] <= 0;
	    bufOutLen[i][0] <= 0;
	    bufOutLen[i][1] <= 0;
        end
	curBufOut <= 0;
    endmethod

    method Action stop;
        started <= False;
    endmethod
    
    interface streamIn = map(toPut, reqQ);
    interface streamOut = map(toGet, respQ);
    interface memReq = map(toGet, memReqQ);
    interface memResp = map(toPut, memRespQ);
endmodule


endpackage
