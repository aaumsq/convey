// Copyright (c) 2012-2015 Bluespec, Inc.  All Rights Reserved.
// Distributed under license.

// Author: Rishiyur S. Nikhil

// This package has utilities that are not BClib-specific and  should
// eventually be merged into the standard BSV libraries

package BC_Utils;

// ================================================================
// Library imports

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// ================================================================
// A convenience function to return the current cycle number during Bluesim simulations

ActionValue #(Bit #(32)) cur_cycle = actionvalue
					Bit #(32) t <- $stime;
					return t / 10;
				     endactionvalue;

// ================================================================
// Dummy Gets, Puts, Clients and Servers
// (Can be used, for example, for unused interfaces)

Get #(t) getstub = interface Get;
		      method ActionValue #(t) get () if (False);
			 return ?;
		      endmethod
		   endinterface;

Put #(t) putstub = interface Put;
		      method Action put (t x) if (False);
			 noAction;
		      endmethod
		   endinterface;

Client #(rq_t, rs_t) clientstub = interface Client;
				     interface Get request  = getstub;
				     interface Put response = putstub;
				  endinterface;

Server #(rq_t, rs_t) serverstub = interface Server;
				     interface Put request  = putstub;
				     interface Get response = getstub;
				  endinterface;

// ================================================================
// Conversion of fifos to Clients and Servers

function Client #(req_t, rsp_t) fifofs_to_client (FIFOF #(req_t) f_req, FIFOF #(rsp_t) f_rsp);
   return interface Client;
	     interface request  = toGet (f_req);
	     interface response = toPut (f_rsp);
	  endinterface;
endfunction

function Server #(req_t, rsp_t) fifofs_to_server (FIFOF #(req_t) f_req, FIFOF #(rsp_t) f_rsp);
   return interface Server;
	     interface request  = toPut (f_req);
	     interface response = toGet (f_rsp);
	  endinterface;
endfunction

// ================================================================
// This is a specialization of mkReg (tagged Invalid).
// It creates a Reg containing a Maybe #(t) initialized to Invalid.
// Only the valid bit is reset to 0 (i.e., Invalid).
// Note that mkReg (tagged Invalid) resets all bits, which can
//     lead to an unnecessarily large fan-out for the RESET signal

module mkInvalidReg (Reg #(Maybe #(t)))
   provisos (Bits #(t, tsize));

   Reg #(Bit #(1))     rg_valid <- mkReg (0);  // reset to 0 = Invalid
   Reg #(Bit #(tsize)) rg_data  <- mkRegU;     // no reset

   return interface Reg;
	     method Action _write (Maybe #(t) mx);
		let bits = pack (mx);
		rg_valid <= msb (bits);
		rg_data  <= truncate (bits);
	     endmethod
	     method Maybe #(t) _read;
		let bits = { rg_valid, rg_data };
	        return unpack (bits);
	     endmethod
	  endinterface;
endmodule

// ================================================================

endpackage
