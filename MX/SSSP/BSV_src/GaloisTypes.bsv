

`ifndef GALOIS_TYPES
`define GALOIS_TYPES

`define WL_ENGINE_PORTS 16
`define WL_SPILL_PORTS 16
`define WL_WLFIFO_SIZE 1024


typedef Bit#(32) WLPriority;
typedef Bit#(32) WLJob;
typedef Tuple2#(WLPriority, WLJob) WLEntry;
typedef Tuple2#(Bool, WLEntry) WLSpillReq; // True = Read, False = Write
typedef Tuple2#(Bool, WLEntry) WLSpillResp; // True = Read, False = Write

typedef enum {
   MK_SSSP = 0,
   MK_ENGINE = 1,
   MK_WORKLIST = 2
} GaloisModule deriving(Eq, Bits);

typedef struct {
   GaloisModule mod;
   Bit#(30) addr;
} GaloisAddress deriving(Eq, Bits);

`endif