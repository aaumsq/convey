package GaloisTypes;

import BC_HW_IFC::*;

typedef Bit#(32) NodeID;  // Probably needs to be increased
typedef Bit#(32) NodePayload;
typedef Bit#(32) NodeNumEdges;
typedef Bit#(32) EdgeWeight;
typedef Bit#(32) EdgePtr; // Probably needs to be increased

typedef Bit#(8) Channel;

typedef struct {
   NodeID id;
   NodePayload payload;
   NodeNumEdges numEdges;
   EdgePtr edgePtr;
} GraphNode deriving (Bits, Eq);

typedef struct {
   NodeID dest;
   EdgeWeight weight;
} GraphEdge deriving (Bits, Eq);

typedef struct {
   NodeID id;
} GraphNodeReq deriving(Bits, Eq);

typedef struct {
   EdgePtr id;
} GraphEdgeReq deriving(Bits, Eq);

typedef struct {
   NodeID id;
   NodePayload cmpVal;
   NodePayload swapVal;
} GraphCASReq deriving(Bits, Eq);

typedef union tagged {
   struct {
      NodeID id;
      Channel channel;
   } ReadNode;
   
   struct {
      EdgePtr edgeID;
      Channel channel;
   } ReadEdge;
   
   struct {
      NodeID id;
      NodePayload cmpVal;
      NodePayload swapVal;
      Channel channel;
   } CAS;
   
} GraphReq deriving(Bits, Eq);

typedef struct {
   GraphNode node;
} GraphNodeResp deriving(Bits, Eq);

typedef struct {
   GraphEdge gedge;
} GraphEdgeResp deriving(Bits, Eq);

typedef struct {
   Bool success;
   NodePayload oldVal;
} GraphCASResp deriving(Bits, Eq);

typedef union tagged {
   struct {
      GraphNode node;
      Channel channel;
   } Node;
   
   struct {
      GraphEdge gedge;
      Channel channel;
   } Edge;

   struct {
      Bool success;
      NodePayload oldVal;
      Channel channel;
   } CAS;
} GraphResp deriving(Bits, Eq);

typedef Bit#(32) WLPriority;
typedef NodeID WLJob;
typedef Tuple2#(WLPriority, WLJob) WLEntry;
typedef Tuple2#(Bool, WLEntry) WLSpillReq; // True = Read, False = Write
typedef Tuple2#(Bool, WLEntry) WLSpillResp; // True = Read, False = Write

typedef enum {
   MK_SSSP = 0,
   MK_ENGINE = 1,
   MK_WORKLIST = 2,
   MK_GRAPH = 3,
   IGNORE = 4
} GaloisModule deriving(Bits, Eq);

typedef struct {
   GaloisModule mod;
   Bit#(5) addr;
} GaloisAddress deriving(Bits, Eq);

typedef union tagged {
   struct {
      BC_Addr addr;
      GaloisAddress gaddr;
   } MemRead64;

   struct {
      BC_Addr addr;
      GaloisAddress gaddr;
   } MemRead32;
   
   struct {
      BC_Addr addr;
      GaloisAddress gaddr;
      Bit#(64) data;
   } MemWrite64;

   struct {
      BC_Addr addr;
      GaloisAddress gaddr;
      Bit#(32) data;
   } MemWrite32;
   
   struct {
      BC_Addr addr;
      GaloisAddress gaddr;
      Bit#(32) cmpVal;
      Bit#(32) swapVal;
   } MemCAS32;
} MemReq deriving(Bits, Eq);

typedef struct {
   GaloisAddress gaddr;
   Bit#(64) data;
} MemResp deriving(Bits, Eq);

endpackage