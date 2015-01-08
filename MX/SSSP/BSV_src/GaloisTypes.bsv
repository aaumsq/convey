package GaloisTypes;

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
 
typedef union tagged {
   struct {
      NodeID id;
      Channel channel;
   } ReadNode;
   
   struct {
      EdgePtr edgePtr;
      Channel channel;
   } ReadEdge;
   
   struct {
      NodeID id;
      Bit#(32) compareVal;
      Bit#(32) swapVal;
      Channel channel;
   } CAS;
   
} GraphReq deriving(Bits, Eq);

typedef struct {
   GraphNode node;
   Channel channel;
} GraphResp deriving(Bits, Eq);

typedef Bit#(32) WLPriority;
typedef NodeID WLJob;
typedef Tuple2#(WLPriority, WLJob) WLEntry;
typedef Tuple2#(Bool, WLEntry) WLSpillReq; // True = Read, False = Write
typedef Tuple2#(Bool, WLEntry) WLSpillResp; // True = Read, False = Write

typedef enum {
   MK_SSSP = 0,
   MK_ENGINE = 1,
   MK_WORKLIST = 2
} GaloisModule deriving(Bits, Eq);

typedef struct {
   GaloisModule mod;
   Bit#(30) addr;
} GaloisAddress deriving(Bits, Eq);

endpackage