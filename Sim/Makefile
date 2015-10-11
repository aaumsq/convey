
INTELPCM := IntelPerformanceCounterMonitor-V2.9

sssp:
	g++ sssp.cpp UnorderedWorklist.cpp LIFO.cpp RandomWorklist.cpp OrderedWorklist.cpp LocalOrderedWorklist.cpp LocalUnorderedWorklist.cpp OBIM.cpp OBIM_HW.cpp OBIM_HW2.cpp Graph.cpp

sssp_optimal:
	g++ -g sssp_optimal.cpp UnorderedWorklist.cpp LIFO.cpp RandomWorklist.cpp OrderedWorklist.cpp LocalOrderedWorklist.cpp LocalUnorderedWorklist.cpp OBIM.cpp OBIM_HW.cpp OBIM_HW2.cpp Graph.cpp

sssp_dijkstra:
	g++ -g sssp_dijkstra.cpp Graph.cpp -I $(INTELPCM) $(INTELPCM)/client_bw.o $(INTELPCM)/cpucounters.o $(INTELPCM)/msr.o $(INTELPCM)/pci.o -lpthread -lrt -std=c++0x
	sudo /opt/opencpi/bin/fpga-bless.sh a.out

components:
	g++ connectedcomponents.cpp UnorderedWorklist.cpp LIFO.cpp RandomWorklist.cpp OrderedWorklist.cpp LocalOrderedWorklist.cpp LocalUnorderedWorklist.cpp OBIM.cpp OBIM_HW.cpp OBIM_HW2.cpp Graph.cpp

bfs:
	g++ bfs.cpp UnorderedWorklist.cpp LIFO.cpp OrderedWorklist.cpp RandomWorklist.cpp LocalOrderedWorklist.cpp LocalUnorderedWorklist.cpp OBIM.cpp OBIM_HW.cpp OBIM_HW2.cpp Graph.cpp

dij:
	g++ CRS.cpp sssp_dij.cpp

# ----------------------------------------------------------------
.PHONY: clean
clean:
	rm -f  *~  *.o $(BUILD_DIR)/*.o
