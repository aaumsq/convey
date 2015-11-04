
INTELPCM := IntelPerformanceCounterMonitor-V2.9

sssp:
	g++ -O3 sssp.cpp UnorderedWorklist.cpp LIFO.cpp RandomWorklist.cpp OrderedWorklist.cpp LocalOrderedWorklist.cpp LocalUnorderedWorklist.cpp OBIM.cpp OBIM_HW.cpp OBIM_HW2.cpp Graph.cpp

sssp_optimal:
	g++ -O3 -g sssp_optimal.cpp UnorderedWorklist.cpp LIFO.cpp RandomWorklist.cpp OrderedWorklist.cpp LocalOrderedWorklist.cpp LocalUnorderedWorklist.cpp OBIM.cpp OBIM_HW.cpp OBIM_HW2.cpp Graph.cpp

sssp_dijkstra:
	g++ -O3 -g sssp_dijkstra.cpp Graph.cpp -I $(INTELPCM) $(INTELPCM)/client_bw.o $(INTELPCM)/cpucounters.o $(INTELPCM)/msr.o $(INTELPCM)/pci.o -lpthread -lrt -std=c++0x
	#sudo /opt/perfmon/enable-msr.sh
	#sudo /opt/opencpi/bin/fpga-bless.sh a.out

synthetic:
	g++ -O3 -g synthetic.cpp Graph.cpp -I $(INTELPCM) $(INTELPCM)/client_bw.o $(INTELPCM)/cpucounters.o $(INTELPCM)/msr.o $(INTELPCM)/pci.o -lpthread -lrt -std=c++0x
	#sudo /opt/perfmon/enable-msr.sh
	#sudo /opt/opencpi/bin/fpga-bless.sh a.out

randStream:
	g++ -O3 -g randStream.cpp Graph.cpp -I $(INTELPCM) $(INTELPCM)/client_bw.o $(INTELPCM)/cpucounters.o $(INTELPCM)/msr.o $(INTELPCM)/pci.o -lpthread -lrt -std=c++0x

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
