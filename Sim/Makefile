
sssp:
	g++ sssp.cpp UnorderedWorklist.cpp LIFO.cpp OrderedWorklist.cpp LocalOrderedWorklist.cpp OBIM.cpp Graph.cpp

sssp_optimal:
	g++ -g sssp_optimal.cpp UnorderedWorklist.cpp LIFO.cpp OrderedWorklist.cpp LocalOrderedWorklist.cpp OBIM.cpp Graph.cpp

components:
	g++ connectedcomponents.cpp UnorderedWorklist.cpp LIFO.cpp OrderedWorklist.cpp LocalOrderedWorklist.cpp OBIM.cpp Graph.cpp

bfs:
	g++ bfs.cpp UnorderedWorklist.cpp LIFO.cpp OrderedWorklist.cpp LocalOrderedWorklist.cpp OBIM.cpp Graph.cpp

dij:
	g++ CRS.cpp sssp_dij.cpp

# ----------------------------------------------------------------
.PHONY: clean
clean:
	rm -f  *~  *.o $(BUILD_DIR)/*.o
