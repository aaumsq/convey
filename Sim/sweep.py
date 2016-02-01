#!/usr/bin/python

import os, sys, re;

apps = [["sssp", "0"], 
        #["sssp_optimal", "0"]
        #["components", ""]
    #["bfs", "0"]
        ];

indir = "inputs/";

outfile = open("rollup.csv", "a+");
outfile.write("Application, Input, Cores, Bucket Size, Iterations, Total Work, Max Cores Utilized, Utilization Percentage, Executed Percentage, Conflict Percentage\n");

benchmarks = [#"USA-road-d.NY.edgelist",
              #"USA-road-d.FLA.edgelist",
              #"USA-road-d.W.edgelist",
              "USA-road-d.USA.edgelist",
              #"r4-2e18.edgelist",
              #"r4-2e23.edgelist"
              ];

cores = [#"1",
         #"2",
         "4",
         "8",
         "16",
         "32",
         "64",
         "128"
         ];

#buckets = ["1024"];
#buckets = ["65536"];
buckets = ["1"];

rollup = re.compile('Iters: (\d+), totalWork: (\d+), max cores active: (\d+), utilization: ([\d\.]+), executed: ([\d\.]+), conflict percent: ([\d\.]+)');



for app in apps:
    for benchmark in benchmarks:
        for core in cores:
            for bucket in buckets:
                cmd = "make "+app[0]+" && ./a.out "+core+" "+bucket+" "+indir+benchmark+" "+app[1]+" > tmp";
                print(cmd);
                assert 0 == os.system(cmd);
                assert 0 == os.system("mv stats.csv runs/stats_"+app[0]+"_"+benchmark+"_"+core+"_"+bucket+".csv");
                assert 0 == os.system("mv tmp runs/out_"+app[0]+"_"+benchmark+"_"+core+"_"+bucket);
                
                file = open("./runs/out_"+app[0]+"_"+benchmark+"_"+core+"_"+bucket);
                for line in file:
                    m = rollup.search(line);
                    if(m):
                        print line;
                        iters = m.group(1);
                        totalWork = m.group(2);
                        maxCores = m.group(3);
                        coreUtilization = m.group(4);
                        conflicts = m.group(5);
                        print m.group(1)+", "+m.group(2)+", "+m.group(3)+", "+m.group(4)+", "+m.group(5);
                        outfile.write(app[0]+", "+benchmark+", "+core+", "+bucket+", "+iters+", "+totalWork+", "+maxCores+", "+coreUtilization+", "+conflicts+", "+line);
                        outfile.flush()
