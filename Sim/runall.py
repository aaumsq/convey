#!/usr/bin/python

import os, re;

apps = [["sssp", "0"], 
        ["sssp_optimal", "0"],
        ["components", ""],
        ["bfs", "0"]
        ];

indir = "inputs/";

outfile = open("rollup.csv", "w+");
outfile.write("Application, Input, Iterations, Total Work, Max Cores Utilized, Utilization Percentage, Conflict Percentage\n");

benchmarks = ["USA-road-d.NY.edgelist",
              "USA-road-d.FLA.edgelist",
              "USA-road-d.W.edgelist",
              "r4-2e18.edgelist",
              "r4-2e23.edgelist"
              ];


rollup = re.compile('Iters: (\d+), totalWork: (\d+), max cores active: (\d+), utilization: ([\d\.]+), conflict percent: ([\d\.]+)');

for app in apps:
    for benchmark in benchmarks:
        cmd = "make "+app[0]+" && time ./a.out "+indir+benchmark+" "+app[1]+" &> tmp";
        print(cmd);
        os.system(cmd);
        os.system("mv stats.csv runs/stats_"+app[0]+"_"+benchmark+".csv");
        os.system("mv tmp runs/out_"+app[0]+"_"+benchmark);
        
        file = open("./runs/out_"+app[0]+"_"+benchmark);
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
                outfile.write(app[0]+", "+benchmark+", "+iters+", "+totalWork+", "+maxCores+", "+coreUtilization+", "+conflicts+"\n");
                outfile.flush()
