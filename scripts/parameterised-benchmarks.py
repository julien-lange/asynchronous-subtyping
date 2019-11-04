#!/usr/bin/python3
import sys
import subprocess
import os
import os.path
import string
import time
# import numpy as np
import csv


# Number of iterations
maxiterations = 4

logfile = "log-file-benchmarks.txt"
tmpfile = "tmp-cfsm.txt"

# OUTPUT FILE
prefname = "parametrised-benchmarks"

# TIMEOUT (in seconds)
# cmdtimeout = 360
# cmdtimeout = 1500 # 25 min
cmdtimeout = 1500 #



def cleanup(): 
    subprocess.call(["killall","KMC"]
                    , stdout=subprocess.PIPE
                    , stderr=subprocess.PIPE)


def runOverRange(minx, maxx):
    name = prefname+"-X-"+str(minx)+"-"+str(maxx)+".csv"
    with open(name,"w") as out:    
        write = csv.writer(out) 
        with open(name+logfile, "wb") as log_file:
            for x in range(minx,maxx):
                        # GENERATE EXAMPLE
                        gencmd = subprocess.Popen(["./GenAsyncTypes",str(x),"1"], stdout=subprocess.PIPE)
                        print("Test: ",str(x))
                        timings = []
                        nstates = ""
                        ntrans = ""
                        for it in range(1,maxiterations):
                            print("Running Checker: ",str(x))
                            startt = time.time() # time in seconds
                            kmccmd = subprocess.Popen(["../tool/Checker","m1.txt","m2.txt"], stdout=subprocess.PIPE)
                            try:
                                kmccmd.wait(timeout=cmdtimeout)
                                endt = time.time()
                                txt = "Measured execution time: "+str(endt-startt)
                                print(txt)
                                for line in kmccmd.stdout:
                                    sp = line.decode("utf-8").split("*")
                                    if len(sp) > 4:
                                        nstates = sp[1]
                                        ntrans = sp[3]
                                    log_file.write(line)
                                log_file.write((txt+"\n").encode())
                                timings.append(endt-startt)
                                # write.writerow([x,y,p,nstates,ntrans,endt-startt])
                            except subprocess.TimeoutExpired:
                                kmccmd.kill()
                                kmccmd.wait()
                                print("Checker timedout")
                                return
                        avg = sum(timings)/float(len(timings))
                        write.writerow([x,nstates,ntrans,avg]+timings)
                            



# Measure growing k with 10 peers
runOverRange(1, 20)

