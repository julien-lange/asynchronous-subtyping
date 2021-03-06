#!/usr/bin/python3
import sys
import subprocess
import os
import os.path
import string
import time
# import numpy as np
import csv


# Number of iterations (> 1)
maxiterations = 6

logfile = "log-file-benchmarks.txt"
tmpfile = "tmp-cfsm.txt"

# OUTPUT FILE
prefname = "parametrised-benchmarks"

# TIMEOUT (in seconds)
# cmdtimeout = 360
# cmdtimeout = 1500 # 25 min
cmdtimeout = 500 #



def cleanup(): 
    subprocess.call(["killall","KMC"]
                    , stdout=subprocess.PIPE
                    , stderr=subprocess.PIPE)


def runOverRange(sid,minx, maxx, gencmd):
    name = prefname+"_"+sid+"_"+str(minx)+"-"+str(maxx)+".csv"
    with open(name,"w") as out:    
        write = csv.writer(out) 
        with open(name+logfile, "wb") as log_file:
            for x in range(minx,maxx):
                        print("Test: ",str(x))
                        gencmd(x)
                        timings = []
                        nstates = []
                        ntrans = ""
                        for it in range(1,maxiterations):
                            print("Running Checker: ",str(x))
                            startt = time.time() # time in seconds
                            kmccmd = subprocess.Popen(["/usr/bin/time","-v","../tool/Checker","m1.txt","m2.txt"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                            try:
                                kmccmd.wait(timeout=cmdtimeout)
                                endt = time.time()
                                txt = "Measured execution time: "+str(endt-startt)
                                print(txt)
                                for line in kmccmd.stderr:
                                    sp = line.decode("utf-8").split(":")
                                    if len(sp) > 1:
                                        if (sp[0].find('Maximum resident set size') > 0):
                                            nstates.append(int(sp[1].strip()))
                                    log_file.write(line)
                                log_file.write((txt+"\n").encode())
                                timings.append(endt-startt)
                                # write.writerow([x,y,p,nstates,ntrans,endt-startt])
                            except subprocess.TimeoutExpired:
                                kmccmd.kill()
                                kmccmd.wait()
                                print("/!\ Checker timedout, terminating these benchmarks")
                                return
                        avg = sum(timings)/float(len(timings))
                        avgmem = sum(nstates)/float(len(nstates))
                        write.writerow([x,avgmem,ntrans,avg]+timings)
                            





def moreAs(x):
    cmd = subprocess.Popen(["./GenAsyncTypes",str(x),"1"], stdout=subprocess.PIPE)
    return cmd

def moreBs(x):
    cmd = subprocess.Popen(["./GenAsyncTypes","1", str(x)], stdout=subprocess.PIPE)
    return cmd 


runOverRange("A",1, 20, moreAs)
runOverRange("B",1, 20, moreBs)

