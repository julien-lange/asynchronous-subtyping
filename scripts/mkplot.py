#!/usr/bin/python

import matplotlib.pyplot as plt
from pylab import * 
import numpy as np
import csv
import string
import os
from matplotlib.ticker import ScalarFormatter 
from scipy.optimize import curve_fit


filetype = ".eps"
ticksfontsize = 12
axisfontsize = 15
legentfontsize = 15
mymarkersize=5


def fitfunc(x, a, b, c ):
    return a * np.power(b, x) + c

def mkPlot(bfile,outpath):
    tab = np.loadtxt(bfile,
                         usecols=(0,1,3),
                         unpack = True,
                         delimiter = ',',
                         dtype = float
                         )

    stab = tab[0].argsort()
    final = tab[:,stab]
    tr,tm,tk = final
    # maxtr = max(tr)

    
    plt.figure(figsize=(9, 9))    

    fix, ax = plt.subplots()


    linx = np.array(tr)
    liny = np.array(tk)
    linspace = np.linspace(0,max(tr))
    logspace = np.logspace(0,max(tr))
    popt, pcov = curve_fit(fitfunc, linx, liny, bounds=(0, [100, 10, 20]))

    print("Fitted curve: "+str(popt))
    # print(pcov)


    preflegend = "Execution time" # "Time with "
    memlegend = "Memory usage"
    legend = "" # "NA"


    plt.yticks(fontsize=ticksfontsize)
    plt.xticks(rotation=40, ha='right', fontsize=ticksfontsize)
  
    plt.grid( linestyle='dotted', color='gray')

    ax.set_ylabel('Time (seconds)', fontsize=axisfontsize)

    plt.yscale('log')
    
    ax2 = ax.twinx()
    ax2.set_yscale("log")

    
    ax.plot(linspace, fitfunc(linspace, *popt),color='orange',zorder=10)  #, linestyle='None',marker='.') 
    ax.plot(tr,tk,marker='o',markersize=mymarkersize,linestyle='None',color='blue',zorder=20)
    ax2.plot(tr, tm,marker='.',markersize=mymarkersize,linestyle='None',color='red',zorder=15)

    
    ax2.set_ylabel('Memory (kbytes)', fontsize=axisfontsize)

    ax2.legend([memlegend],loc=(0.05,0.68),fontsize=legentfontsize)
    
    
    # ax.legend(
    #     [r'$F(x)\approx%5.5f * %5.4f^x +%5.4f$' % tuple(popt),
    #      preflegend+legend,memlegend], loc=(0.05,0.8),fontsize=legentfontsize)


    
    (ca,cb,cc) = tuple(popt)
    ax.legend(
        [r'$F(x)\approx%5.5f * %5.4f^x$' % (ca,cb),
         preflegend+legend,memlegend], loc=(0.05,0.8),fontsize=legentfontsize)



    

    xlabel = "m"
    if bfile.find("_A_") > 1:
        xlabel = "n"
    ax.set_xlabel( r'Parameter $'+xlabel+'$',fontsize=axisfontsize)
    plt.xscale('linear')


    posfifx = '-lin'+filetype
    plt.ticklabel_format(style='sci', axis='x',useOffset=True)



    plt.savefig('./plots/'+outpath #bfile.replace(".","-")+posfifx
                    , dpi=300
                    , bbox_inches="tight"
                    , frameone=False)
    

    # plt.show()


if not os.path.exists('./plots'):
    os.makedirs('./plots')
    
i = 0
for f in os.listdir("./"):
    if (f.startswith("parametrised-benchmarks_")) and (f.endswith(".csv")):
        ostr = f+"plot-"+string.ascii_lowercase[i]+filetype
        print("Converting "+f+" to "+ostr)
        mkPlot(f,ostr)
        i += 1
