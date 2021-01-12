#!/share/apps/bio3user/miniconda3/bin/python
# delete this after done

import os
import sys
import argparse
import deeptools
import deeptools.getScorePerBigWigBin as getScorePerBigWigBin
import deeptools.heatmapper as heatmapper
import deeptools.heatmapper_utilities as heatmapper_utilities
import matplotlib.pyplot as plt

# for colorful printing in bash
LRD='\033[1;31m'
LGN='\033[1;32m'
LCY='\033[1;36m'
YW='\033[1;33m'
LPR='\033[1;35m'
N='\033[0m'

# parser argument
print("") # add paragraph so text wont be too clutter
parser = argparse.ArgumentParser()
parser.add_argument("-i", "--bed",       metavar="File.bed",      type=str, nargs="?", help="Bed File.",required=True)
parser.add_argument("-q", "--que",       metavar="que.bw",        type=str, nargs="+", help="Query bigWig File(s) - separate by space.",required=True)
parser.add_argument("-o", "--out",       metavar="string",        type=str, nargs="?", help="output name")
parser.add_argument("-p", "--pos",       metavar="pos.bw",        type=str, nargs="+", help="Positive control bigWig File(s) i.e. best Rloop samples - separate by space.")
parser.add_argument("-n", "--neg",       metavar="neg.bw",        type=str, nargs="+", help="Negative control bigWig File(s) e.g. RNaseH/Input/IgG - separate by space.")
parser.add_argument("-t", "--type",      metavar="scale-regions", type=str, nargs="?", help="'scale-regions' or 'reference-point' from computeMatrix.",default="scale-regions")
parser.add_argument("-b", "--before",    metavar="5000",          type=int, nargs="?", help="bp [B]efore ref. point. -b from computeMatrix.",default=5000)
parser.add_argument("-a", "--after",     metavar="5000",          type=int, nargs="?", help="bp [A]fter ref. point. -a from computeMatrix.",default=5000)
parser.add_argument("-z", "--skipZeros", metavar="TRUE",          type=str, nargs="?", help="--skipZeros from computeMatrix.",default="--skipZeros")
print("")

args = parser.parse_args()

# make output dirs
os.makedirs("matrix", exist_ok=True)
os.makedirs("sortedRegions", exist_ok=True)
os.makedirs("svg", exist_ok=True)
os.makedirs("png", exist_ok=True)

# define input and input names, as well as output names if not defined with -o
bedFile, bedFileName = args.bed, os.path.basename(args.bed)
queFile, queFileName = args.que, os.path.basename(args.que[0])
outName = f"""{bedFileName},{queFileName}"""

# check if there's positive control files
# also add pos and neg control filenames to default outName
if args.pos != None:
	posFileName = os.path.basename(args.pos[0])
	posFile = args.pos
	outName = f"""{outName},{posFileName}"""
else:
	posFileName = ""
	posFile = ["None"]

# check if there's negative controlf iles
if args.neg != None:
	negFileName = os.path.basename(args.neg[0])
	negFile = args.neg
	outName = f"""{outName},{negFileName}"""
else:
	negFileName = ""
	negFile = ["None"]

# outName might be too long, check this later

# if -o is defined then output name use -o instead
if args.out != None:
	outName=args.out

# print parameters
argsprint = f"""
{YW}{'-'*30}{N}
         {YW}PARAMETERS{N}
{YW}{'-'*30}{N}

-i [bedFile]   = {LCY}{args.bed}{N}
-q [queFile]   = {LCY}{",".join(args.que)}{N}
-o [outName]   = {LCY}{outName}{N}
-p [posFile]   = {LCY}{",".join(args.pos)}{N}
-n [negFile]   = {LCY}{",".join(args.neg)}{N}
-t [type]      = {LPR}{args.type}{N}
-z [skipZeros] = {LPR}{args.skipZeros}{N}
-b [before]    = {LGN}{args.before}{N}
-a [after]     = {LGN}{args.after}{N}

{YW}{'-'*30}{N}
"""

print(argsprint)


# print output files
outMatrixFile=f"""matrix/{outName}.matrix.gz"""
outSortedFile=f"""sortedRegions/{outName}.sorted.bed""" # not yet
outsvgFile=f"""svg/{outName}.svg"""
outpngFile=f"""png/{outName}.png"""

outsprint = f"""
{YW}{'-'*30}{N}
           {YW}OUTPUTS{N}
{YW}{'-'*30}{N}

outMatrixFile = {LCY}{outMatrixFile}{N}
outSortedFile = {LCY}{outSortedFile}{N}
outsvgFile = {LCY}{outsvgFile}{N}
outpngFile = {LCY}{outpngFile}{N}
{YW}{'-'*30}{N}
"""

print(outsprint)

# bless matrix object
dm = heatmapper.heatmapper()

# define parameters for computeMatrix (have to define EVERYTHNG or it dies...)
# scale-regions most likely won't do anything, i just put in there to remind myself

parameters = {
"scale-regions":1,
"bin size":10,
"downstream":500,
"upstream":500,
"body":0,
"unscaled 3 prime":0,
"unscaled 5 prime":0,
"scale":1,
"skip zeros":1,
"proc number":1,
"verbose":1,
"nan after end":None,
"missing data as zero":1,
"ref point":"TSS",
"max threshold":None,
"min threshold":None,
"bin avg type":"mean",
"group_boundaries":2,
"sample_boundaries":2,
"sample_labels":queFile,
"group_labels":["group1","group2"],
"sort regions":1,
"sort using":None
}

# computeMatrix run
heatmapper.heatmapper.computeMatrix(dm,score_file_list=queFile,regions_file=bedFile,parameters=parameters)

# save matrix
heatmapper.heatmapper.save_matrix(dm,file_name=f"""matrix/{outName}.matrix.gz""")

# plotly plot. Still doesn't work and debugging. Died due to tuple out of bound.
myplot = heatmapper_utilities.plotly_single(dm,average_type="mean",color="red",label="label")#["blue","red"],label=["label1","label2"],plot_type=["line","line"])

# debug prints
print("myplot is",myplot)

print("dm is",dm)

# non-plotly plot. Copy pasted from docs. Also doesn't work & died due to tuple out of bound.
#fig = plt.figure()
#ax = fig.add_subplot(111)
#ax = heatmapper_utilities.plot_single(ax, dm, 'mean', color='blue', label='red')
#leg = ax.legend()
#plt.savefig(f"""png/{outName}.png""")
#plt.close()
