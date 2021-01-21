import os
import os.path
from os import path
import sys
import argparse
import subprocess
import math
import numpy as np

#deeptools
import deeptools.getScorePerBigWigBin as getScorePerBigWigBin
import deeptools.heatmapper as hmp
import deeptools.heatmapper_utilities as hmpu
import matplotlib.pyplot as plt

#dash
import plotly.graph_objs as go
import dash
import dash_core_components as dcc
import dash_html_components as html
import pandas as pd
from flask import request

#my own module
import imlib
RD, GN, YW, BU, PR, CY, LRD, LGN, LYW, LBU, LPR, LCY, N = imlib.imcols() #for bash color
# can't run deeptols in windows, have to be remotely, so have to use bash

def im_plot_single(dm,outName):
	mymatrix = dm.matrix.get_matrix(group=0,sample=0)['matrix']
	fig = plt.figure()
	ax = fig.add_subplot(111)
	ax = hmpu.plot_single(ax=ax,ma=mymatrix,average_type='mean',color='blue', label='red')
	leg = ax.legend()
	plt.savefig(f"""png/{outName}.png""")
	plt.close()

# parser argument

print("") # add paragraph so text wont be too clutter
parser = argparse.ArgumentParser()
parser.add_argument("-A", "--all",       metavar="inputlst.csv",  type=str, nargs="?", help="Table (csv) containing list of bigWig files, type (que/pos/neg/all), and species.")
parser.add_argument("-B", "--bed",       metavar="File.bed",      type=str, nargs="?", help="Bed File.",required=True)
parser.add_argument("-q", "--que",       metavar="que.bw",        type=str, nargs="+", help="Query bigWig File(s) - separate by space.")
parser.add_argument("-o", "--out",       metavar="string",        type=str, nargs="?", help="output name")
parser.add_argument("-p", "--pos",       metavar="pos.bw",        type=str, nargs="+", help="Positive control bigWig File(s) i.e. best Rloop samples - separate by space.")
parser.add_argument("-n", "--neg",       metavar="neg.bw",        type=str, nargs="+", help="Negative control bigWig File(s) e.g. RNaseH/Input/IgG - separate by space.")
parser.add_argument("-t", "--type",      metavar="body",          type=str, nargs="?", help="body (scaled genebody), TSS, or TTS.",default="body")
parser.add_argument("-b", "--before",    metavar="5000",          type=int, nargs="?", help="bp [B]efore ref. point. -b from computeMatrix.",default=5000)
parser.add_argument("-a", "--after",     metavar="5000",          type=int, nargs="?", help="bp [A]fter ref. point. -a from computeMatrix.",default=5000)
parser.add_argument("-z", "--skipZeros", help="--skipZeros from computeMatrix [default: True]",action="store_false")
parser.add_argument("-Q", "--QUIET",     help="make it not print anything [default: False]",action="store_true")
print("")


args = parser.parse_args()

# define printv which will not print if -Q QUIET is true
printv = print if args.QUIET == False else lambda *a, **k: None

# make output dirs
os.makedirs("matrix", exist_ok=True)
os.makedirs("plotly", exist_ok=True)
os.makedirs("svg", exist_ok=True)
os.makedirs("png", exist_ok=True)

# define input and input names, as well as output names if not defined with -o
# Reassign args into readable variables

bedFiles = args.bed
bedNames = os.path.basename(args.bed)

# if --all then read from csv
def check_file_exists(files,group):
	tempfiles = []
	tempgroup = []
	print("group is",group)
	for i in range(0,len(files)):
		if path.exists(files[i]) == False:
			continue

		tempfiles.append(files[i])

		for i in range(0,len(group)):
			tempgroup.append(group[i])
	files = tempfiles
	return files

posFiles = []
negFiles = []
queFiles = []
queFilePrint = ""
posFilePrint = ""
negFilePrint = ""

if args.all != None:

	files = pd.read_csv(args.all)

	allGroup = files["type"]+files["species"]

	allFiles = check_file_exists(("bw/"+files["file"]).values.tolist(),allGroup)
	posFiles = check_file_exists(("bw/"+files.loc[files["type"] == "pos"]["file"]).values.tolist(),[])
	negFiles = check_file_exists(("bw/"+files.loc[files["type"] == "neg"]["file"]).values.tolist(),[])
	queFiles = check_file_exists(("bw/"+files.loc[files["type"] == "que"]["file"]).values.tolist(),[])

	outName = f"""{bedNames},all,b{args.before},a{args.after}""" if args.out == None else args.out

else:
	queFiles = args.que
	posFiles = args.pos
	negFiles = args.neg

	if queFiles != None:
		queNames = os.path.basename(queFiles[0])
		queGroup = ["que"]*len(queFiles)
		queFilePrint = ",".join(args.que)
	if posFiles != None:
		posNames = os.path.basename(posFiles[0])
		posGroup = ["pos"]*len(posFiles)
		posFilePrint = ",".join(args.pos)
	if negFiles != None:
		negNames = os.path.basename(negFiles[0])
		negGroup = ["neg"]*len(negFiles)
		negFilePrint = ",".join(args.neg)

	allFiles = queFiles + posFiles + negFiles
	allGroup = queGroup + posGroup + negGroup

	outName = f"""{bedNames},{queNames}"""
	outName = f"""{outName},{posNames}""" if args.pos != None else outName
	outName = f"""{outName},{negNames}""" if args.neg != None else outName
	outName = f"""{outName},b{args.before},a{args.after}"""

print(LYW,"all Files:\n",LCY,allFiles,N)
print(LYW,"que Files:\n",LCY,queFiles,N)
print(LYW,"pos Files:\n",LCY,posFiles,N)
print(LYW,"neg Files:\n",LCY,negFiles,N)
print(LYW,"out Files:",LCY,outName,N)

# print parameters
argsprint = f"""
{YW}{'-'*30}{N}
         {YW}PARAMETERS{N}
{YW}{'-'*30}{N}

-i [bedFiles]   = {LCY}{args.bed}{N}
-q [queFiles]   = {LCY}{queFilePrint}{N}
-o [outName]    = {LCY}{outName}{N}
-p [posFiles]   = {LCY}{posFilePrint}{N}
-n [negFiles]   = {LCY}{negFilePrint}{N}
-t [type]       = {LPR}{args.type}{N}
-z [skipZeros]  = {LPR}{args.skipZeros}{N}
-b [before]     = {LGN}{args.before}{N}
-a [after]      = {LGN}{args.after}{N}

{YW}{'-'*30}{N}
"""

printv(argsprint)


print(YW,"all Files:",LCY,allFiles,N)
print(YW,"all Group:",LCY,allGroup,N)
print("\n\n")

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

printv(outsprint)

# bless matrix object
dm = hmp.heatmapper()

# define parameters for computeMatrix (have to define EVERYTHNG or it dies...)
param_verbose = 1 if args.QUIET == False else 0
parameters = {
"bin size":200,
"downstream":500,
"upstream":500,
"body":0,
"unscaled 3 prime":0,
"unscaled 5 prime":0,
"scale":1,
"skip zeros":1,
"proc number":1,
"verbose":param_verbose,
"nan after end":None,
"missing data as zero":1,
"ref point":"TSS",
"max threshold":None,
"min threshold":None,
"bin avg type":"mean",
"group_boundaries":2,
"sample_boundaries":2,
"sample_labels":queFiles,
"group_labels":allGroup,
"sort regions":1,
"sort using":None
}

type = args.type

printv(allFiles)
printv(allGroup)



parameters["upstream"]   = args.before
parameters["downstream"] = args.after

outnametype=""

# draw body (body)
parameters["ref point"]  = None if args.type == "body" else args.type
parameters["body"]       = args.before+args.after if args.type == "body" else 0
outNametype              = f"""{outName},{args.type}"""

if args.type != "body" and args.type != "TSS" and args.type != "TTS":
	printv("type is not defined in parameter list (body, TSS, TTS)")
	printv(args.type)



# computeMatrix run
# Todo: make option to not re-run matrix if exist
matrixFile = f"""matrix/{outNametype}.matrix.gz"""
hmp.heatmapper.computeMatrix(dm,score_file_list=allFiles,regions_file=bedFiles,parameters=parameters)

hmp.heatmapper.save_matrix(dm,file_name=matrixFile) # save matrix
#hmp.heatmapper.read_matrix_file(dm,matrixFile) # read matrix

plotlycsvFile = f"""plotly/{outNametype}.csv"""
f = open(plotlycsvFile,"w")

f.write("x,y,y_upper,y_lower,labels\n")

tempmat = {}

# turn heatmapper matrix object into numpy array
# get mean, stdev, stderr
for i in range(0,len(allFiles)):

	if path.exists(allFiles[i]) == False:
		next(i)

	# get matrix
	mymatrix = dm.matrix.get_matrix(group=0,sample=i)['matrix']

	# turn into simple array trace
	# Todo: this is weird though, gonna check if there's better method
	myplot = hmpu.plotly_single(mymatrix,average_type="mean",color="green",label=allFiles[i])

	# append data from each group to numpy readable format
	currGroup = allGroup[i]
	printv("\n","\n","".join(["-"]*10)) # debug print
	printv(LGN,i,N,"currGroup",LCY,currGroup,N,"currFile",LYW,allFiles[i],N)	# debug print

	x = myplot[0]["x"]
	y = myplot[0]["y"]
	y_upper = y
	y_lower = y
	labels = os.path.basename(allFiles[i])

	# debug prints, only print first 5 values
	printv(f"""\tx       : {LGN}{x[0:5]}{N}""")
	printv(f"""\ty       : {LGN}{y[0:5]}{N}""")
	printv(f"""\tlabels  : {LGN}{labels[0:5]}{N}""")

	# then write it
	printv(YW,"Printing file",labels,N)
	for j in range(0,len(y)):
		f.write(f"""{x[j]},{y[j]},{y_upper[j]},{y_lower[j]},{os.path.basename(allFiles[i])}\n""")

	if currGroup in tempmat:	# if group exist in dict then append
		tempmat[currGroup]["val"].append(myplot[0]["y"])
		tempmat[currGroup]["tot"] += 1
	else:
		tempmat[currGroup] = {}
		printv(LGN,"i=",i,N,"	New group with new trace!",allFiles)
		tempmat[currGroup]["name"] = os.path.basename(allFiles[i])
		tempmat[currGroup]["val"] = [(myplot[0]["y"])]
		tempmat[currGroup]["tot"] = 1
		tempmat[currGroup]["x"] = myplot[0]["x"]

# then calculate mean, stdev, stderr
for currGroup in tempmat:
	currmean = np.mean(tempmat[currGroup]["val"],axis=0)
	currsd   = np.std(tempmat[currGroup]["val"],axis=0)
	currse   = np.divide(currsd,math.sqrt(tempmat[currGroup]["tot"]))
	currname = tempmat[currGroup]["name"]
	printv(f"""Group {LCY}{currGroup}{N} (total files = {LGN}{tempmat[currGroup]['tot']}{N})\n""") # debug print

	# assign new variable to make it easy to read
	x = tempmat[currGroup]["x"]
	y = currmean
	y_upper = np.sum([currmean,currse],axis=0)
	y_lower = np.subtract(currmean,currse)
	labels = [currname]*len(x)

	# debug prints, only print first 5 values
	printv(f"""\tx       : {LGN}{x[0:5]}{N}""")
	printv(f"""\ty       : {LGN}{y[0:5]}{N}""")
	printv(f"""\ty_upper : {LGN}{y_upper[0:5]}{N}""")
	printv(f"""\ty_lower : {LGN}{y_lower[0:5]}{N}""")
	printv(f"""\tlabels  : {LGN}{labels[0:5]}{N}""")

	# then write it
	printv(YW,"Printing group",currGroup,LPR,"labels=",labels[i],N,"length=",len(x))
	for i in range(0,len(x)):
		f.write(f"""{x[i]},{y[i]},{y_upper[i]},{y_lower[i]},{currGroup}\n""")

# close the plotly csv File
f.close()

printv("Wrote plotly input file into",plotlycsvFile)


sys.exit(0)
############################
# BELOWS ARE NOTES, IGNORE #
############################

# TO MAKE plotProfile Graph
#bodycmd = f"""plotProfile -m matrix/{outnametype}.matrix.gz -o png/{outnametype}.png --perGroup --colors green green red red blue blue --plotTitle body"""
#printv(bodycmd)
#os.system(bodycmd)

# HEATMAPPER MATRIX MANIPULATION NOTES
"""
matrix manipulation
def __init__(self, regions, matrix, group_boundaries, sample_boundaries,
def get_matrix(self, group, sample):
def get_num_samples(self):
def get_num_groups(self):
def set_group_labels(self, new_labels):
def set_sample_labels(self, new_labels):
def set_sorting_method(self, sort_method, sort_using):
def get_regions(self):
def sort_groups(self, sort_using='mean', sort_method='no', sample_list=None):
def hmcluster(self, k, evaluate_silhouette=True, method='kmeans', clustering_samples=None):
def computeSilhouette(self, k):
def removeempty(self):
def flatten(self):
"""

# EXAMPLE RUN
"""
python3 immain.py -i bed/goldstd.bed \
-q bw/SRX1070678_NT2_DRIP-seq_1.hg38.bw bw/SRX1070680_NT2_DRIP-seq_RNaseA.hg38.bw \
-p bw/SRX6427717_DMSO_qDRIP-seq_1.hg38.bw bw/SRX6427718_DMSO_qDRIP-seq_2.hg38.bw \
-n bw/SRX6427715_siGL3_qDRIP-seq_andRH.hg38.bw bw/SRX6427722_DRB_qDRIP-seq_andRH.hg38.bw
"""
