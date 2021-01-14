import os
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
	
def get_plotly_color(type):
	if (type == 0): #neg control
		mycol = "blue"
	elif (type == 1): #query
		mycol = "green"
	elif (type == 2): #pos control
		mycol = "red"
	return mycol

# parser argument
print("") # add paragraph so text wont be too clutter
parser = argparse.ArgumentParser()
parser.add_argument("-i", "--bed",       metavar="File.bed",      type=str, nargs="?", help="Bed File.",required=True)
parser.add_argument("-q", "--que",       metavar="que.bw",        type=str, nargs="+", help="Query bigWig File(s) - separate by space.",required=True)
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
os.makedirs("sortedRegions", exist_ok=True)
os.makedirs("svg", exist_ok=True)
os.makedirs("png", exist_ok=True)

# define input and input names, as well as output names if not defined with -o
bedFile, bedFileName = args.bed, os.path.basename(args.bed)
queFile, queFileName = args.que, os.path.basename(args.que[0])
outName = f"""{bedFileName},{queFileName}"""

allFile = queFile
queType = [1]*len(allFile)
allGroup = queType
# check if there's positive control files
# also add pos and neg control filenames to default outName
if args.pos != None:
	posFileName = os.path.basename(args.pos[0])
	posFile = args.pos
	posType = [2]*len(posFile)
	allGroup = allGroup + posType
	allFile = allFile + posFile
	outName = f"""{outName},{posFileName}"""
else:
	posFileName = ""
	posFile = ["None"]

# check if there's negative controlf iles
if args.neg != None:
	negFileName = os.path.basename(args.neg[0])
	negFile = args.neg
	negType = [0]*len(negFile)
	allGroup = allGroup + negType
	allFile = allFile + negFile
	outName = f"""{outName},{negFileName}"""
else:
	negFileName = ""
	negFile = ["None"]

# outName might be too long, check this later
outName = f"""{outName},b{args.before},a{args.after}"""
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

printv(argsprint)


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
"sample_labels":queFile,
"group_labels":allGroup,
"sort regions":1,
"sort using":None
}

type = args.type

printv(allFile)
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
	sys.exit(0)


# computeMatrix run
# Todo: make option to not re-run matrix if exist
matrixFile = f"""matrix/{outNametype}.matrix.gz"""
hmp.heatmapper.computeMatrix(dm,score_file_list=allFile,regions_file=bedFile,parameters=parameters)

hmp.heatmapper.save_matrix(dm,file_name=matrixFile) # save matrix
#hmp.heatmapper.read_matrix_file(dm,matrixFile) # read matrix

plotlycsvFile = f"""plotly/{outNametype}.csv"""
f = open(plotlycsvFile,"w")

f.write("x,y,y_upper,y_lower,labels\n")

tempmat = {}

# turn heatmapper matrix object into numpy array
# get mean, stdev, stderr
for i in range(0,len(allFile)):


	# get color: blue for neg ctrl, green for query, red for pos ctrl
	mycol = get_plotly_color(allGroup[i]) 

	# get matrix
	mymatrix = dm.matrix.get_matrix(group=0,sample=i)['matrix']
	
	# turn into simple array trace
	# Todo: this is weird though, gonna check if there's better method
	myplot = hmpu.plotly_single(mymatrix,average_type="mean",color=mycol,label=allFile[i])
	
	# append data from each group to numpy readable format
	currGroup = allGroup[i]
	printv("\n","\n","".join(["-"]*10)) # debug print
	printv(LGN,i,N,"currGroup",LCY,currGroup,N,"currFile",LYW,allFile[i],N)	# debug print

	if (currGroup in tempmat):	# if group exist in dict then append
		tempmat[currGroup]["val"].append(myplot[0]["y"])
		tempmat[currGroup]["tot"] += 1
	else:	# else make new dict
		tempmat[currGroup] = {}
		printv("	New group with new trace!")
		tempmat[currGroup]["name"] = "query" if currGroup == 1 else "pos control" if currGroup == 2 else "neg control" if currGroup == 0 else "unknown group"
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
	for i in range(0,len(x)):
		f.write(f"""{x[i]},{y[i]},{y_upper[i]},{y_lower[i]},{labels[i]}\n""")

# close the plotly csv File
f.close()

printv("Wrote plotly input file into",plotlycsvFile)




##################
# DASH PART      #
# NOT YET TESTED #
# TOGETHER SO    #
# COMMENTED OUT  #
##################

"""
df = pd.read_csv(plotlycsvFile)

labelz = ['neg control','query','pos control']
colorz = ['rgb(155,0,0)','rgb(0,100,0)','rgb(0,0,155)']
fiillz = ['rgba(250,50,0,0.2)','rgba(0,250,0,0.2)','rgba(0,50,250,0.2)']

fig = go.Figure()

for i in range(0,3):
    curr = df.loc[df['labels'] == labelz[i]]
    x = curr['x'].tolist()
    x_rev = x[::-1]

    y = curr['y'].tolist()
    y_upper = curr['y_upper'].tolist()
    y_lower = curr['y_lower'].tolist()
    y_lower = y_lower[::-1]
    fig.add_trace(go.Scatter(
        x=x+x_rev,
        y=y_upper+y_lower,
        fill='toself',
        fillcolor=fiillz[i],
        line_color='rgba(255,255,255,0)',
        showlegend=False,
        name=labelz[i]
    ))

for i in range(0,3):
    curr = df.loc[df['labels'] == labelz[i]]
    x = curr['x']
    y = curr['y']

    fig.add_trace(go.Scatter(
        x=x,y=y,
        line_color=colorz[i],
        name=labelz[i]
    ))

fig.update_layout(
    yaxis_title='Signal',
    xaxis_title='',
    title=outNametype,
    hovermode="x"
)


app = dash.Dash()
app.layout = html.Div([
    dcc.Graph(figure=fig)
])

app.run_server(debug=True, use_reloader=True)
"""

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
