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

#plotly
#import plotly.express as px

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
parser.add_argument("-t", "--type",      metavar="scale-regions", type=str, nargs="?", help="'scale-regions' or 'reference-point' from computeMatrix.",default="scale-regions")
parser.add_argument("-b", "--before",    metavar="5000",          type=int, nargs="?", help="bp [B]efore ref. point. -b from computeMatrix.",default=5000)
parser.add_argument("-a", "--after",     metavar="5000",          type=int, nargs="?", help="bp [A]fter ref. point. -a from computeMatrix.",default=5000)
parser.add_argument("-z", "--skipZeros", metavar="TRUE",          type=str, nargs="?", help="--skipZeros from computeMatrix.",default="--skipZeros")
print("")

args = parser.parse_args()
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
dm = hmp.heatmapper()

# define parameters for computeMatrix (have to define EVERYTHNG or it dies...)

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
"group_labels":allGroup,
"sort regions":1,
"sort using":None
}

type = args.type

print(allFile)
print(allGroup)


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


# draw TSS, body, then TTS
if (args.type == "scale-regions"):
	type = "body"
	print("type is",args.type,"is inputted as",type,"for computeMatrix parameters")
	# draw body
	parameters["ref point"]  = None
	parameters["body"]       = 10000
	parameters["upstream"]   = 5000
	parameters["downstream"] = 5000
	#hmp.heatmapper.computeMatrix(dm,score_file_list=allFile,regions_file=bedFile,parameters=parameters)	# computeMatrix run
	outNamebody = f"""{outName},body"""
	#hmp.heatmapper.save_matrix(dm,file_name=f"""matrix/{outNamebody}.matrix.gz""")	# save matrix
	#bodycmd = f"""plotProfile -m matrix/{outNamebody}.matrix.gz -o png/{outNamebody}.png --perGroup --colors green green red red blue blue --plotTitle body"""
	#print(bodycmd)
	#os.system(bodycmd)
	matrixFile = f"""matrix/{outNamebody}.matrix.gz"""
	hmp.heatmapper.read_matrix_file(dm,matrixFile)
	
	plotlycsvFile = f"""plotly/{outName},body.csv"""
	f = open(plotlycsvFile,"w")

	print("")
#	print(dm.matrix.get_num_samples())
#	print(dm.matrix.get_num_groups())
#	print(dm.matrix.get_matrix(group=0,sample=0))
#	print(dm.matrix.get_matrix(group=0,sample=1))
	print("group labels:",dm.matrix.group_labels)
	print("sample labels:",dm.matrix.sample_labels)
	print("")
	# non-plotly plot. Copy pasted from docs. Also doesn't work & died due to tuple out of bound.

	tempmat = {}
	newmat = {}

	for i in range(0,len(allFile)):
		mycol = get_plotly_color(allGroup[i])
		mymatrix = dm.matrix.get_matrix(group=0,sample=i)['matrix']
		myplot = hmpu.plotly_single(mymatrix,average_type="mean",color=mycol,label=allFile[i])#["blue","red"],label=["label1","label2"],plot_type=["line","line"])
		
		currGroup = allGroup[i]
		print("\n","\n","".join(["-"]*10))
		print(LGN,i,N,"currGroup",LCY,currGroup,N,"currFile",LYW,allFile[i],N)
		if (currGroup in tempmat):
			
			print(f"""group {currGroup} file {allFile[i]} added prev y with new y""")
#			print(f"""prev y: {tempmat[currGroup]['y'][0:5]}""")
#			print(f"""curr y: {myplot[0]['y'][0:5]}""")
#			print(f"""add  y: {tempmat[currGroup]['y'][0:5]}""")
			tempmat[currGroup]["val"].append(myplot[0]["y"])
			tempmat[currGroup]["tot"] += 1
		else:
			tempmat[currGroup] = {}
			print("	New group with new trace!")
			tempmat[currGroup]["name"] = "query" if currGroup == 1 else "pos control" if currGroup == 2 else "neg control" if currGroup == 0 else "unknown group"
			tempmat[currGroup]["val"] = [(myplot[0]["y"])]
			tempmat[currGroup]["tot"] = 1
			tempmat[currGroup]["x"] = myplot[0]["x"]
#			print(f"""curr y: {myplot[0]['y'][0:5]}""")


	f.write("x,y,y_upper,y_lower,labels\n")
	for currGroup in tempmat:
		currmean = np.mean(tempmat[currGroup]["val"],axis=0)
		currsd   = np.std(tempmat[currGroup]["val"],axis=0)
		currse   = np.divide(currsd,math.sqrt(tempmat[currGroup]["tot"]))
		currname = tempmat[currGroup]["name"]

		print("")
		print(f"""Group {LCY}{currGroup}{N} (total files = {LGN}{tempmat[currGroup]['tot']}{N})""")
#		print(f"""\tvalues  : {LGN}{tempmat[currGroup]['val'][0:5]}{N}""")
#		print(f"""\tmean    : {LGN}{currmean}{N}""")
#		print(f"""\tmean len  : {len(currmean)}""")
#		print(f"""\tsd      : {LGN}{currsd}{N}""")
#		print(f"""\tsd len  : {len(currsd)}""")
#		print(f"""\tse      : {LGN}{currse}{N}""")
#		print(f"""\tse len  : {len(currse)}""")

		x = tempmat[currGroup]["x"]
		print(f"""\tx       : {LGN}{x}{N}""")

		y = currmean
		print(f"""\ty       : {LGN}{y}{N}""")

		y_upper = np.sum([currmean,currse],axis=0)
		print(f"""\ty_upper : {LGN}{y_upper}{N}""")

		y_lower = np.subtract(currmean,currse)
		print(f"""\ty_lower : {LGN}{y_lower}{N}""")

		labels = [currname]*len(x)
		print(f"""\tlabels  : {LGN}{labels}{N}""")
		print("")

		for i in range(0,len(x)):
			f.write(f"""{x[i]},{y[i]},{y_upper[i]},{y_lower[i]},{labels[i]}\n""")

	f.close()
	print("written into plotlycsvFile",plotlycsvFile)

elif (args.type == "TSS"):
	# draw TSS
	parameters['ref point']  = "TSS"
	parameters["body"]       = 0
	parameters["upstream"]   = 5000
	parameters["downstream"] = 5000
	hmp.heatmapper.computeMatrix(dm,score_file_list=queFile,regions_file=bedFile,parameters=parameters)	# computeMatrix run
	outNameTSS = f"""{outName},TSS"""
	hmp.heatmapper.save_matrix(dm,file_name=f"""matrix/{outNameTSS}.matrix.gz""")	# save matrix
	TSScmd = f"""plotProfile -m matrix/{outNameTSS}.matrix.gz -o png/{outNameTSS}.png --perGroup --colors green red blue --plotTitle TSS"""
	print(TSScmd)
	os.system(TSScmd)
	myplot = hmpu.plot_single(dm,average_type="mean",color="red",label="label")#["blue","red"],label=["label1","label2"],plot_type=["line","line"])
	#myplot = hmpu.plotly_single(dm,average_type="mean",color="red",label="label")#["blue","red"],label=["label1","label2"],plot_type=["line","line"])

#can delete this as it's same as TSS just change ref point to type
elif (args.type == "TTS"):
	# draw TTS
	parameters["ref point"]  = "TTS"
	parameters["body"]       = 0
	parameters["upstream"]   = 5000
	parameters["downstream"] = 5000
	hmp.heatmapper.computeMatrix(dm,score_file_list=queFile,regions_file=bedFile,parameters=parameters)	# computeMatrix run
	outNameTTS = f"""{outName},TTS"""
	hmp.heatmapper.save_matrix(dm,file_name=f"""matrix/{outNameTTS}.matrix.gz""")	# save matrix
	TTScmd = f"""plotProfile -m matrix/{outNameTTS}.matrix.gz -o png/{outNameTTS}.png --perGroup --colors green red blue --plotTitle TTS"""
	print(TTScmd)
	os.system(TTScmd)

else:
	print("type is not defined in parameter list (body, TSS, TTS)")
	print(args.type)



sys.exit(0)
# plotly plot. Still doesn't work and debugging. Died due to tuple out of bound.
#myplot = hmpu.plotly_single(dm,average_type="mean",color="red",label="label")#["blue","red"],label=["label1","label2"],plot_type=["line","line"])

# debug prints
#print("myplot is",myplot)

#print("dm is",dm)

# non-plotly plot. Copy pasted from docs. Also doesn't work & died due to tuple out of bound.
#fig = plt.figure()
#ax = fig.add_subplot(111)
#ax = hmpu.plot_single(ax, dm, 'mean', color='blue', label='red')
#leg = ax.legend()
#plt.savefig(f"""png/{outName}.png""")
#plt.close()


"""
		fig = go.Figure([
			go.Scatter(
	       	x=x,
	       	y=y,
	       	line=dict(color='rgb(0,100,80)'),
	       	mode='lines'
			),
			go.Scatter(
       		x=x+x[::-1], # x, then x reversed
       		y=y_upper+y_lower[::-1], # upper, then lower reversed
        		fill='toself',
        		fillcolor='rgba(0,100,80,0.2)',
        		line=dict(color='rgba(255,255,255,0)'),
        		hoverinfo="skip",
        		showlegend=False
			)
		])


app = dash.Dash()
app.layout = html.Div([
    dcc.Graph(figure=fig)
])

app.run_server(debug=True, use_reloader=False)  # Turn off reloader if inside Jupyter
"""
#		print(f"""\ngroup {currGroup}\n\tmean {currmean[0:5]}\n\tsd {currsd[0:5]}\n\tse {currsd[0:5]}\n""")
#		if (i == 0):
#			print(f"""{LGN}{i}{N}: {myplot}\n""")
#			print(LGN,"type",N,myplot[0]["type"])
#			print(LGN,"x",N,myplot[0]["x"][0:5])
#			print(LGN,"y",N,myplot[0]["y"][0:5])
#			print(LGN,"name",N,myplot[0]["name"])
#			print(LGN,"line",N,myplot[0]["line"])
#			print(LGN,"line color",N,myplot[0]["line"]["color"])
#			print(LGN,"showlegend",N,myplot[0]["showlegend"])

#df = px.data.gapminder().query("country=='Canada'") fig = px.line(df, x="year", y="lifeExp", title='Life expectancy in 
#Canada') fig.show()

