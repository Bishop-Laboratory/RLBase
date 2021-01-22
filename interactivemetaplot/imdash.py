import sys
import argparse

#dash
import plotly.graph_objs as go
import dash
import dash_core_components as dcc
import dash_html_components as html
import pandas as pd
from flask import request


print("") # add paragraph so text wont be too clutter
parser = argparse.ArgumentParser()
parser.add_argument("-i", "--inputcsv",       metavar="File.csv",      type=str, nargs="?", help="csv file from immain.py",required=True)
parser.add_argument("-Q", "--QUIET",     help="make it not print anything [default: False]",action="store_true")
print("")


args = parser.parse_args()

df = pd.read_csv(args.inputcsv)


df = df[(df['labels'] == "ERX2277510_E-MTAB-6318DRIP_mOHT.hg38.bw")|(df['labels'] == "SRX1025890_TC32_NT_DRIP.hg38.bw")]

labelz = df['labels'].unique().tolist()
#colorz = ['rgb(0,100,0)'] * len(labelz)
#fillsz = ['rgba(0,250,0,0.2)'] * len(labelz)
colorz = ['rgb(155,0,0)','rgb(0,100,0)','rgb(0,0,155)']
fillsz = ['rgba(250,50,0,0.2)','rgba(0,250,0,0.2)','rgba(0,50,250,0.2)']

print(df)
print(len(labelz))
fig = go.Figure()

# Draw geom_ribbon-like stderr shades
#for i in range(0,3):
for i in range(0,len(labelz)):
    curr = df[df['labels'] == labelz[i]]

    x = curr['x'].tolist()
    x_rev = x[::-1]
    y = curr['y'].tolist()
    y_upper = curr['y_upper'].tolist()
    y_lower = curr['y_lower'].tolist()
    y_lower = y_lower[::-1]

    fig.add_trace(go.Scatter(
        name       = labelz[i],
        x          =  x + x_rev,
        y          =  y_upper + y_lower,
        fill       = 'toself',
        fillcolor  = fillsz[i],
        line       = dict(width=0),
        line_color = 'rgba(255,255,255,0)',
        showlegend = False
    ))

#for i in range(0,3):
for i in range(0,len(labelz)):
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
    title='YeS2',
    hovermode="x",
    xaxis = dict(
        tickmode = 'array',
        tick0    = 0,
        tickvals = [0,5000/20000*100,10000/20000*100,15000/20000*100,99],
        ticktext = ["-5kb","TSS","genebody","TTS","5kb"]
    )
)


app = dash.Dash()
app.layout = html.Div([
    dcc.Graph(figure=fig)
])

app.run_server(debug=True, use_reloader=True)
