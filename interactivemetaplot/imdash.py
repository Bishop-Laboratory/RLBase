#dash
import plotly.graph_objs as go
import plotly.express as px
import dash
import dash_core_components as dcc
import dash_html_components as html
import pandas as pd
import sys
from flask import request


#Todo: put parser
df = pd.read_csv("interactivemetaplot\plotly\goldstd.bed,SRX1070678_NT2_DRIP-seq_1.hg38.bw,SRX6427717_DMSO_qDRIP-seq_1.hg38.bw,SRX6427715_siGL3_qDRIP-seq_andRH.hg38.bw,b5000,a5000,body.csv")

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
    title='YeS',
    hovermode="x"
)


app = dash.Dash()
app.layout = html.Div([
    dcc.Graph(figure=fig)
])

app.run_server(debug=True, use_reloader=True)
