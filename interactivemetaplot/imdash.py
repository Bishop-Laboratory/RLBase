import sys
import re
import math
import argparse
import pandas as pd

#dash
import plotly.graph_objs as go
import dash
import dash_bootstrap_components as dbc
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import dash_table
import plotly.express as px
from flask import request


prevpage = {0:-1}

colorz = ['rgb(0,100,0)'] * 100
fillsz = ['rgba(0,250,0,0.2)'] * 100

all = pd.read_csv("../data/RMapDB_samples_10_22_2020.csv")
#all = all.loc[:,["sample_name","clean_name"]]
#for col in all.columns:
#    print(col)

#sys.exit(0)
df = pd.read_csv("plotly/goldstd_allbw_body.csv")

df["sample_name"] = df["labels"].str.extract(r"^(.+)\..+\.bw$")
df.loc[df.sample_name != df.sample_name,"sample_name"] = df.loc[df.sample_name != df.sample_name,"labels"]

a = pd.merge(df, all, on="sample_name", how="left").fillna(0)
a.loc[a.sample_name == 0,"sample_name"] = a.loc[a.sample_name == 0,"labels"]
a.loc[a.clean_name == 0,"clean_name"] = a.loc[a.clean_name == 0,"sample_name"]
a["clean_name"] = a["clean_name"].astype(str)
a["Species"] = a["Species"].astype(str)
a["Genotype"] = a["Genotype"].astype(str)
a["clean_name2"] = a["clean_name"] + " (" + a["Species"] + ", " + a["Genotype"] + ")" #a[["clean_name","Species","Genotype"]].agg("_".join, axis=1)
a.loc[a.Species == "0","clean_name2"] = a.loc[a.Species == "0","clean_name"]

df = a

labelz = df.sample_name.unique().tolist()
namez  = df.clean_name2.unique().tolist()

a = {"index":list(range(1,1+len(labelz))),"clean_name":namez,"sample_name":labelz}
a = pd.DataFrame(a)

df2 = a
print(df2)
named_colorscales = px.colors.qualitative.Plotly
TABLE = []
colorz = named_colorscales * (int(len(labelz)/len(named_colorscales))+1)
fillsz = named_colorscales * (int(len(labelz)/len(named_colorscales))+1)

#df2 = pd.DataFrame.from_dict({"index":list(range(1,1+len(labelz))),"clean_name":clean_name,"sample_name":sample_name}) #"sample_name":labelz,})

#print("DF2 =\n",df2,"\n\n")

#print(len(df2))
#print("df:\n",df)
#print("labelz:\n",labelz[0:5])
#print("df2:\n",df2["sample_name"][0:5])
app = dash.Dash(external_stylesheets=[dbc.themes.BOOTSTRAP])

#app = dash.Dash(__name__)
mypagesize = len(labelz)

currpage = {}
uselabelz = {}
totalpage = int(len(labelz)/mypagesize)+1
for i in range(0,totalpage):
    currpage2 = [0]*mypagesize
    currpage[i] = currpage2



mydashtable = dash_table.DataTable(
        id='imdashtable',
        columns=[
            {"name": i, "id": i, "deletable": False, "selectable": False} for i in df2.columns
        ],
        data=df2.to_dict('records'),
        editable=True,
        #filter_action="native",
        #sort_action="native",
        sort_mode="multi",
        column_selectable="single",
        row_selectable="multi",
        row_deletable=False,
        selected_columns=[],
        selected_rows=[300,301,299],
        page_action="custom",
        page_current= 0,
        page_size= mypagesize,
        page_count=int((len(labelz)-1)/mypagesize)+1,
        persistence=True
    )
# Table layout
app.layout = dbc.Container(
    [
        html.H1("Interactive Metaplot"),
        html.Hr(),
        dbc.Row(dbc.Col(html.Div(id='imdashtable-container'),lg=12)),
        dbc.Row(dbc.Col(mydashtable,width="auto"))
    ]
)

# table page
@app.callback(
    Output('imdashtable', 'data'),
    Input('imdashtable', "page_current"),
    Input('imdashtable', "page_size"),
    Input('imdashtable', "selected_rows")
    )
def update_table(page_current,page_size,selected_rows):

    return df2.iloc[
        page_current*page_size:(page_current+ 1)*page_size
    ].to_dict('records')


# Table callback
@app.callback(
    Output("imdashtable", "style_data_conditional"),
    [Input("imdashtable", "selected_columns")]
)

# when selected, change background of selected table
def update_styles(selected_columns):
    return [{
        'if': { 'column_id': i },
        'background_color': '#D2F3FF'
    } for i in selected_columns]



################################

# Line chart

@app.callback(
    Output('imdashtable-container', "children"),
   Input('imdashtable', "derived_virtual_data"),
   Input('imdashtable', "derived_virtual_selected_rows")
   )

def update_line_chartz(rows,drows):
    print("\n\n----------------------")
    print("----------------------")

    fig = go.Figure()

    allrows = pd.DataFrame(rows)

    print("Using drows",drows)
    rows = allrows if drows is None else allrows.iloc[drows]

    print("rows=\n",rows,"type=",type(rows),"\n\n")
    print("drows=\n",drows,"\n\n")


    print(len(df2))

    if drows is None:
         print("Here!")
         return [dcc.Graph(id="sample_name",figure=fig)]


    # Draw geom_ribbon-like stderr shades

    labelz = rows['sample_name'].tolist()


    for i in range(0,len(labelz)):
        curr = df.loc[df['sample_name'] == labelz[i]]

        x = curr['x'].tolist()
        x_rev = x[::-1]
        y = curr['y'].tolist()
        y_upper = (curr['y_upper']).tolist()
        y_lower = (curr['y_lower']).tolist()
        y_lower = y_lower[::-1]

        fig.add_trace(go.Scatter(
            name       = labelz[i],
            x          =  x + x_rev,
            y          =  y_upper + y_lower,
            fill       = 'toself',
            fillcolor  = fillsz[i],
            line       = dict(width=0),
            line_color = 'rgba(255,255,255,0)',
            opacity    = 0.25,
            showlegend = False
        ))

        fig.add_trace(go.Scatter(
            x=x,y=y,
            line_color=colorz[i],
            name=labelz[i]
        ))

    fig.update_layout(
        yaxis_title='Signal',
        xaxis_title='',
        title='',
        hovermode="x",
        xaxis = dict(
            tickmode = 'array',
            tick0    = 0,
            tickvals = [0,5000/20000*100,10000/20000*100,15000/20000*100,99],
            ticktext = ["-5kb","TSS","genebody","TTS","5kb"],
            automargin = True
        ),
        yaxis = dict(automargin = True),
        margin={"t":100,"l":100,"r":100}
    )
    return [dcc.Graph(id="sample_name",figure=fig)]


app.run_server(debug=True)
