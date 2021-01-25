import sys
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

#print("") # add paragraph so text wont be too clutter
#parser = argparse.ArgumentParser()
#parser.add_argument("-i", "--inputcsv",       metavar="File.csv",      type=str, nargs="?", help="csv file from immain.py",required=True)
#parser.add_argument("-Q", "--QUIET",     help="make it not print anything [default: False]",action="store_true")
#print("")
#
#args = parser.parse_args()
#df = pd.read_csv(args.inputcsv)

df = pd.read_csv("plotly/goldstd_allbw_body.csv")

labelz = df.labels.unique().tolist()

named_colorscales = px.colors.qualitative.Plotly
TABLE = []
colorz = named_colorscales * (int(len(labelz)/len(named_colorscales))+1)
fillsz = named_colorscales * (int(len(labelz)/len(named_colorscales))+1)

df2 = pd.DataFrame.from_dict({"index":list(range(1,1+len(labelz))),"labels":labelz})

#print("DF2 =\n",df2,"\n\n")

#print(len(df2))
#print("df:\n",df)
#print("labelz:\n",labelz[0:5])
#print("df2:\n",df2["labels"][0:5])
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
         return [dcc.Graph(id="labels",figure=fig)]


    # Draw geom_ribbon-like stderr shades

    labelz = rows['labels'].tolist()


    for i in range(0,len(labelz)):
        curr = df.loc[df['labels'] == labelz[i]]

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
        title='YeS2',
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
    return [dcc.Graph(id="labels",figure=fig)]


app.run_server(debug=True)
