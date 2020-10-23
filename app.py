import json

import dash
import dash_core_components as dcc
import dash_html_components as html
import dash_table
from dash.dependencies import Input, Output
import plotly.express as px
import pandas as pd
import numpy as np
import dash_bootstrap_components as dbc
from collections import OrderedDict

app = dash.Dash(external_stylesheets=[dbc.themes.FLATLY])

samplesraw = pd.read_csv("data/RMapDB_samples_10_22_2020.csv")
read_qc = pd.read_csv("data/read_qc.csv")
bam_qc = pd.read_csv("data/bam_qc.csv")
corr = pd.read_csv("data/corr.csv")
anno = pd.read_csv("data/annotations.csv")
anno_rep = anno[anno.annotate_type == "rep"]
anno_gene = anno[anno.annotate_type == "gene"]
anno_RNA = anno[anno.annotate_type == "RNA"]
samplesraw['id'] = [i for i in range(len(samplesraw.index))]
samplesraw = samplesraw.rename(columns={'SRX': 'Accession (SRA)', 'Cell': 'Tissue',
                                        'mode': 'Protocol', 'sample_name': 'Sample Name',
                                        'genome': 'Genome', 'paired_end': 'Paired End',
                                        'strand_specific': "Strand Specific", 'Other': 'Treatment/Other',
                                        'moeity': 'Moeity', 'ip_type': 'IP Type',
                                        'read_length': 'Read Length', 'Shearing_method': "Shearing Method",
                                        'ControlType': "Control"})
main_cols = ['Sample Name', 'Accession (SRA)', 'Protocol', 'Species', 'Tissue', 'Condition', 'Group']

samples_main = samplesraw[main_cols]



# controls = dbc.Card(
#     [
#         dbc.FormGroup(
#             [
#                 dbc.Label("X variable"),
#                 dcc.Dropdown(
#                     id="x-variable",
#                     options=[
#                         {"label": col, "value": col} for col in iris.columns
#                     ],
#                     value="sepal length (cm)",
#                 ),
#             ]
#         ),
#         dbc.FormGroup(
#             [
#                 dbc.Label("Y variable"),
#                 dcc.Dropdown(
#                     id="y-variable",
#                     options=[
#                         {"label": col, "value": col} for col in iris.columns
#                     ],
#                     value="sepal width (cm)",
#                 ),
#             ]
#         ),
#         dbc.FormGroup(
#             [
#                 dbc.Label("Cluster count"),
#                 dbc.Input(id="cluster-count", type="number", value=3),
#             ]
#         ),
#     ],
#     body=True,
# )
#


app.layout = dbc.Container([
    html.H1("RMapDB"),
    html.Hr(),
    dbc.Row([
        dbc.Col([
            html.Div(id='sample-info-card')
        ], md=3),
        dbc.Col([
            html.Div([
                dash_table.DataTable(
                    id='main-table',
                    columns=[{"name": i, "id": i} for i in samples_main.columns],
                    data=samples_main.to_dict('records'),
                    cell_selectable=False,
                    fill_width=True,
                    page_size=10,
                    row_selectable="single",
                    selected_rows=[0],
                    sort_action="native",
                    style_cell={'padding': '5px'},
                    style_cell_conditional=[
                        {
                            'if': {'column_id': c},
                            'textAlign': 'left'
                        } for c in ['Sample Name']
                    ]
                )
            ], style={'padding': '10px'}),
        ], md=4)
    ]),
    dbc.Row([
        dbc.Col([
            html.Div([
                dcc.Graph(id="annotation_chart")
            ])
        ], md=10)
    ])

], fluid=True)


@app.callback(
    Output(component_id='sample-info-card', component_property='children'),
    [Input(component_id='main-table', component_property='selected_rows')]
)
def update_output_div(row):
    row_info = samplesraw[['Sample Name', 'Genome', 'Tissue', 'Genotype', 'Treatment/Other', 'Protocol', 'Paired End',
                                 'Strand Specific', 'Moeity', 'IP Type', 'Read Length',
                                 'Shearing Method', 'Control']].iloc[row].to_dict(into=OrderedDict)
    sample_name = samplesraw['Accession (SRA)'].iloc[row]
    sample_info_card = dbc.Card([
        dbc.CardBody(
            [
                html.H5(sample_name, className="card-title"),
                html.Div([html.Div([html.P([str(key) + ": ", html.Strong(str(list(val.values())[0]))], className="card-text")]) for key, val in row_info.items()])
            ]
        )
    ])
    return sample_info_card


@app.callback(
    Output(component_id='annotation_chart', component_property='figure'),
    [Input(component_id='main-table', component_property='selected_rows')]
)
def plot_annotations(row):
    sample_name = samplesraw['Sample Name'].iloc[row[0]]
    # Get max-min for linked plots
    anno_now = anno[anno["sample"] == sample_name]
    maxval = anno_now['Log2 Ratio (obs/exp)'].max() * 1.05
    minval = anno_now['Log2 Ratio (obs/exp)'].min() * 1.05
    if minval > 0:
        minval = anno_now['Log2 Ratio (obs/exp)'].min() * .95  # Case when minval isn't negative
    # Get plotting data
    anno_RNA_now = anno_RNA[anno_RNA['sample'] == sample_name]
    fig = px.bar(anno_now, x="Annotation", facet_col="annotate_type",
                 y='Log2 Ratio (obs/exp)', color="annotate_type",
                 title="Genomic Feature Enrichment")
    fig.update_layout(transition_duration=500, showlegend=False)
    fig.for_each_annotation(lambda a: a.update(text=a.text.split("=")[-1]))
    fig.update_xaxes(title=None, matches=None)
    return fig



server = app.server


if __name__ == '__main__':
    app.run_server(debug=True)





