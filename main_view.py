# import dash
# import dash_core_components as dcc
# import dash_html_components as html
# import dash_table
# from dash.dependencies import Input, Output
# import plotly.express as px
# import pandas as pd
# import plotly.graph_objects as go
# import plotly.figure_factory as ff
# import numpy as np
# from scipy.spatial.distance import pdist, squareform
# import dash_bootstrap_components as dbc
# from collections import OrderedDict
#
# app = dash.Dash(external_stylesheets=[dbc.themes.FLATLY,
#                                       'https://codepen.io/chriddyp/pen/bWLwgP.css'])
#
#
# samplesraw = pd.read_csv("data/RMapDB_samples_10_22_2020.csv")
# read_qc = pd.read_csv("data/read_qc.csv")
# bam_qc = pd.read_csv("data/bam_qc.csv")
# corr = pd.read_csv("data/corr.csv")
# corr.index = corr.columns
# corr.index = corr.index.rename("Sample Name")
# anno = pd.read_csv("data/annotations.csv")
# samplesraw['id'] = [i for i in range(len(samplesraw.index))]
# samplesraw = samplesraw.rename(columns={'SRX': 'Accession (SRA)', 'Cell': 'Tissue',
#                                         'mode': 'Protocol', 'sample_name': 'Sample Name',
#                                         'genome': 'Genome', 'paired_end': 'Paired End',
#                                         'strand_specific': "Strand Specific", 'Other': 'Treatment/Other',
#                                         'moeity': 'Moeity', 'ip_type': 'IP Type',
#                                         'read_length': 'Read Length', 'Shearing_method': "Shearing Method",
#                                         'ControlType': "Control"})
# samplesraw.index = samplesraw['Sample Name']
# main_cols = ['Sample Name', 'Accession (SRA)', 'Protocol', 'Species', 'Tissue', 'Condition', 'Group']
# samples_main = samplesraw[main_cols]
#
#
# app.layout = dbc.Container([
#     html.H1("RMapDB - Main"),
#     html.Hr(),
#     dbc.Row([
#         dbc.Col([
#             html.Div([
#                 dcc.RadioItems(id='heat-selector',
#                                options=[{'label': 'Cluster', 'value': 'Cluster'},
#                                         {'label': 'Protocol', 'value': 'Protocol'},
#                                         {'label': 'Group', 'value': 'Group'}],
#                                value='Cluster')
#             ])
#         ], md=2),
#         dbc.Col([
#             html.Div([
#                 dcc.Graph(id='main-heatmap')
#             ])
#         ], md=10)
#     ])
#
# ], fluid=True)
#
#
# @app.callback(
#     Output(component_id='main-heatmap', component_property='figure'),
#     [Input(component_id='heat-selector', component_property='value')]
# )
# def plot_corr_heatmap(selection):
#     print(selection)
#     fig = ff.create_dendrogram(corr, orientation='bottom', labels=corr.index)
#     fig.update_layout(plot_bgcolor='white')
#     for i in range(len(fig['data'])):
#         fig['data'][i]['yaxis'] = 'y2'
#     # Create Side Dendrogram
#     dendro_side = ff.create_dendrogram(corr, orientation='right')
#     dendro_side.update_layout(plot_bgcolor='white')
#     for i in range(len(dendro_side['data'])):
#         dendro_side['data'][i]['xaxis'] = 'x2'
#     # Add Side Dendrogram Data to Figure
#     for data in dendro_side['data']:
#         fig.add_trace(data)
#     # Create Heatmap
#     dendro_leaves = dendro_side['layout']['yaxis']['ticktext']
#     dendro_leaves = list(map(int, dendro_leaves))
#     heat_data = corr.iloc[dendro_leaves, dendro_leaves]
#     heatmap = [
#         go.Heatmap(
#             x=dendro_leaves,
#             y=dendro_leaves,
#             z=heat_data,
#             colorscale="RdBu_r",
#             zmid=0
#         )
#     ]
#     heatmap[0]['x'] = fig['layout']['xaxis']['tickvals']
#     heatmap[0]['y'] = dendro_side['layout']['yaxis']['tickvals']
#     for data in heatmap:
#         fig.add_trace(data)
#
#     # Get the categories heatmap
#     import numpy as np
#     from sklearn.preprocessing import LabelEncoder
#     lb_make = LabelEncoder()
#     samplesraw['Protocol_Dummy'] = lb_make.fit_transform(samplesraw['Protocol'])
#     lst = samplesraw['Protocol_Dummy'].tolist()
#     sample_dendsort = samplesraw.iloc[dendro_leaves]
#     pdnow = sample_dendsort[['Protocol_Dummy']]
#     pdnow.index = sample_dendsort[['Protocol']]
#     annomap = go.Heatmap(z=pdnow.T, x=dendro_leaves, showlegend=False,
#                          text=pdnow.index, showscale=False)
#     annomap['x'] = fig['layout']['xaxis']['tickvals']
#     annomap['xaxis'] = 'x'
#     annomap['y'] = dendro_side['layout']['yaxis']['tickvals']
#     annomap['yaxis'] = 'y3'
#
#     fig.add_trace(annomap)
#
#     # Edit Layout
#     fig.update_layout({'width': 800, 'height': 800,
#                        'showlegend': False, 'hovermode': 'closest', })
#     # Edit xaxis
#     fig.update_layout(xaxis={'domain': [.15, 1],
#                              'mirror': False,
#                              'showgrid': False,
#                              'showline': False,
#                              'zeroline': False,
#                              'ticks': ""})
#     # Edit xaxis2
#     fig.update_layout(xaxis2={'domain': [0, .15],
#                               'mirror': False,
#                               'showgrid': False,
#                               'showline': False,
#                               'zeroline': False,
#                               'showticklabels': False,
#                               'ticks': ""})
#
#     # Edit yaxis
#     fig.update_layout(yaxis={'domain': [0, .85],
#                              'mirror': False,
#                              'showgrid': False,
#                              'showline': False,
#                              'zeroline': False,
#                              'showticklabels': False,
#                              'ticks': ""
#                              })
#     # Edit yaxis2
#     fig.update_layout(yaxis2={'domain': [.87, 1],
#                               'mirror': False,
#                               'showgrid': False,
#                               'showline': False,
#                               'zeroline': False,
#                               'showticklabels': False,
#                               'ticks': ""})
#
#     # Edit yaxis3
#     fig.update_layout(yaxis3={'domain': [0.82, .87],
#                               'mirror': False,
#                               'showgrid': False,
#                               'showline': False,
#                               'zeroline': False,
#                               'showticklabels': False,
#                               'ticks': ""})
#
#     # fig['layout']['xaxis1'].update(autorange="reversed")
#     # Plot!
#     return fig
#
#
# server = app.server
#
# if __name__ == '__main__':
#     app.run_server(debug=True)
#
#
#
