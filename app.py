import dash
import dash_core_components as dcc
import dash_html_components as html
import dash_table
from dash.dependencies import Input, Output
import plotly.express as px
import pandas as pd
import dash_bootstrap_components as dbc
from collections import OrderedDict
from io import BytesIO
import base64
from matplotlib.colors import rgb2hex
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.pyplot import gcf
import matplotlib
sns.set_theme(color_codes=True)
matplotlib.use('Agg')  # Prevents threading issues


app = dash.Dash(external_stylesheets=[dbc.themes.FLATLY])

samplesraw = pd.read_csv("data/RMapDB_samples_10_22_2020.csv")
read_qc = pd.read_csv("data/read_qc.csv")
bam_qc = pd.read_csv("data/bam_qc.csv")
corr = pd.read_csv("data/corr.csv")
corr.index = corr.columns
corr.index = corr.index.rename("Sample Name")
anno = pd.read_csv("data/annotations.csv")
samplesraw['id'] = [i for i in range(len(samplesraw.index))]
samplesraw = samplesraw.rename(columns={'SRX': 'Accession (SRA)', 'Cell': 'Tissue',
                                        'mode': 'Protocol', 'sample_name': 'Sample Name',
                                        'genome': 'Genome', 'paired_end': 'Paired End',
                                        'strand_specific': "Strand Specific", 'Other': 'Treatment/Other',
                                        'moeity': 'Moeity', 'ip_type': 'IP Type',
                                        'read_length': 'Read Length', 'Shearing_method': "Shearing Method",
                                        'ControlType': "Control"})
samplesraw.index = samplesraw['Sample Name']
main_cols = ['Accession (SRA)', 'Protocol', 'Species', 'Tissue', 'Condition', 'Group']
samples_main = samplesraw[main_cols]


def fig_to_uri(in_fig, close_all=True, **save_args):
    """
    Save a figure as a URI
    from https://github.com/4QuantOSS/DashIntro/blob/master/notebooks/Tutorial.ipynb
    :param in_fig:
    :param close_all:
    :return:
    """
    out_img = BytesIO()

    in_fig.savefig(out_img, format='png', **save_args)
    if close_all:
        in_fig.clf()
        plt.close('all')
    out_img.seek(0)  # rewind file
    encoded = base64.b64encode(out_img.read()).decode("ascii").replace("\n", "")
    return "data:image/png;base64,{}".format(encoded)


app.layout = dbc.Container([
    html.H1("RMapDB"),
    html.Hr(),
    dbc.Row([
        dbc.Col([
            html.Div([
                html.Img(id='corr_heatmap', src='', height='100%')
            ])
        ], md=5),
        dbc.Col([
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
                ], md=8),
            ]),
            dbc.Row([
                dbc.Col([
                    html.Div([
                        dcc.Graph(id='corr_scatter')
                    ])
                ], md=5),
                dbc.Col([
                    html.Div([
                        dcc.Graph(id="annotation_chart")
                    ])
                ], md=7)
            ])
        ], md=7),
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
                html.Div([html.Div([html.P([str(key) + ": ", html.Strong(str(list(val.values())[0]))],
                                           className="card-text")]) for key, val in row_info.items()])
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
    anno_now = anno[anno["sample"] == sample_name]
    fig = px.bar(anno_now, x="Annotation", facet_col="annotate_type",
                 y='Log2 Ratio (obs/exp)', color="annotate_type",
                 title="Genomic Feature Enrichment")
    fig.update_layout(transition_duration=500, showlegend=False)
    fig.for_each_annotation(lambda a: a.update(text=a.text.split("=")[-1]))
    fig.update_xaxes(title=None, matches=None)
    return fig


@app.callback(
    Output(component_id='corr_scatter', component_property='figure'),
    [Input(component_id='main-table', component_property='selected_rows')]
)
def plot_corr_scatter(row):
    sample_name = samplesraw['Sample Name'].iloc[row[0]]
    corr_now = corr[[sample_name]].join(samples_main)
    corr_now = corr_now.rename(columns={sample_name: "Pearson Correlation (R)"})
    corr_now = corr_now.sort_values(by=["Pearson Correlation (R)"])
    corr_now['rank'] = [i for i in range(len(corr_now.index))]

    protocols = samplesraw['Protocol']
    cmap = plt.get_cmap("tab20")
    clst = [rgb2hex(cmap(i)[:3]) for i in range(cmap.N)]
    protocol_lut = dict(zip(map(str, protocols.unique()), clst))
    fig = px.scatter(corr_now, x="rank", color_discrete_map=protocol_lut,
                     y='Pearson Correlation (R)', color="Protocol",
                     title="Correlation chart",
                     category_orders={'Protocol': list(protocols.unique())})
    fig.update_layout(transition_duration=500)
    fig.update_xaxes(title=None)
    return fig


@app.callback(
    Output(component_id='corr_heatmap', component_property='src'),
    [Input(component_id='main-table', component_property='selected_rows')]
)
def plot_corr_heatmap(row):
    current_sample = samplesraw['Sample Name'].iloc[row[0]]

    protocols = samplesraw['Protocol']
    protocol_lut = dict(zip(map(str, protocols.unique()), plt.get_cmap("tab20").colors))
    protocol_colors = pd.Series(protocols).map(protocol_lut)

    current_srx = samplesraw.loc[current_sample, 'Accession (SRA)']
    samplesraw[current_srx] = [str(i) for i in samplesraw.index.isin([current_sample])]
    samples = samplesraw[current_srx]
    sample_lut = dict(zip(map(str, ['True', 'False']), ['firebrick', 'gainsboro']))
    samples_colors = pd.Series(samples).map(sample_lut)

    full_colors = pd.DataFrame(protocol_colors).join(pd.DataFrame(samples_colors))
    g = sns.clustermap(corr, cmap='RdBu_r', center=0, dendrogram_ratio=.25, method='average',
                       row_cluster=True, col_cluster=True, linewidths=0, figsize=(7.5, 7.5),
                       col_colors=full_colors, row_colors=full_colors, xticklabels=False, yticklabels=False)

    for label in protocols.unique():
        g.ax_row_dendrogram.bar(0, 0, color=protocol_lut[label], label=label, linewidth=0)
    g.ax_row_dendrogram.legend(title='Protocol', loc="center", ncol=4,
                               bbox_to_anchor=(0.55, 0.9), bbox_transform=gcf().transFigure)
    g.ax_heatmap.set_xlabel("")
    g.ax_heatmap.set_ylabel("")

    out_url = fig_to_uri(g.fig)
    return out_url


server = app.server

if __name__ == '__main__':
    app.run_server(debug=True)
