import dash_html_components as html
import dash_bootstrap_components as dbc


def get_igv_input_forms():
    return dbc.Row([
        html.Div(id='dummy-input', style={'display': 'none'}),
        html.Div(id='dummy-output', style={'display': 'none'}),
        html.Div(id='dummy-output-1', style={'display': 'none'}),
        html.Div(id='dummy-output-2', style={'display': 'none'}),
        dbc.Col([
            html.Div([
            ], id='igv-input'),
            html.Div(id='igv-div')
        ], md=12)
    ])
