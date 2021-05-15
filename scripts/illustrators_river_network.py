import argparse
import sys

import numpy as np
import pandas as pd
import plotly
import plotly.graph_objects as go
import plotly.io.orca
import random
#plotly.io.orca.config.executable = 'C:/Users/Roman.LAPTOP-088R18A9/AppData/Local/Programs/orca/orca.exe'


def preprocess_data(in_dir, threshold):
    coeffs = pd.read_csv(in_dir)
    filtered_coeffs = coeffs[coeffs.metric >= threshold]

    filtered_coeffs.cluster_id1 = filtered_coeffs.cluster_id1.astype('str')
    filtered_coeffs.cluster_id2 = filtered_coeffs.cluster_id2.astype('str')

    filtered_coeffs['source'] = filtered_coeffs['volume1'] + "(" + filtered_coeffs['cluster_id1'] + ")"
    filtered_coeffs['target'] = filtered_coeffs['volume2'] + "(" + filtered_coeffs['cluster_id2'] + ")"

    return filtered_coeffs


def generate_names(filtered_coeffs):
    names = list(set(list(filtered_coeffs['source']) + list(filtered_coeffs['target'])))
    return names


def create_indexes(filtered_coeffs, names):
    filtered_coeffs['id_source'] = filtered_coeffs['source'].apply(lambda x: names.index(x))
    filtered_coeffs['id_target'] = filtered_coeffs['target'].apply(lambda x: names.index(x))

    return filtered_coeffs


def generate_nodes_data(names):
    nodes_dataset = pd.DataFrame(pd.Series(names), columns=['name'])
    nodes_dataset['date'] = nodes_dataset['name'].apply(lambda x: x[:9])
    return nodes_dataset


def generate_color():
    random_number = random.randint(0, 16777215)
    hex_number = format(random_number, 'x')
    hex_number = '#' + hex_number
    return hex_number


def visualize(data, nodes_data, title, out_dir):
    dates = ["1932-1939", "1940-1945", "1946-1948", "1949-1950", "1951-1952", "1953-1954", "1955-1957", "1958-1960", "1961-1963",
             "1964-1966", "1967-1969", "1970-1971", "1972-1973", "1974-1975", "1976-1978", "1979-1981", "1982-1984"]

    clusters = list(set(nodes_data['name']))

    ys = np.linspace(0.1, 0.9, len(clusters)).tolist()
    xs = np.linspace(0.1, 0.95, len(dates))

    nodes_data['X'] = nodes_data['date'].apply(lambda x: xs[dates.index(x)])
    nodes_data['Y'] = nodes_data['name'].apply(lambda x: ys[clusters.index(x)])

    i = 0
    length = len(nodes_data['name'])
    random.seed(1234)
    while i <= (length-1):
        nodes_data.at[i, 'color'] = "#{:06x}".format(random.randint(0, 0xFFFFFF))
        i += 1

    fig = go.Figure(go.Sankey(
        arrangement="freeform",
        node={
            "label": nodes_data['name'],
            "x": nodes_data['X'],
            "y": nodes_data['Y'],
            "color": nodes_data['color'],
            'pad': 40},  # 10 Pixels
        link={
            "source": data['id_source'],
            "target": data['id_target'],
            "value": data['metric'] * 100}),

    )

    fig.update_layout(title_text=title,
                      font_size=15, width=2500, height=1000, margin_r=300)
    fig.write_image(format="png", file=out_dir)


def parse_arguments():
    parser = argparse.ArgumentParser(description='Preprocess jaccard coefficients to construct river network',
                                     epilog="""Jaccard coefficients are filtered out to keep only the most similar 
                                     pairs of clusters, then river network is constructed""")
    parser.add_argument('in_dir', nargs='?', help='Input file (csv)',
                        type=argparse.FileType('r'), default=sys.stdin)
    parser.add_argument('-thr', '--threshold',
                        help='value of jaccard coefficient such that lower values are filtered out from data')
    parser.add_argument('-tit', '--title', help='title to be displayed on graph',
                        action='store')
    parser.add_argument('--out_dir', nargs='?', help='Output file (png)',
                        action='store')
    return parser.parse_args()


def main():
    args = parse_arguments()
    filtered_coeffs = preprocess_data(args.in_dir, threshold=float(args.threshold))
    names = generate_names(filtered_coeffs=filtered_coeffs)
    filtered_coeffs = create_indexes(filtered_coeffs=filtered_coeffs, names=names)
    nodes = generate_nodes_data(names)
    filtered_coeffs.to_csv(path_or_buf='filtered_metrics.csv')
    visualize(data=filtered_coeffs, nodes_data=nodes, title=args.title, out_dir=args.out_dir)


if __name__ == '__main__':
    main()
