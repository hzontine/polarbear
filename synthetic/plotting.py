
# Common plotting functions.
# Stephen and Hannah

import pandas as pd
from collections import Counter
import matplotlib.pyplot as plt

from IPython.core.debugger import Tracer


# Given a list of igraph objects representing an evolving graph, plot the 
# fraction of vertices with opinion 1 over time.
def plot_binary_opinions(graphs, attr1="opinion", attr2=None):

    if attr2 is not None:
        sys.exit("Can't do this yet.")
    else:
        frac_0s = []
        frac_1s = []
        time_pts = []
        for graph in graphs:
            counts = Counter(graph.vs[attr1])
            frac_0s.append(counts[0]/graph.vcount())
            frac_1s.append(counts[1]/graph.vcount())
            time_pts.append(graph["num_encounters"])
        df = pd.DataFrame({'0':frac_0s,'1':frac_1s,'encounters':time_pts})
        _, ax = plt.subplots()
        ax = df.plot(ax=ax, kind='line', x='encounters', y='0', c='blue')
        ax = df.plot(ax=ax, kind='line', x='encounters', y='1', c='red')
        lines, _ = ax.get_legend_handles_labels()
        ax.legend(lines, ["blue","red"], loc='best')
        plt.show()

