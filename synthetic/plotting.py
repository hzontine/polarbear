
# Common plotting functions.
# Stephen and Hannah

import igraph
import pandas as pd
from collections import Counter
import matplotlib.pyplot as plt
from pathlib import Path
import tempfile
import time
import math
import os

from IPython.core.debugger import Tracer


def plot_polar_graph(
    graph,
    legend,
    fill,
    vertex_coords,
    vertex_frame_color,
    main_title,
    subtitle,
    filename=None):

    p = igraph.plot(graph,layout=vertex_coords,edge_arrow_size=1.5,
        vertex_shape='circle',vertex_frame_color=vertex_frame_color,
        vertex_frame_width=1,vertex_size=15,main=main_title,
        sub=subtitle)
    if filename is None:
        p.show()
    else:
        p.save(filename)


# Given a list of graphs, plot them, (possibly) ensuring that vertices are
# plotted in the same location from graph to graph.
TRANSCRIPT = 1
SUMMARY_STATS = 2
def plot_animation(
    graphs, 
    attr_name='opinion',
    second_attr=None,
    try_to_keep_vertex_positions=True,
    delay_between_frames=.5,
    interactive=False,
    animation_filename='polar.gif',
    overwrite_animation_file=True,
    subtitle=TRANSCRIPT):

    if not interactive and not overwrite_animation_file:
        if Path(animation_filename).is_file():
            return "File " + animation_filename + " already exists!"

    # Note: we're assuming discrete attributes here, for simplification. (See
    # plotting.R for how to implement continuous attrs.)

    if not interactive:
        base_filename = "plot" + next(tempfile._get_candidate_names())

    if try_to_keep_vertex_positions:
        vertex_coords = graphs[0].layout("kk")
    else:
        vertex_coords = None

    for i in range(1,len(graphs)):
        graph = graphs[i]
        num_distinct_attr_one_vals = len(set(graph.vs[attr_name]))

        if num_distinct_attr_one_vals == 2:
            graph.vs['color'] = [ "blue" if attr is 0 else "red"
                for attr in graph.vs[attr_name] ]
            graph.vs['label_color'] = [ "white" if attr is 0 else "black"
                for attr in graph.vs[attr_name] ]
            fill = ["blue","red"]
            legend = ["Liberal","Conservative"]
        else:
            return ("" + str(num_distinct_attr_one_vals) + " attribute " +
                "values not supported yet!")
            
        two_attr = second_attr is not None

        if not two_attr:
            vertex_frame_color = "black"
        else:
            return "More than one attribute not supported yet." 
            
        if isinstance(subtitle,str):
            subtitle_to_plot = subtitle
        elif subtitle == TRANSCRIPT:
            if i+1 < len(graphs):
                message_for_next_frame = graphs[i+1]['message']
                if message_for_next_frame == '':
                    subtitle_to_plot = "(no changes about to happen)"
                else: 
                    subtitle_to_plot = ("this is about to happen:\n" +
                        message_for_next_frame)
            else:
                subtitle_to_plot = "last one!"
        elif subtitle == SUMMARY_STATS:
            return "Summary stats not supported yet."
        else:
            return "Illegal subtitle value: " + str(subtitle)

        if interactive:
            filename = None
        else:
            number = ''.join(map(str,([0]*(3-math.floor(math.log10(i))+1))))
            filename = base_filename + number + str(i) + ".png"

        plot_polar_graph(graph,legend,fill,vertex_coords,vertex_frame_color,
            main_title="Iteration " + str(i) + " of " + str(len(graphs)),
            subtitle=subtitle_to_plot,filename=filename)
        if interactive:
            time.sleep(delay_between_frames) 
        else:
            print("Generating animation frame",i,"of", len(graphs),"...")
    
    if not interactive:
        print("Assembling animation...")
        os.system("convert -loop 1 -delay " + str(delay_between_frames*100) + 
            " " + base_filename + "*.png " + animation_filename)
        os.system("rm " + base_filename + "*.png")
        print("Animation in file ", animation_filename, ".", sep="")



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


