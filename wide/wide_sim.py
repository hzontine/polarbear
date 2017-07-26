
import igraph
import logging
import random
import os
#import pandas as pd

import wide


def run_bvm(graph, num_iter, plot_graphs=False):
    '''
    Run the binary voter model (selection-with-replacement variant) for the
    given number of iterations on the graph shown. Return a dataframe with one
    row per simulation iteration.
    If plot_graphs is True, display an animation of the evolving graph itself.
    '''
    #iters = pd.Series(range(num_iter), name='iteration')
    #columns = ['assortativity']
    #results = pd.DataFrame(index=iters, columns=columns)
    results = [None] * num_iter
    if plot_graphs:
        # Static layout: compute once and use, since graph structure doesn't
        # change.
        graph.vs['label'] = list(range(graph.vcount()))
        layout = graph.layout_kamada_kawai(seed=None)
    for i in range(num_iter):
        logging.warning('Iteration {}...'.format(i))
        vertex = random.choice(graph.vs)
        if vertex.neighbors():
            neighbor = random.choice(vertex.neighbors())
            vertex['color'] = neighbor['color']
        results[i] = wide.compute_assortativity(graph)
        if len(set(graph.vs['color'])) == 1:
            logging.warning('Converged (at iteration {})!'.format(i))
            break
        if plot_graphs:
            igraph.plot(graph, inline=False, layout=layout,
                target='plot{:03d}.png'.format(i))
    if plot_graphs:
        os.system('eog plot*.png')
        os.system('rm plot*.png')
    return results
