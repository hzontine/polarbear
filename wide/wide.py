
import igraph
import random
import os
import logging

logging.getLogger().setLevel('INFO')

# Configuration parameters.
N = 50
NUM_ITER = 100
NUM_IDEOLOGIES = 3


def generate_friends_graph(associates_graph, openness=.5):
    '''
    Given a graph of associations (i.e., whose edges represent the random
    starting material that people get as far as who they encounter), produce a
    graph of friendships, where each of a node's friendships are either chosen
    from their associates, or from the graph at large, depending on the
    openness parameter passed.
    '''
    return associates_graph


POSSIBLE_COLORS = ['red','blue','green','brown','purple']
colors = POSSIBLE_COLORS[:NUM_IDEOLOGIES]

associates_graph = igraph.Graph.Erdos_Renyi(N, .1)
associates_graph.vs['color'] = [ random.choice(colors) for _ in range(N) ]
graph = generate_friends_graph(associates_graph)


# Static layout: compute once and use, since graph structure doesn't change.
layout = graph.layout_kamada_kawai(seed=None)
for i in range(NUM_ITER):
    logging.info('Iteration {}...'.format(i))
    vertex = random.choice(graph.vs)
    if vertex.neighbors():
        neighbor = random.choice(vertex.neighbors())
        vertex['color'] = neighbor['color']
    if len(set(graph.vs['color'])) == 1:
        logging.info('Converged (at iteration {})!'.format(i))
        break
    igraph.plot(graph, inline=False, layout=layout,
        target='plot{:03d}.png'.format(i))

os.system('eog plot*.png')
os.system('rm plot*.png')
