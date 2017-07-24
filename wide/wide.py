
import igraph
import random
import os
import logging

logging.getLogger().setLevel('INFO')

N = 50
NUM_ITER = 1000

graph = igraph.Graph.Erdos_Renyi(N, .1)
graph.vs['color'] = [random.choice(['red','green','blue']) for _ in range(N)]

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
