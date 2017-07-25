#!/usr/bin/env python3

import igraph
import random
import os
import sys
import logging

from wide import *


if len(sys.argv) > 5:
    print('Usage: main_wide.py [env_openness] [tolerance] [sweep=#] [seed] ' +
        '[logLevel|NONE].')
    sys.exit(1)

if len(sys.argv) > 5:
    if sys.argv[5] == 'NONE':
        logging.getLogger().setLevel(logging.CRITICAL + 1)
    else:
        logging.getLogger().setLevel(sys.argv[5])
else:
    logging.getLogger().setLevel('INFO')

if len(sys.argv) > 4:
    try:
        seed = int(sys.argv[4])
        if seed == 0:
            seed = random.randrange(10000)
    except:
        print_usage()
        print("('seed' must be numeric.)")
        sys.exit(4)
else:
    seed = random.randrange(10000)

if len(sys.argv) > 3:
    do_sweep = sys.argv[3].startswith('sweep=')
    if do_sweep:
        try:
            num_runs = int(sys.argv[3][sys.argv[3].index('=')+1:])
        except:
            print_usage()
            print("('sweep=#' parameter must be numeric.)")
            sys.exit(5)
else:
    do_sweep = False

params = [('TOLERANCE',.5),('ENV_OPENNESS',.5)]
this_module = sys.modules[__name__]
for i,(param,default) in enumerate(params):
    if len(sys.argv) > i+1:
        try:
            setattr(this_module, param, float(sys.argv[i+1]))
            if not 0 <= float(sys.argv[i+1]) <= 1:
                raise exception
        except:
            print("{} '{}' must be float in (0,1).".format(param.title(),
                                                            sys.argv[i+1]))
            sys.exit(10+i)
    else:
        setattr(this_module, param, default)


# Other configuration parameters.
N = 50
MIN_FRIENDS_PER_NEIGHBOR = 3
NUM_IDEOLOGIES = 3
NUM_ITER = 100

print('=== Using seed {}.'.format(seed))
print("=== TOLERANCE={}, ENV_OPENNESS={}, N={}, MIN_FRIENDS={}.".format(
    TOLERANCE, ENV_OPENNESS, N, MIN_FRIENDS_PER_NEIGHBOR))
print("=== NUM_ITER={}, NUM_IDEOLOGIES={}.".format(NUM_ITER, NUM_IDEOLOGIES))
random.seed(seed)


POSSIBLE_COLORS = ['red','lightblue','green','brown','purple']
colors = POSSIBLE_COLORS[:NUM_IDEOLOGIES]

associates_graph = igraph.Graph.Erdos_Renyi(N, MIN_FRIENDS_PER_NEIGHBOR/N)
associates_graph['name'] = 'associates graph'
associates_graph.vs['color'] = [ random.choice(colors) for _ in range(N) ]
graph = generate_friends_graph(associates_graph, ENV_OPENNESS, TOLERANCE,
    MIN_FRIENDS_PER_NEIGHBOR)


# Static layout: compute once and use, since graph structure doesn't change.
layout = graph.layout_kamada_kawai(seed=None)
for i in range(NUM_ITER):
    logging.warning('Iteration {}...'.format(i))
    vertex = random.choice(graph.vs)
    if vertex.neighbors():
        neighbor = random.choice(vertex.neighbors())
        vertex['color'] = neighbor['color']
    if len(set(graph.vs['color'])) == 1:
        logging.warning('Converged (at iteration {})!'.format(i))
        break
    igraph.plot(graph, inline=False, layout=layout,
        target='plot{:03d}.png'.format(i))

os.system('eog plot*.png')
os.system('rm plot*.png')
