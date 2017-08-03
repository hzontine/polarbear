
import igraph
import logging
import random
import os
import math
#import pandas as pd

import wide


def run_bvm(graph, num_iter, update_graph_rate=0, homophily=.7,
    plot_graphs=False):
    '''
    Run the binary voter model (selection-with-replacement variant) for the
    given number of iterations on the graph shown. Return a dataframe with one
    row per simulation iteration.
    The update_graph_rate parameter is used to control how often during
    the process a vertex will attach/detach from its neighbors based on its
    homophily. If 1, each iteration of the BVM (which involves one vertex
    updating its opinion) will also involve that vertex either attaching, or
    detaching (with equal probability) a neighbor. If .5, it will happen every
    other iteration. Etc.
    If plot_graphs is True, display an animation of the evolving graph itself.
    '''
    #iters = pd.Series(range(num_iter), name='iteration')
    #columns = ['assortativity']
    #results = pd.DataFrame(index=iters, columns=columns)
    friendships_update_ctr = 0
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
        if (math.floor(friendships_update_ctr) < 
            math.floor(friendships_update_ctr + update_graph_rate)):
            # Time to do a friendships update.
                update_friendships_for_vertex(graph, vertex, homophily)
        # Update our counter (always).
        friendships_update_ctr += update_graph_rate
        results[i] = wide.compute_assortativity(graph)
        if len(set(graph.vs['color'])) == 1:
            logging.warning('Converged (at iteration {})!'.format(i))
            break
        if plot_graphs:
            layout = graph.layout_kamada_kawai(seed=layout)
            igraph.plot(graph, inline=False, layout=layout,
                target='plot{:04d}.png'.format(i))
    if plot_graphs:
        os.system('eog plot*.png')
        os.system('rm plot*.png')
    return results


def update_friendships_for_vertex(graph, vertex, homophily):
    '''
    With equal probability, attach the given vertex to a new friend, or detach
    it from an existing friend, with preferences based on the homophily value
    passed. If not possible, do nothing.
    '''
    f_ids = [ n.index for n in vertex.neighbors() ]
    if random.random() < .5:
        # Add a friend to this vertex.
        logging.info('Adding a friend to vertex {}...'.format(vertex.index))
        candidate_ids = (set(range(graph.vcount())) - { vertex.index } - 
                set(f_ids))
        new_friend_id = wide.choose_friend(graph, vertex.index, candidate_ids, 
                homophily)
        if new_friend_id:
            logging.info('  (Adding {}...)'.format(new_friend_id))
            graph.add_edge(vertex.index, new_friend_id)
    else:
        # Remove a friend from this vertex.
        logging.info('Removing a friend from vertex {}...'.format(vertex.index))
        old_friend_id = wide.choose_friend(graph, vertex.index, f_ids, 
            1-homophily)
        if old_friend_id:
            logging.info('  (Removing {}...)'.format(old_friend_id))
            graph.delete_edges([(vertex.index, old_friend_id)])
