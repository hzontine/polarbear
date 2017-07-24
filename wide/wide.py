
import igraph
import random
import os
import logging

logging.getLogger().setLevel('INFO')

# Configuration parameters.
N = 50
NUM_ITER = 100
MIN_FRIENDS_PER_NEIGHBOR = 3
NUM_IDEOLOGIES = 3


def generate_friends_graph(associates_graph, env_openness=.5, tolerance=.3):
    '''
    Given a graph of associations (i.e., whose edges represent the random
    starting material that people get as far as who they encounter), produce a
    graph of friendships, where each of a node's friendships are either chosen
    from their associates, or from the graph at large, depending on the
    env_openness parameter passed.
    The tolerance parameter is used to control how likely a vertex is to form
    friendships with others of its own color. If 0, it will strongly prefer
    this. If 0, it will strongly prefer not to. If .5, it is indifferent to
    other vertex's colors.
    '''
    friends_graph = associates_graph.copy()
    friends_graph.delete_edges(None)
    for vid in range(associates_graph.vcount()):
        a_ids = [ n.index for n in associates_graph.vs[vid].neighbors() ]
        f_ids = [ n.index for n in friends_graph.vs[vid].neighbors() ]
        logging.debug('Vertex {} (associates {})...'.format(vid, a_ids))
        # Hack. Add friends to this vertex until they have at least 
        # MIN_FRIENDS_PER_NEIGHBOR. (They may start out with more because
        # lower-id vertices may well have already made friends with them
        # before we got to 'their' iteration.)
        num_additional_edges = max(0, MIN_FRIENDS_PER_NEIGHBOR - len(f_ids))
        logging.debug(('{} already has {} edges, and should have 3.' + 
            ' Adding {} more.').format(vid, len(f_ids), num_additional_edges))
        for edge_num in range(num_additional_edges):
            # Need to recalculate f_ids in this inner loop since we're adding
            # edges for this vertex, drr.
            f_ids = [ n.index for n in friends_graph.vs[vid].neighbors() ]
            if random.random() < env_openness or set(a_ids) == set(f_ids):
                logging.debug('Choosing from pool at large...')
                # If we randomly decided to choose from the pool at large (or
                #   if we've already added all associates as friends)...
                candidate_ids = (set(range(friends_graph.vcount())) -
                    { vid } - set(f_ids))
            else:
                logging.debug('Choosing from associates...')
                candidate_ids = set(a_ids) - { vid } - set(f_ids)
            new_friend_id = choose_friend(associates_graph, vid, candidate_ids,
                tolerance)
            logging.debug('Adding edge {}.'.format((vid, new_friend_id)))
            if new_friend_id:
                friends_graph.add_edge(vid, new_friend_id)
            
    friends_graph['name'] = 'friends_graph'
    logging.info('{} friends from {} associates ({:.1f}% increase)'.format(
        len(friends_graph.es), len(associates_graph.es),
        len(friends_graph.es) / len(associates_graph.es) * 100))
    still_friends = len(set([e.tuple for e in associates_graph.es]) -
        set([e.tuple for e in friends_graph.es]))
    logging.info('{}/{} ({:.1f}%) of associates are still friends.'.format(
        still_friends, len(associates_graph.es), 
        still_friends / len(associates_graph.es) * 100)) 
    def compute_assortativity(g):
        return g.assortativity_nominal(
            [ colors.index(c) for c in g.vs['color']], directed=False)
    logging.info('Assortativity: associates {:.3f}, friends {:.3f}'.format(
        compute_assortativity(associates_graph),
        compute_assortativity(friends_graph)))

    PLOT_AND_QUIT = True
    if PLOT_AND_QUIT:
        layout = associates_graph.layout_kamada_kawai(seed=None)
        associates_graph.vs['label'] = list(range(associates_graph.vcount()))
        friends_graph.vs['label'] = list(range(friends_graph.vcount()))
        igraph.plot(associates_graph, layout=layout, inline=False,
            target="as.png")
        igraph.plot(friends_graph, layout=layout, inline=False,
            target="fr.png")
        import sys ; sys.exit(1)

    return friends_graph


def choose_friend(graph, vid, candidate_f_ids, tolerance):
    '''
    Choose a friend for the vid passed randomly from the candidate ids
    passed. The "tolerance" parameter controls how likely it is that the
    chosen friend will have the same color as vid. If 1, it always will (if
    possible). If 0, it always won't (if possible). If 0.5, it is agnostic
    with respect to color.
    '''
    # https://stackoverflow.com/questions/3679694
    def weighted_choice(choices):
       total = sum(w for c, w in choices.items())
       r = random.uniform(0, total)
       upto = 0
       for c, w in choices.items():
          if upto + w >= r:
             return c
          upto += w
       assert False, "Shouldn't get here"

    if (len(candidate_f_ids)) == 0:
        return None
    my_color = graph.vs[vid]['color']
    weighted_ids = { v : 
            (1-tolerance if graph.vs[v]['color'] == my_color else tolerance)
            for v in list(candidate_f_ids) }
    logging.debug("It's: {}.".format(weighted_ids))
    logging.debug('({}): Choosing from {}...'.format(vid, candidate_f_ids))
    return weighted_choice(weighted_ids)


POSSIBLE_COLORS = ['red','lightblue','green','brown','purple']
colors = POSSIBLE_COLORS[:NUM_IDEOLOGIES]

associates_graph = igraph.Graph.Erdos_Renyi(N, MIN_FRIENDS_PER_NEIGHBOR/N)
associates_graph['name'] = 'associates graph'
associates_graph.vs['color'] = [ random.choice(colors) for _ in range(N) ]
graph = generate_friends_graph(associates_graph)


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
