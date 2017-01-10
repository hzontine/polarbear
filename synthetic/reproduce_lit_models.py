# Reproduce results of published papers

import opinion_dynamics as od
import igraph
import random

from IPython.core.debugger import Tracer

# Holley and Liggett 1975        Clifford and Sudbury 1973
# Vertex always changes her opinion to reflect that of the victim's opinion
# Only one encounter per iteration
# Results: Opinions will always converge to a consensus
#
def binary_voter(plot=True, num=10, prob=0.3, num_enc=2000):

    """Reproduce Holley/Liggett 1975."""

    if num % 2 == 0:
        values = [0]*(num//2) + [1]*(num//2)
    else:
        print("(Can't split", num, "nodes evenly.)")
        values = [0]*(num//2) + [1]*(num//2+1)

    random.shuffle(values)

    init = od.get_plain_old_graph(values, prob)
    while not init.is_connected():
        init = od.get_plain_old_graph(values, prob)

    graphs = od.sim_opinion_dynamics(init_graph=init, num_encounters=num_enc,
        encounter_func="graph_neighbors_encounter_function",
        victim_update_function="automatically_update_victim_function",
        choose_randomly_each_encounter=True)

#   if plot:
#       plot_animation(graphs, "opinion", delay_between_frames=.5)
#       plot_binary_opinions(graphs, attribute1="opinion")

    return graphs


def main():
    random.seed(1234)
    return binary_voter()

graphs = main()
