
import igraph
import random
import sys

from IPython.core.debugger import Tracer

# Hate these global variables, but for the moment...
expressed_encounter_num = 0
hidden_encounter_num = 0


def sim_opinion_dynamics(
    init_graph,
    num_encounters=200,
    encounter_func="graph_neighbors_encounter_function",
    victim_update_function="automatically_update_victim_function",
    choose_randomly_each_encounter=False,
    generate_graph_per_encounter=False,
    termination_function="unanimity_termination_function",
    terminate_after_max_num_encounters=True,
    verbose=False):

    print("Starting.....")

    if type(encounter_func) is not list:
        # If it's not already a list, make it one, so hereafter we can assume
        # it's always a list of one or more encounter functions.
        encounter_func = [encounter_func]
        victim_update_function = [victim_update_function]

    init_graph["message"] = ""
    graphs = [init_graph]

    graphs[0]["num_encounters"] = 0

    graphs.append(graphs[0].copy())
    graph_num = 1

    encounter_num = 0
    num_effectual_encounters = [0,0]

    # For each iteration of the sim...
    while (((not terminate_after_max_num_encounters) or
            encounter_num < num_encounters) and not 
            T[termination_function](graphs[graph_num])):

        if verbose:
            print("Encounter #",encounter_num,"...\n",sep="")

        # Go through all the vertices, in random order:
        vertices = list(range(0,graphs[graph_num].vcount()))
        random.shuffle(vertices)
        for v in vertices:

            if T[termination_function](graphs[graph_num]):
                break

            # If Holley variant, then choose a random vertex instead of the
            # one from our shuffling, above.
            if choose_randomly_each_encounter:
                v = random.randint(0, graphs[graph_num].vcount()-1)

            for i in range(0,len(encounter_func)):
                list_of_vertex_IDs = E[encounter_func[i]](
                                                        graphs[graph_num],v)
                # (Python passes by reference, so we can do this part easier.)
                for id in list_of_vertex_IDs:
                    did_update = VU[victim_update_function[i]](
                                                        graphs[graph_num],v,id)
                    if did_update:
                        num_effectual_encounters[i] += 1
                    encounter_num += 1

            if generate_graph_per_encounter:
                # Create a new igraph object to represent this point in time. 
                graphs.append(graphs[graph_num].copy())
                graph_num += 1
                graphs[graph_num]["message"] = ""

                # Annotate the graph object with a graph attribute indicating
                # the number of encounters that had taken place at the time
                # this snapshot was taken.
                graphs[graph_num]["num_encounters"] = encounter_num

                # (This is hardcoded to assume exactly two encounter/vupdate
                # functions, the first of which updates hidden and the second
                # of which updates expressed.)
                graphs[graph_num]["num_effectual_hidden_encounters"] = \
                    num_effectual_encounters[0]
                graphs[graph_num]["num_effectual_expressed_encounters"] = \
                    num_effectual_encounters[1]

        if (not T[termination_function](graphs[graph_num]) and
                not generate_graph_per_encounter):

            # Create a new igraph object to represent this point in time. 
            graphs.append(graphs[graph_num].copy())
            graph_num += 1
            graphs[graph_num]["message"] = ""

            # Annotate the graph object with a graph attribute indicating
            # the number of encounters that had taken place at the time
            # this snapshot was taken.
            graphs[graph_num]["num_encounters"] = encounter_num

    print("Done!")
    return graphs[0:graph_num]




# Returns a graph whose nodes have a binary or continuous opinion attribute. 
# Default is continuous
def get_plain_old_graph(
    opinion=[random.uniform(0,1) for _ in range(30)],
    probability_connected=0.2,
    dir=False):

    g = igraph.Graph.Erdos_Renyi(
        n=len(opinion),p=probability_connected,directed=dir)
    g.vs['opinion'] = opinion
    return(g)



# Terminology:
#
# ** termination functions: a "termination function" is one that takes a graph
# and returns True if that graph is considered a "terminal state" of a
# simulation -- i.e., if the simulation should stop when this graph is
# reached.
#
# ** termination generator functions: a "termination generator function"
# is one that can be called to return a termination function.

# Never terminate.
def never_terminate_function(graph):
    return False


# Terminate if the graph has total uniformity of opinions.
def unanimity_termination_function(graph, attr1="opinion", attr2=None):
    if attr2 is None:
        return len(set(graph.vs[attr1])) == 1
    else:
        return (len(set(graph.vs[attr1])) == 1  and
                len(set(graph.vs[attr2])) == 1)


# Terminology:
#
# ** victim update functions: a "victim update function" is one that takes a
# graph, an influencing vertex, and a victim vertex, and returns a list with
# two elements: (1) "new.value" is the new value for the victim's opinion, and
# (2) "victim.vertex" is the vertex ID of the victim (i.e., the node that is
# influenced). (Note that victim.vertex could be NULL, in which case the
# caller
# can safely change the "no vertex"'s opinion.)
#
# ** victim update generator functions: a "victim update generator function"
# is one that can be called to return a victim update function.


# "Always" (meaning, with the given probability) update the victim with the
# influencer's opinion.

def automatically_update_victim_function(
    graph, 
    vertex_A, 
    vertex_B,
    A_is_victim=False,
    prob_update=0.4,
    opinion_type="opinion"):

    global hidden_encounter_num

    if A_is_victim:
        vertex = vertex_B
        victim_vertex = vertex_A
    else:
        vertex = vertex_A
        victim_vertex = vertex_B

    if ("stubbornness" not in graph.vs.attribute_names()  or
        graph[victim.vertex]["stubbornness"] == 0):

        if (graph.vs[vertex][opinion_type] == 
            graph.vs[victim_vertex][opinion_type]):

            graph["message"] += ("online: " + str(vertex) + " talks, but " +
                str(victim_vertex) + " is already " +
                color_for(graph.vs[vertex][opinion_type]) + "\n")

        elif random.random() < prob_update:
            hidden_encounter_num += 1
            graph.vs[victim_vertex][opinion_type] = \
                                            graph.vs[vertex][opinion_type]
            graph["message"] += ("online: " + str(vertex) + " persuades " +
                str(victim_vertex) + " to go " +
                color_for(graph.vs[vertex][opinion_type]) + "\n")
        else:
            graph["message"] += ("online: " + str(vertex) + 
                " unable to persuade " + str(victim_vertex) + " to go " +
                color_for(graph.vs[vertex][opinion_type]) + "\n")
    else:
        sys.exit("*** stubborn ***")
        # Nothing will get updated. We're too dang stubborn.



# Terminology:
#
# ** encounter functions: an "encounter function" is one that takes a graph
# and a vertex, and returns a set of vertices that vertex will encounter in
# one generation.
#
# ** encounter generator functions: an "encounter generator function" is one
# that can be called to return an encounter function.

# Each vertex encounters some others at random from a vector of its "outgoing"
# neighbors, of whom he may pass information to/influence.
def graph_neighbors_encounter_function(
    graph, 
    vertex, 
    num_vertices=1, 
    all_neighbors=False):

    if num_vertices==0:
        all = True
    outgoing_neighbors = graph.neighbors(vertex, mode="in")
    if len(outgoing_neighbors) <= num_vertices or all_neighbors:
        return outgoing_neighbors
    else:
        return random.sample(outgoing_neighbors, num_vertices)


def color_for(n):
    return "blue" if n==0 else "red"


# Dictionaries to look up plug-and-play functions based on string IDs, since I
# can't figure out how to get Python lambdas to play nicely here like R does.
T = {"never_terminate_function" : never_terminate_function,
     "unanimity_termination_function" : unanimity_termination_function}
E = {"graph_neighbors_encounter_function" : graph_neighbors_encounter_function}
VU = {"automatically_update_victim_function" :
    automatically_update_victim_function}
