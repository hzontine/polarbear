
# Code to reproduce standard opinion dynamics simulation results.
# Stephen and Hannah

library(igraph)
source("synthetic.R")

# Run an opinion dynamics simulation with n agents for num.iter iterations.
# Return a list of igraph objects, each giving the graph at a snapshot in
# time. 
# 
# Agents have univariate opinions, which could be binary or weighted.
#
# The probability of one agent successfully influencing another to change
# their opinion is fixed, and not based on a homophilic threshold.
#
# init.opinions -- a vector of n nodes (n is up to the caller) with the
# initial opinions for each of the nodes. If binary=TRUE, these should be
# integer values =0 or =1. If binary=FALSE, they should be reals on [0,1].
#
# num.nodes -- number of vertices in the graph
#
# num.iter -- the number of iterations to run the simulation.
#
# binary -- are opinions categorical with two possible values (TRUE), or
# reals on [0,1] (FALSE)?
#
# is.random -- random interactions between all the nodes in the graph (TRUE)
# or random interactions between a node's neighbors to the Nth degree in
# the graph (FALSE)? 
#
# encounter.func -- a function which takes a graph and a vertex ID. Returns a 
# vector of vertex IDs of which the vector may randomly encounter in
# the current iteration.
#
# prob.connected -- the probability of edges between nodes on the initial graph
#
# prob.convert -- when an encounter between heterogeneous agents occurs, the
# probability that one agent (chosen at random) changes their opinion to
# match the other.
#

# SD: note that when we actually call sim.opinion.dynamics() to run a
# simulation, we will be giving it an encounter.func of our choice. That
# encounter.func must be a function that takes two arguments (a graph, and the
# vertex for which you want to get encountered vertices) and returns a vector
# of vertices that will be encountered (and which may, as a result, possibly
# influence or be influenced by that vertex).


sim.opinion.dynamics <- function(num.nodes=50, 
        num.iter=20,
        binary=TRUE, 
        encounter.func=get.mean.field.encounter.func(3),
        N=1,
        is.random=TRUE,
        prob.connected=0.05,
        prob.convert=0.5) {

    if (binary){
        init.opinions=sample(c(0,1),num.nodes,replace=TRUE)
    }

    if (binary  &&  any(!init.opinions %in% c(0,1))) {
        stop("init.opinions not all binary!")
    }
    if (!binary  &&  any(init.opinions < 0 | init.opinions > 1)) {
        stop("init.opinions not all in range!")
    }

    graphs <- list(length=num.iter)
    if(is.random){
        graphs[[1]] <- make_empty_graph(length(init.opinions), directed=FALSE)
        V(graphs[[1]])$opinion <- init.opinions
    } else {
        graphs[[1]] <- erdos.renyi.game(length(init.opinions), prob.connected)
        V(graphs[[1]])$opinion <- init.opinions
    }
    # For each iteration of the sim...
    for (i in 2:num.iter) {

        cat("Iteration",i,"of",num.iter,"...\n")

        # Create a new igraph object to represent this point in time.
        graphs[[i]] <- graphs[[i-1]]

        # Go through all the vertices, in random order:
        for (v in sample(1:gorder(graphs[[i]]))) {
            encountered.vertices <- encounter.func(graphs[[i]],v)
        
            # For each of these encountered partners...
            for (ev in encountered.vertices) {

                if (binary) {
                    if (V(graphs[[i]])[v]$opinion != V(graphs[[i]])[ev]$opinion) {
                        num.incoming.edges <- neighbors(g, ev, mode="in")
                        #if () {
                            V(graphs[[i]])[ev]$opinion <- V(graphs[[i]])[v]$opinion
                        #}
                    } 
                } else {
                    stop("non-binary not supported yet.")
                }
             }
         }
     }
    graphs
}



# Terminology:
#
# ** encounter functions: an "encounter function" is one that takes a graph
# and a vertex, and returns a set of vertices that vertex will encounter in
# one generation.
#
# ** generator functions: a "generator function" is one that can be called to
# return an encounter function.


# Return a "mean-field" encounter function; i.e., vertices are chosen
# uniformly from the entire population (unrelated to anything about
# connections in the graph.)


# SD: second level TODO:
# deal with directed graphs. (1) In get.graph.neighbors.encounter.func(), if
# the graph is directed, only use outgoing edges. (2) In sim.opinion.dynamics,
# get rid of the whole part of the code where Stephen had a bug, and only
# outward influence. (in other words, the node-in-question is always the
# influencer, never the influencee.)

get.mean.field.encounter.func <- function(num.vertices) {
    return(
        function(graph, vertex) {
            # Each vertex encounters some others at random (mean field).
            return (
                sample((1:gorder(graph))[-vertex],num.vertices)
            )
        }
    )
}
get.graph.neighbors.encounter.func <- function(num.vertices) {
    return(
        function(graph,vertex) {
            # Each vertex encounters some others at random from a vector
            # of its "outgoing" neighbors, of whom he may pass 
            # information to/influence.
            total.neighbors <- neighbors(graph, V(graph)[vertex], mode="out")
            if(length(total.neighbors) <= length(num.vertices)){
                return (
                    total.neighbors
                )
            } else {
                return ( 
                    sample(total.neighbors, num.vertices)
                )
            }
        }
    )
}


main <- function() {
    graphs <<- sim.opinion.dynamics(encounter.func=get.mean.field.encounter.func(5))
    plot.animation(graphs,"opinion",delay.between.frames=.2)
    plot.binary.opinions(graphs)
}
