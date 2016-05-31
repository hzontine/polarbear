
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
# N --  random interactions between a node and all its neighbors to the Nth 
# degree. Default is N = 1.
#
# encounter.func -- a function which takes several parameters including, a 
# graph, vertex ID,  
# Returns a vector of vertex IDs of which the vector may randomly encounter in
# the current iteration.
#
# prob.connected -- the probability of edges between nodes on the initial graph
#
# prob.convert -- when an encounter between heterogeneous agents occurs, the
# probability that one agent (chosen at random) changes their opinion to
# match the other.
#
sim.opinion.dynamics <- function(num.nodes=50, 
	num.iter=20,
        binary=TRUE, 
        encounter.func=get.mean.field.encounter.func(3),
	N=1,
	is.random=TRUE,
	prob.connected=0.05,
        prob.convert=1.0) {

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
	    encountered.vertices <- encounter.func(graphs[[i]],v,is.random,N)
        
	    # For each of these encountered partners...
            for (ev in encountered.vertices) {

                if (binary) {
                    # If they already have the same opinion, nm. If they
                    # don't, roll dice to see whether they influence, and if
                    # so, choose a random one to influence the other.
                    if (V(graphs[[i]])[v]$opinion !=
                        V(graphs[[i]])[ev]$opinion) {
                        if (runif(1) < prob.convert) {
                            change.first <- runif(1) < .5
                                if (change.first) {
                                   V(graphs[[i]])[v]$opinion <-
                                     V(graphs[[i]])[ev]$opinion
                            	 } else {
                                   V(graphs[[i]])[ev]$opinion <-
                                     V(graphs[[i]])[v]$opinion
                                 }
                         }
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



get.mean.field.encounter.func <- function(num.vertices) {
    return(
	function(graph, vertex, is.random, x) {
	    if (is.random){
		# Each vertex encounters some others at random (mean field).
		sample((1:gorder(graph))[-vertex],num.vertices)
    	    }else{
		# Each vertex encounters some others at random from a vector
		# of its neighbors who are up to x degrees away.
		sample(neighbors(graphs, V(graphs)[vertex], mode=x),num.vertices)
	    }
	}
    )
}
		

main <- function() {
    graphs <<- sim.opinion.dynamics()
    plot.animation(graphs,"opinion",delay.between.frames=.2)
    plot.binary.opinions(graphs)
}
