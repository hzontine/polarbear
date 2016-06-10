
# Code to reproduce standard opinion dynamics simulation results.
# Stephen and Hannah

library(igraph)
source("synthetic.R")
source("plotting.R")

# Run an opinion dynamics simulation with n agents for num.iter iterations.
# Return a list of igraph objects, each giving the graph at a snapshot in
# time. 
# 
# Agents have univariate opinions, which could be binary or weighted.
#
# The probability of one agent successfully influencing another to change
# their opinion is fixed, and not based on a homophilic threshold.
#
# init.graph -- the initial condition of the simulation. This can be any
# igraph object with an attribute on each vertex called "opinion". (The value
# of the attribute can be binary or continuous.)
#
# num.iter -- the number of iterations to run the simulation.
#
# encounter.func -- a function which takes a graph and a vertex ID. Returns a 
# vector of vertex IDs of which the vector may randomly encounter in
# the current iteration.
#
# victim.update.function -- a function which takes a graph and two vertex IDs:
# the first is the "potential influencer" (i.e., the node whose opinion may
# cause the second vertex's opinion to be updated) and the second is the
# "potential victim." It will return the (possibly new) value of the second
# vetex.

sim.opinion.dynamics <- function(init.graph,
        num.iter=20,
        encounter.func=get.mean.field.encounter.func(3),
        victim.update.function=get.bounded.confidence.update.victim.function(threshold.val=1.)) {

    graphs <- list(length=num.iter)
    graphs[[1]] <- init.graph

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
                V(graphs[[i]])[ev]$opinion <- victim.update.function(graphs[[i]], v, ev)
            }
        }
    }
    graphs
}


# Terminology:
#
# ** victim update functions: a "victim update function" is one that takes a
# graph, an influencing vertex, and a victim vertex, and returns the new value
# for the victim's opinion.
#
# ** victim update generator functions: a "victim update generator function"
# is one that can be called to return a victim update function.


# (1) This could be a function that returns the "influencer's" value, period.
# The generator function is called: get.automatically.update.victim.function()

get.automatically.update.victim.function <- function(){
    return (
        function(graph, vertex, victim.vertex){
            return(V(graph)[vertex]$opinion)
        }
    )
}

# (2) This could be a function that either returns (a) the "influencer's"
# value, or (b) the victim's current value. The probability of (a) is equal to
# probability 1/nie, where nie is the number of incoming edges, and the
# original value otherwise.
# The generator function is called:
# get.proportional.to.in.degree.update.victim.function()

# TODO: add a scaling factor. In other words, we do want higher in-degree to
# correspond to lower-chance-of-updating, but we don't necessarily want to fix
# it to the exact equation: P(updating) = 1/in-degree.

get.proportional.to.in.degree.update.victim.function <- function(){
    return (
        function(graph, vertex, victim.vertex){
            probability.of.converting <- 1 / (neighbors(graph, vertex, mode="in"))
            if (runif(1, 1, probability.of.converting) == 1){
                    return( V(graph)[vertex]$opinion )
            } else {
                    # Victim vertex opinion value stays the same
                    return( V(graph)[victim.vertex]$opinion )
            }
        }
    )
}

# If binary = FALSE:
# (3) This could be a function that returns a new value iff the distance
# between my current ideology and yours is less than a threshold ("bounded
# confidence"), otherwise the victim's current value.
# The generator function is called:
# get.bounded.confidence.update.victim.function()

# get.bounded.confidence.update.victim.function: note this ONLY makes sense
# for continuous (not binary) opinions.
#
# threshold.value -- the maximum distance between the victim's and
# influencer's opinions that will cause the victim to update their opinion at
# all. (If the difference exceeds the threshold.value, the victim's opinion
# stays exactly the same.)
#
# migration.factor -- a numeric between 0 and 1 that dictates what fraction of
# the distance between the victim's opinion and the influencer's opinion the
# victim will move, *if* the victim does move. For example: if the victim's
# opinion is .4 and the influencer's is .8, and the migration.factor is .5,
# then the victim's opinion will be updated to .6, which is half the distance
# between them. If the migration.factor is 0, the victim's opinion is
# unchanged. If the migration.factor is 1, the victim's opinion is moved all
# the way to .8.

get.bounded.confidence.update.victim.function <- function(threshold.value,
    migration.factor=.5){
    return (
        function(graph, vertex, victim.vertex){
            return (
                if(abs(V(graph)[vertex]$opinion - V(graph)[victim.vertex]$opinion)
                    < threshold.value ){

                    # "Okay, you have a point."
                    diff.of.opinion <- V(graph)[vertex]$opinion -
                        V(graph)[victim.vertex]$opinion
                    diff.of.opinion * migration.factor +
                        V(graph)[victim.vertex]$opinion
                } else {
                    # "Sorry, you violated my confidence bound." I'm staying
                    # put.
                    V(graph)[victim.vertex]$opinion
                }
            )
        }
    )
}

# Terminology:
#
# ** encounter functions: an "encounter function" is one that takes a graph
# and a vertex, and returns a set of vertices that vertex will encounter in
# one generation.
#
# ** encounter generator functions: an "encounter generator function" is one
# that can be called to return an encounter function.

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
            outgoing.neighbors <- neighbors(graph, V(graph)[vertex], mode="out")
            if(length(outgoing.neighbors) <= num.vertices){
                return (
                    outgoing.neighbors
                )
            } else {
                return ( 
                    sample(outgoing.neighbors, num.vertices)
                )
            }
        }
    )
}

main <- function() {
    init.graph <- erdos.renyi.game(50,.1)
    V(init.graph)$opinion <- runif(vcount(init.graph))
    graphs <<- sim.opinion.dynamics(init.graph,num.iter=7,
        encounter.func=get.graph.neighbors.encounter.func(3))
        #encounter.func=get.mean.field.encounter.func(3))
    plot.animation(graphs,"opinion",delay.between.frames=.5)
#    plot.binary.opinions(graphs)
}

param.sweep <- function() {

    library(doParallel)
    registerDoParallel(8)

    set.seed(1234)

    init.graph <- erdos.renyi.game(50,.1)
    V(init.graph)$opinion <- runif(vcount(init.graph))

    encs.per.iter <- 1
    bc.thresh <- 1

    foreach (encs.per.iter=1:8) %dopar% {
      for (bc.thresh in 1) {
        graphs <- sim.opinion.dynamics(init.graph,num.iter=20,
            encounter.func=get.graph.neighbors.encounter.func(encs.per.iter),
            victim.update.function=
                get.bounded.confidence.update.victim.function(bc.thresh))
            #encounter.func=get.mean.field.encounter.func(3))
        plot.animation(graphs,"opinion",delay.between.frames=.5,
            interactive=FALSE,
            subtitle=paste0("encounter: ",encs.per.iter,
                                            " graph neighbor per iteration\n",
                "update: bounded confidence threshold ",bc.thresh),
            animation.filename=paste0("polarGraph",encs.per.iter,"UpdateBC",
                bc.thresh,".gif"),
            overwrite.animation.file=TRUE
        )
      }
    }

#    plot.binary.opinions(graphs)
}
