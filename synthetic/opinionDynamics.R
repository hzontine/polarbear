
# Code to reproduce standard opinion dynamics simulation results.
# Stephen and Hannah

library(igraph)
source("synthetic.R")
source("plotting.R")

# Run an opinion dynamics simulation with n agents for num.iter iterations.
# Return a list of igraph objects, each giving the graph at a snapshot in
# time. 
# 
# Agents have an opinion, which could be binary or weighted, and a stubbornness
# attribute, which can be binary or weighted (coming soon).
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
        victim.update.function=get.bounded.confidence.update.victim.function(threshold.val=1.),
        binaryVoterModel=FALSE) {

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
                if(binaryVoterModel==FALSE){
                    V(graphs[[i]])[ev]$opinion <- victim.update.function(graphs[[i]], v, ev)
                }else{
                    V(graphs[[i]])[v]$opinion <- V(graphs[[i]])[ev]$opinion
                }
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
# Note: this function does not factor in a node's stubbornness

# Stubbornness must be binary
get.automatically.update.victim.function <- function(){
    return (
        function(graph, vertex, victim.vertex){
            if(V(graph)[victim.vertex]$stubbornness == 0){
                return(V(graph)[vertex]$opinion)
            } else {
                return(V(graph)[victim.vertex]$opinion)
            }
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
            scaling.factor <- 1 / neighbors(graph, vertex, mode="in")
            probability.of.converting <- scaling.factor * (1- V(graph)[victim.vertex]$stubbornness)
            if (rbinom(1, 1, probability.of.converting) == 1){
                    return( V(graph)[vertex]$opinion )
            } else {
                    # Victim vertex opinion value stays the same
                    return( V(graph)[victim.vertex]$opinion )
            }
        }
    )
}

# If V(g)$opinions are continuous:
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

get.bounded.confidence.update.victim.function <- function(threshold.value=0.3,
    migration.factor=0.5){
    return (
        function(graph, vertex, victim.vertex){
            if(abs(V(graph)[vertex]$opinion - V(graph)[victim.vertex]$opinion) <  threshold.value){ 
                probability.of.converting <- (1 - V(graph)[victim.vertex]$stubbornness)
                if (rbinom(1, 1, probability.of.converting) == 1){  
                    diff.of.opinion <- V(graph)[vertex]$opinion - V(graph)[victim.vertex]$opinion
                    return( diff.of.opinion * migration.factor + V(graph)[victim.vertex]$opinion)
                } else {
                    return( V(graph)[victim.vertex]$opinion )
                }
            } else {
                return( V(graph)[victim.vertex]$opinion )
            }
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


# Return a graph that has a fairly strongly connected group of liberals, a
# fairly strongly connected group of conservatives, and num.connections
# between the two groups.
get.barely.connected.polarized.graph <- function(num.connections=2) {

    liberal.peeps <- erdos.renyi.game(20,.16)
    V(liberal.peeps)$name <- letters[1:20]
    V(liberal.peeps)$opinion <- runif(vcount(liberal.peeps),0,.5)

    conservative.peeps <- erdos.renyi.game(20,.16)
    V(conservative.peeps)$name <- LETTERS[1:20]
    V(conservative.peeps)$opinion <- runif(vcount(conservative.peeps),.5,1)

    barely <- union(liberal.peeps,conservative.peeps)
    V(barely)$opinion <- ifelse(is.na(V(barely)$opinion_1),
        V(barely)$opinion_2, V(barely)$opinion_1)
    for (i in 1:num.connections) {
        barely <- barely + edge(letters[i],LETTERS[i])
    }
    return(barely)
}

# Returns a graph whose nodes have one ideology from the set, where the size of the
# set is equal to num.ideologies. 
# If stubborn=TRUE, nodes will have binary stubborn attribute values.
# Default is binary: only two ideologies in the set.
get.discrete.graph <- function(num.ideologies=2, stubborn=TRUE) {
    if(stubborn){
        g <- get.stubborn.graph(opinion=floor(runif(30, min=0, max=num.ideologies)))
    } else {
        g <- get.plain.old.graph(opinion=floor(runif(30, min=0, max=num.ideologies)))
    }
    return(g)
}


# Returns a graph whose nodes have a binary or continuous opinion and stubbornness attribute.
# opinion -- vector of opinion values. Default is continuous.
# stubborn.peeps -- a vector of binary stubbornness values (continuous not supported yet)
get.stubborn.graph <- function(opinions=runif(30), stubbornnesses=rbinom(30, 1, 0.5), 
    probability.connected=0.2, dir=FALSE){
    if(dir){
        g <- erdos.renyi.game(length(stubbornnesses), probability.connected, 
        type="gnp", directed=TRUE)
    } else {
        g <- erdos.renyi.game(length(stubbornnesses), probability.connected)
    }
    V(g)$opinion <- opinions
    V(g)$stubbornness <- stubbornnesses
    return(g)
}

# Returns a graph whose nodes have a binary or continuous opinion attribute. 
# Default is continuous
get.plain.old.graph <- function(opinion=runif(30), probability.connected=0.2) {
    g <- erdos.renyi.game(length(opinion),probability.connected)
    V(g)$opinion <- opinion
    return(g)
}

param.sweep <- function(init.graph) {

    library(doParallel)
    registerDoParallel(8)

    encs.per.iter <- 4

    invisible(foreach (migration.factor=seq(0.2,1,.2)) %dopar% {
      for (bc.thresh in seq(0.2,1,.2)) {
        graphs <- sim.opinion.dynamics(init.graph,num.iter=50,
            encounter.func=get.graph.neighbors.encounter.func(encs.per.iter),
            victim.update.function=
                get.bounded.confidence.update.victim.function(bc.thresh,
                    migration.factor=migration.factor))
        plot.animation(graphs,"opinion",delay.between.frames=.25,
            interactive=FALSE,
            subtitle=paste0("encounter: ",encs.per.iter,
                                            " graph neighbor per iteration\n",
                "update: bounded confidence threshold ",bc.thresh,",\n",
                "migration factor ",migration.factor),
            animation.filename=paste0("barelyBC",bc.thresh,"MigFac",
                migration.factor,".gif"),
            overwrite.animation.file=TRUE
        )
      }
    })
}

main <- function() {
    set.seed(11111)
    #param.sweep(get.barely.connected.polarized.graph())

    # Discrete Opinions
     graphs <- sim.opinion.dynamics(get.discrete.graph(4), num.iter=20, 
        encounter.func=get.graph.neighbors.encounter.func(4),
        victim.update.function=get.proportional.to.in.degree.update.victim.function())

    # Continuous Opinions
    # graphs <- sim.opinion.dynamics(get.stubborn.graph(), num.iter=20,
    #   encounter.func=get.graph.neighbors.encounter.func(4),
    #   victim.update.function=get.bounded.confidence.update.victim.function(0.5, 0.2))

    plot.animation(graphs, "opinion", delay.between.frames=.25)
}
