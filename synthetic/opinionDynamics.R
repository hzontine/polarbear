
# Code to reproduce standard opinion dynamics simulation results.
# Stephen and Hannah

library(igraph)
source("synthetic.R")
source("plotting.R")

# Run an opinion dynamics simulation with n agents for num.encounters
# encounters. Return a list of igraph objects, each giving the graph at a
# snapshot in time immediately following an encounter.
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
# num.encounters -- the number of encounters to run the simulation. (Note that
# if this is not a multiple of the number of nodes, fewer than this number of
# encounters may occur.)
#
# choose.randomly.each.encounter -- if TRUE, each encounter will have a
# completely random "encountering vertex" chosen. If FALSE, all the vertices
# will be chosen as the "encountering vertex" (in random order) before they
# are all chosen again (i.e., no repeats are allowed.)
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
#
# edge.update.function -- a function which takes a graph and a vertex ID. It
# will return information about who the vertex should connect to, and
# disconnect to.
# A list of two elements:
#    $new.edges -- vertex IDs this vertex is currently not connected to, but
#    should be.
#    $old.edges -- vertex IDs this vertex is currently connected to, but
#    shouldn't be.

sim.opinion.dynamics <- function(init.graph,
        num.encounters=200,
        encounter.func=get.mean.field.encounter.func(3),
        victim.update.function=get.bounded.confidence.update.victim.function(threshold.val=1.),
        choose.randomly.each.encounter=FALSE,
        edge.update.function=dave.edge.update.function(),
        verbose=TRUE,
        majority=FALSE,
        edge.update=FALSE) {

    graphs <- vector("list",length=(num.encounters+1))
    graphs[[1]] <- init.graph

    encounter.num <- 0

    # For each iteration of the sim...
    num.iter <- num.encounters+1
    for (i in 2:num.iter) {

        if (verbose) {
            cat("---------------------------------\n")
        }

        # Create a new igraph object to represent this point in time.
        graphs[[i]] <- graphs[[i-1]]
        # Go through all the vertices, in random order:
        for (v in sample(1:gorder(graphs[[i]]))) {

            if (choose.randomly.each.encounter) {
                v <- sample(1:gorder(graphs[[i]]),1)
            }

            encounter.num <- encounter.num + 1
            if (verbose) {
                cat("Encounter",encounter.num,"of",num.encounters*vcount(init.graph)," (",
                    v,")...\n")
            }

            if(edge.update){
                list.of.edges <- edge.update.function(graphs[[i]],v)
                new <- list.of.edges[[1]]
                old <- list.of.edges[[2]]
                for(e in 1:length(new)){
                    # Probability ?
                    graphs[[i]] <- add_edges(graphs[[i]],
                        c(V(graphs[[i]])[v], V(graphs[[i]])[new[e]]))
                }
                for(o in 1:length(new)){
                    # Probability ?
                    delete_edges(graphs[[i]],get.edge.ids(graphs[[i]],c(v,old[o]),directed=TRUE))
                }
            } else {
                encountered.vertices <- encounter.func(graphs[[i]],v)
                if(majority){
                    update.info <- victim.update.function(graphs[[i]], v, encountered.vertices[1])
                    V(graphs[[i]])[update.info$victim.vertex]$opinion <- 
                        update.info$new.value
                } else {
                    # For each of these encountered partners...
                    for (ev in encountered.vertices) {
                        update.info <- victim.update.function(graphs[[i]], v, ev)
                        V(graphs[[i]])[update.info$victim.vertex]$opinion <- 
                            update.info$new.value
                    }
                }
            }
        }
    }
    graphs
}


# Terminology:
#
# ** victim update functions: a "victim update function" is one that takes a
# graph, an influencing vertex, and a victim vertex, and returns a list with
# two elements: (1) "new.value" is the new value for the victim's opinion, and
# (2) "victim.vertex" is the vertex ID of the victim (i.e., the node that is
# influenced). (Note that victim.vertex could be NULL, in which case the caller
# can safely change the "no vertex"'s opinion.)
#
# ** victim update generator functions: a "victim update generator function"
# is one that can be called to return a victim update function.


# (1) This could be a function that returns the "influencer's" value, period.
# The generator function is called: get.automatically.update.victim.function()
# Note: this function does not factor in a node's stubbornness

# Stubbornness must be binary
get.automatically.update.victim.function <- function(A.is.victim=FALSE) {
    return (
        function(graph, vertex.A, vertex.B){
            if (A.is.victim) {
                vertex <- vertex.B
                victim.vertex <- vertex.A
            } else {
                vertex <- vertex.A
                victim.vertex <- vertex.B
            }
            if(!"stubbornness" %in% list.vertex.attributes(graph) ||
                  V(graph)[victim.vertex]$stubbornness == 0){
                return(list(new.value=V(graph)[vertex]$opinion,
                    victim.vertex=victim.vertex))
            } else {
                # Nothing will get updated. We're too dang stubborn.
                return(list(new.value=0,victim.vertex=NULL))
            }
        }
    )
}

get.update.majority.neighbors.function <- function(){
    return (
        function(graph, vertex.A, vertex.B){
            victim.vertex <- vertex.A
            neighbors <- neighbors(graph, victim.vertex, mode="out")
            if(!"stubbornness" %in% list.vertex.attributes(graph) ||
                    V(graph)[victim.vertex]$stubbornness == 0){
                zero <- 0
                one <- 1
                for(n in 1:length(neighbors)){
                    if(V(graph)[neighbors[n]]$opinion == 0){
                        zero <- zero + 1
                    } else{
                        one <- one + 1
                    }
                } 
                new.opinion <- ifelse(zero > one, 0, 1)
                return(list(new.value=new.opinion,
                         victim.vertex=victim.vertex))
            } else {
                # Nothing will get updated. We're too dang stubborn.
                return(list(new.value=0,victim.vertex=NULL))
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

            scaling.factor <- 1 / length(neighbors(graph, victim.vertex, mode="in"))
            probability.of.converting <- scaling.factor * (1- V(graph)[victim.vertex]$stubbornness)
            if (rbinom(1, 1, probability.of.converting) == 1){
                    return(list(new.value=V(graph)[vertex]$opinion,
                        victim.vertex=victim.vertex))
            } else {
                    # Victim vertex opinion value stays the same
                    return(list(new.value=0,victim.vertex=NULL))
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
                    new.value <- diff.of.opinion * migration.factor + V(graph)[victim.vertex]$opinion
                } else {
                    # We didn't pass our die roll. Don't update.
                    new.value <- V(graph)[victim.vertex]$opinion
                }
            } else {
                # This dude is too wacky for me to listen to him. Don't
                # update.
                new.value <- V(graph)[victim.vertex]$opinion
            }
            return(list(new.value=new.value,victim.vertex=victim.vertex))
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
get.graph.neighbors.encounter.func <- function(num.vertices=0, all=FALSE) {
    return(
        function(graph,vertex) {
            # Each vertex encounters some others at random from a vector
            # of its "outgoing" neighbors, of whom he may pass 
            # information to/influence.
            outgoing.neighbors <- neighbors(graph, V(graph)[vertex], mode="out")
            if(length(outgoing.neighbors) <= num.vertices || all){
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


# Terminology:
#
# ** edge update functions: an "edge update function" is one that takes a 
# graph and a vertex, and returns information about who the vertex should 
# connect to, and disconnect to. This is a list of two elements:
#    $new.edges -- vertex IDs this vertex is currently not connected to, but
#    should be.
#    $old.edges -- vertex IDs this vertex is currently connected to, but
#    shouldn't be.
#
# ** edge update generator functions: an "edge update generator function" is 
# one that can be called to return an edge update function.

dave.edge.update.function <- function() {
    return(
        function(g, vertex.ID){
            neighbors <- neighbors(g,vertex.ID,mode="in")
            new.edges <- vector()
            old.edges <- vector()
            for (n in 1:length(neighbors)){
                if( V(g)[neighbors[n]]$opinion == V(g)[vertex.ID]$opinion ){
                    foaf <- neighbors(g,neighbors[n],mode="in")
                    foaf <- foaf[-(n==vertex.ID)]
                    foaf <- foaf[-(which(neighbors %in% foaf))]
                    new.edges <- c(new.edges, foaf)
                } else {
                    old.edges <- c(old.edges, neighbors[n])
                }
            }
            return(list(new.edges, old.edges))
        }
    )
    # loop through all vertex.ID's neighbors. For each one:
    #   - if you agree, have victim add an edge to a random influencer's
    #       neighbor (FOAF)  (note: EVEN if that neighbor doesn't agree.)
    #   - if you disagree, break the edge
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

# Binary Opinions
#get.vector.opinions.graph <- function(num.opinions=3, num.nodes=40, prob.connect=0.2, dir=FALSE){
#    g <- erdos.renyi.game(num.nodes, prob.connect)
#    alpha <- c("a","b","c","d","e","f","g","h","i","j","k")
#    for( i in 1:num.opinions ) {
#        for( node in 1:num.nodes ) {
#            set_vertex_attr(g, alpha[i], node, sample(c(0,1),1))
#        }
#    }
#    return(g) 
#}

# Returns a graph whose nodes have a binary or continuous opinion attribute. 
# Default is continuous
get.plain.old.graph <- function(opinion=runif(30), probability.connected=0.2, dir=FALSE) {
    if(dir){
        g <- erdos.renyi.game(length(opinion),probability.connected, directed=TRUE)
    } else {
        g <- erdos.renyi.game(length(opinion),probability.connected)
    }
    V(g)$opinion <- opinion
    return(g)
}

param.sweep <- function(init.graph) {

    library(doParallel)
    registerDoParallel(8)

    encs.per.iter <- 4

    invisible(foreach (migration.factor=seq(0.2,1,.2)) %dopar% {
      for (bc.thresh in seq(0.2,1,.2)) {
        graphs <- sim.opinion.dynamics(init.graph,
            num.encounters=50*vcount(init.graph),
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
    da.graph <- get.discrete.graph(2,stubborn=TRUE)
    graphs <<- sim.opinion.dynamics(da.graph, 
        num.encounters=80*vcount(da.graph), 
        #encounter.func=get.graph.neighbors.encounter.func(2),
        encounter.func=get.mean.field.encounter.func(3),
        #victim.update.function=get.proportional.to.in.degree.update.victim.function())
        victim.update.function=get.automatically.update.victim.function())

    # Continuous Opinions
    # graphs <- sim.opinion.dynamics(get.stubborn.graph(), num.encounters=200,
    #   encounter.func=get.graph.neighbors.encounter.func(4),
    #   victim.update.function=get.bounded.confidence.update.victim.function(0.5, 0.2))

    plot.animation(graphs, "opinion", delay.between.frames=.25)
}
