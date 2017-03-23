
# Code to reproduce standard opinion dynamics simulation results.
# Stephen and Hannah

library(igraph)
if (file.exists("plotting.R")) {
    source("plotting.R")
} else if (file.exists("../plotting.R")) {
    source("../plotting.R")
} else {
    stop("Whoa -- can't find plotting.R!")
}

# Run an opinion dynamics simulation with n agents for num.encounters
# encounters. Return a list of igraph objects, each giving the graph at a
# snapshot in time immediately following an encounter.
# 
# Agents have an opinion, which could be binary or weighted, and a
# stubbornness attribute, which can be binary or weighted (coming soon).
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
# are all chosen again (i.e., no repeats are allowed.) (Note: TRUE corresponds
# to Holley's original; FALSE is the Zontine variant.)
#
# encounter.func -- either (a) a function which takes a graph and a vertex ID.
# Returns a vector of vertex IDs of which the vector may randomly encounter in
# the current iteration. Or (b) a list of such functions. In the former case,
# victim.update.function must also be a function (not a list). In the latter
# case, this list should be element-by-element paired with the list of update
# functions in victim.update.function.
#
# victim.update.function -- either (a) a function which takes a graph and two
# vertex IDs: the first is the "potential influencer" (i.e., the node whose
# opinion may cause the second vertex's opinion to be updated) and the second
# is the "potential victim." It will return the (possibly new) value of the
# second vetex. Or (b) a list of such functions. In the former case,
# encounter.func must also be a function (not a list). In the latter case,
# this list should be element-by-element paired with the list of encounter
# functions in encounter.func.
#
# edge.update.function -- a function which takes a graph and a vertex ID. It
# will return information about who the vertex should connect to, and
# disconnect to. A list of two elements: $new.edges -- vertex IDs this vertex
# is currently not connected to, but should be. $old.edges -- vertex IDs this
# vertex is currently connected to, but shouldn't be.
#
# generate.graph.per.encounter -- if TRUE, return a graph for every single
# encounter that takes place. If FALSE, only return one graph for an entire
# "iteration," where "iteration" is defined as "as many encounters as there
# are vertices." (This effectively means "every vertex gets a chance to
# influence" if choose.randomly.each.encounter is FALSE.)
#
# terminate.after.max.num.encounters -- if TRUE, put a cap on the total number
# of encounters the simulation will run. If FALSE, never terminate until the
# termination function itself says to stop.

sim.opinion.dynamics <- function(init.graph,
        num.encounters=200,
        encounter.func=get.mean.field.encounter.func(1),
        victim.update.function=get.no.update.victim.function(),
        choose.randomly.each.encounter=FALSE,
#        edge.update.function=get.no.edge.update.function(),
        generate.graph.per.encounter=FALSE,
        termination.function=get.unanimity.termination.function(),
        terminate.after.max.num.encounters=!is.infinite(num.encounters),
        verbose=FALSE,
        progress=NULL) {

	cat("Starting.....\n")


    if (!is.list(encounter.func)) {
        # If it's not already a list, make it one, so hereafter we can assume
        # it's always a list of one or more encounter functions.
        encounter.func <- list(encounter.func)
        victim.update.function <- list(victim.update.function)
    }

    if (terminate.after.max.num.encounters) {
        if (generate.graph.per.encounter) {
            graphs <- vector("list",length=num.encounters)
        } else {
            graphs <-
                vector("list",
                    length=trunc((num.encounters/gorder(init.graph))+1))
        }
    } else {
        graphs <- vector("list",length=200)
    }

    graphs[[1]] <- init.graph
    graphs[[1]] <- set.graph.attribute(graphs[[1]], "num.encounters", 0)
    graphs[[1]] <- set.graph.attribute(graphs[[1]], "num.current.hidden.encounters", 0)
    graphs[[1]] <- set.graph.attribute(graphs[[1]], "num.current.exp.encounters", 0)
    graphs[[1]] <- set.graph.attribute(graphs[[1]], "num.effective.encounters", 0)
    graphs[[1]] <- set.graph.attribute(graphs[[1]], "blue.hidden.effective.encounters", 0)
    graphs[[1]] <- set.graph.attribute(graphs[[1]], "red.hidden.effective.encounters", 0)
    graphs[[1]] <- set.graph.attribute(graphs[[1]], "blue.exp.effective.encounters", 0)
    graphs[[1]] <- set.graph.attribute(graphs[[1]], "red.exp.effective.encounters", 0)
    
    
    graphs[[2]] <- graphs[[1]]
    graph.num <- 2

    encounter.num <<- 0
    num.effective.encounters <<- 0

    
    # For each iteration of the sim...
    while ((!terminate.after.max.num.encounters ||
                                        encounter.num < num.encounters)  &&
        !termination.function(graphs[[graph.num]])) {

        if (verbose) {
            cat("Encounter #",encounter.num,"...\n")
        }

        current.hidden.encounters <- 0
        current.exp.encounters <- 0
        blue.hidden.encounters <- 0
        red.hidden.encounters <- 0
        blue.exp.encounters <- 0
        red.exp.encounters <- 0
        
        # Go through all the vertices, in random order:
        for (v in sample(1:gorder(graphs[[graph.num]]))) {

            if (termination.function(graphs[[graph.num]])) {
                break
            }

            # If Holley variant, then choose a random vertex instead of the
            # one from our shuffling, above.
            if (choose.randomly.each.encounter) {
                v <- sample(1:gorder(graphs[[graph.num]]),1)
            }

            for (i in 1:length(encounter.func)){
           
                list.of.vertex.IDs <- 
                    encounter.func[[i]](graphs[[graph.num]],v)

                # (R passes by value, so this part is harder.)
                for (id in list.of.vertex.IDs) {
                    update.info <- 
                        victim.update.function[[i]](graphs[[graph.num]],v,id)
                    
                    encounter.num <<- encounter.num + 1
                    
                    if(update.info$type == "hidden"){
                      current.hidden.encounters <- current.hidden.encounters + 1
                    } else {
                      current.exp.encounters <- current.exp.encounters + 1
                    }
                    
                    if (!is.null(progress)) {
                        progress$set(message=paste0("Encounter ",
                            encounter.num,"/",num.encounters),
                            value=encounter.num)
                    }

                    if (length(update.info$victim.vertex) > 0  &&
                        get.vertex.attribute(graphs[[graph.num]],
                            update.info$type,
                            update.info$victim.vertex) !=
                                update.info$new.value) {

                        graphs[[graph.num]] <- 
                        	set.vertex.attribute(graphs[[graph.num]],
                               	update.info$type, update.info$victim.vertex,
                               	update.info$new.value)

                        if(update.info$type == "hidden"){
                          if (update.info$new.value == 0){
                            blue.hidden.encounters <- blue.hidden.encounters + 1
                          } else {
                            red.hidden.encounters <- red.hidden.encounters + 1
                          }
                        } else{
                          if(update.info$new.value == 0){
                            blue.exp.encounters <- blue.exp.encounters + 1
                          } else{
                            red.exp.encounters <- red.exp.encounters + 1
                          }
                        }
                        num.effective.encounters <<- num.effective.encounters + 1
                    }
                    graphs[[graph.num]] <- append.message(graphs[[graph.num]],
                        update.info$message)
                }
            }


            if (generate.graph.per.encounter) {
                # Create a new igraph object to represent this point in time. 
                graphs[[graph.num+1]] <- 
                    set.graph.attribute(graphs[[graph.num]],"message",NULL)
                graph.num <- graph.num + 1

                # Annotate the graph object with a graph attribute indicating
                # the number of encounters that had taken place at the time
                # this snapshot was taken.

                # (This is hardcoded to assume exactly two encounter/vupdate
                # functions, the first of which updates hidden and the second
                # of which updates expressed.)
                graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]],
                    "num.encounters", encounter.num)
                graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]],
                    "num.effective.encounters", num.effective.encounters)
                graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]], 
                    "num.current.hidden.encounters", current.hidden.encounters)
                graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]], 
                    "num.current.exp.encounters", current.exp.encounters)
                graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]],
                    "blue.hidden.effective.encounters", blue.hidden.encounters)
                graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]], 
                    "red.hidden.effective.encounters", red.hidden.encounters)
                graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]],
                    "blue.exp.effective.encounters", blue.exp.encounters)
                graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]], 
                    "red.exp.effective.encounters", red.exp.encounters)
                
            }
        }

        if (!termination.function(graphs[[graph.num]])  &&
                !generate.graph.per.encounter) {
            # Create a new igraph object to represent this point in time. 
            graphs[[graph.num+1]] <- 
                set.graph.attribute(graphs[[graph.num]],"message",NULL)
            graph.num <- graph.num + 1

            # Annotate the graph object with a graph attribute indicating the
            # number of encounters that had taken place at the time this
            # snapshot was taken.

            # (This should be updated to match the above? (hidden vs
            # effectual?))
            graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]],
                 "num.encounters", encounter.num)
            graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]],
                  "num.effective.encounters", num.effective.encounters)
            graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]], 
                  "num.current.hidden.encounters", current.hidden.encounters)
            graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]], 
                  "num.current.exp.encounters", current.exp.encounters)
            graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]],
                  "blue.hidden.effective.encounters", blue.hidden.encounters)
            graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]], 
                  "red.hidden.effective.encounters", red.hidden.encounters)
            graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]],
                  "blue.exp.effective.encounters", blue.exp.encounters)
            graphs[[graph.num]] <- set.graph.attribute(graphs[[graph.num]], 
                  "red.exp.effective.encounters", red.exp.encounters)
 
        }
    }
	cat("Done!\n")
    if (!is.null(progress)) {
        progress$close()
    }
    graphs[1:graph.num]
}



# Terminology:
#
# ** termination functions: a "termination function" is one that takes a graph
# and returns TRUE if that graph is considered a "terminal state" of a
# simulation -- i.e., if the simulation should stop when this graph is
# reached.
#
# ** termination generator functions: a "termination generator function"
# is one that can be called to return a termination function.

# Never terminate.
get.never.terminate.function <- function() {
    return (
        function(graph) {
            return(FALSE)
        }
    )
}

# Terminate if the graph has total uniformity of opinions.
get.unanimity.termination.function <- function(attribute1="opinion") {
    return (
        function(graph) {
            if(attribute1 == "opinion" || attribute1 == "hidden" || attribute1 == "expressed") {
                return (length(unique(get.vertex.attribute(graph, attribute1))) == 1)
            }else if (attribute1 == "both") {
                # attribute1 == "both"
                return ((length(unique(get.vertex.attribute(graph, "hidden"))) == 1) &&
                    (length(unique(get.vertex.attribute(graph, "expressed"))) == 1))
            } else {
                # Never terminate.
                return(FALSE)
            }
        }
    )
}


color.for <- function(n) {
    ifelse(n==0,"blue","red")
}

append.message <- function(graph,message) {
    curr.msg <- get.graph.attribute(graph, "message")
    if (is.null(curr.msg)) {
        return(set.graph.attribute(graph, "message", 
                message))
    } else {
        return(set.graph.attribute(graph, "message", 
            paste(curr.msg,message,sep="\n")))
    }
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

# Never update the victim.
get.no.update.victim.function <- function() {
    return (
        function(graph, vertex.A, vertex.B){
            return(list(new.value=0,victim.vertex=NULL))
        }
    )
}

# "Always" (meaning, with the given probability) update the victim with the
# influencer's opinion.
get.automatically.update.victim.function <- function(A.is.victim=FALSE, prob.update=0.4,
    opinion.type="opinion") {
    return (
        # Opinion is default
        function(graph, vertex.A, vertex.B) {
            if (A.is.victim) {
                vertex <- vertex.B
                victim.vertex <- vertex.A
            } else {
                vertex <- vertex.A
                victim.vertex <- vertex.B
            }
            if(!"stubbornness" %in% list.vertex.attributes(graph)
                || V(graph)[victim.vertex]$stubbornness == 0){

                if (get.vertex.attribute(graph,opinion.type,vertex) ==
                  get.vertex.attribute(graph,opinion.type,victim.vertex)){

                    return(list(new.value=0,
                        victim.vertex=NULL, type="hannah",
                        message=paste0("online: ", vertex," talks, but ", 
                            victim.vertex, " is already ", 
                            color.for(get.vertex.attribute(graph,
                            opinion.type,vertex)))))
                }

                if(runif(1) < prob.update) {


                    return(list(new.value=get.vertex.attribute(graph,
                            opinion.type,vertex),
                        victim.vertex=victim.vertex, type=opinion.type,
                        message=paste0("online: ", vertex," persuades ", 
                            victim.vertex, " to go ", 
                            color.for(get.vertex.attribute(graph,
                            opinion.type,vertex)))))
                } else{
                    return(list(new.value=0,victim.vertex=NULL,
                        type=opinion.type,
                        message=paste0("online: ", vertex,
                            " unable to persuade ",
                            victim.vertex, " to go ", 
                            color.for(get.vertex.attribute(graph,
                                opinion.type,vertex)))))
                }
            } else {
                stop("stubborn")
                # Nothing will get updated. We're too dang stubborn.
                return(list(new.value=0,victim.vertex=NULL,type=opinion.type))
            }
        }
    )
}

# Possibly update my expressed opinion, and/or my hidden opinion, based on
# what another node are expressing.
# More precisely: (1) if our expressed opinion differs from that of the other
# node, possibly update our expressed opinion to match the other's.
# (Probability prob.knuckle.under.pressure.)
# (2) Otherwise, if our expressed opinions already agree, possibly update our
# own hidden opinion to reflect our expressed opinion. (Probability
# prob.internalize.expressed.opinion.)
get.peer.pressure.update.function <- function(A.is.victim=FALSE,
    prob.knuckle.under.pressure, prob.internalize.expressed.opinion, trumpEffect=TRUE) {
    return (
        # Opinion is default
        function(graph, vertex.A, vertex.B) {
            if (A.is.victim) {
                vertex <- vertex.B
                victim.vertex <- vertex.A
            } else {
                vertex <- vertex.A
                victim.vertex <- vertex.B
            }

            # For now, assume we're not stubborn. (This is complicated enough
            # as it is.)
            if (V(graph)[vertex]$expressed != 
                V(graph)[victim.vertex]$expressed) {
                # We don't agree with them externally. Possibly succumb to
                # peer pressure and "pretend" we agree.

              if(trumpEffect){
                # if victim is red
                  if(V(graph)[victim.vertex]$expressed == 1 && runif(1) < prob.knuckle.under.pressure) {

                      return(list(new.value=V(graph)[vertex]$expressed,
                         victim.vertex=victim.vertex, type="expressed",
                         message=paste0("toface: ", vertex," intimidates ", 
                              victim.vertex, " to pretend he's ", 
                              color.for(V(graph)[vertex]$expressed))))
                  } else {
                      return(list(new.value=0,victim.vertex=NULL, type="hannah",
                         message=paste0("toface: ", vertex,
                              " UNsuccessfully tries to pressure ",
                              victim.vertex, " to go ", 
                              color.for(V(graph)[vertex]$expressed))))
                  }
               } else {
                  if(runif(1) < prob.knuckle.under.pressure){
                      return(list(new.value=V(graph)[vertex]$expressed,
                              victim.vertex=victim.vertex, type="expressed",
                              message=paste0("toface: ", vertex," intimidates ", 
                                             victim.vertex, " to pretend he's ", 
                                             color.for(V(graph)[vertex]$expressed))))
                  } else {
                      return(list(new.value=0,victim.vertex=NULL, type="hannah",
                              message=paste0("toface: ", vertex,
                                             " UNsuccessfully tries to pressure ",
                                             victim.vertex, " to go ", 
                                             color.for(V(graph)[vertex]$expressed))))
                  }
               }
            } else {
                # We already agree with them externally. Possibly update our
                # hidden opinion to match.
                if (V(graph)[victim.vertex]$expressed ==
                    V(graph)[victim.vertex]$hidden) {
                        return(list(new.value=0,victim.vertex=NULL, 
                            type="hannah",
                            message=paste0("toface: (", 
                                vertex, " talks, but ", 
                                victim.vertex, " already true ",
                                color.for(V(graph)[vertex]$expressed), 
                                " believer)")))
                }
                if (runif(1) < prob.internalize.expressed.opinion) {
                    return(list(new.value=V(graph)[victim.vertex]$expressed,
                       victim.vertex=victim.vertex, type="hidden",
                       message=paste0("toface: ", vertex," convinces ", 
                            victim.vertex, 
                            ", who was nominally already ", 
                            color.for(V(graph)[vertex]$expressed), 
                            ", to be true believer")))
                } else {
                    return(list(new.value=0,victim.vertex=NULL, type="hannah",
                       message=paste0("toface: ", vertex," can't convince ", 
                            victim.vertex, " to truly be ", 
                            color.for(V(graph)[vertex]$expressed), 
                            " (still faking it)")))
                }
            }
        }
    )
}

# Update the victim by making its opinion be whatever opinion the majority of
# its neighbors have.
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
                new.opinion <- ifelse(zero > one, 0, 1) # HZ TODO: flip coin
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

# Each vertex encounters some other vertices at random (mean field) and may or
# may not update his/her latent opinion as a result.
get.mean.field.encounter.func <- function(num.vertices) {
    return(
        function(graph, vertex) {
            return (
                sample((1:gorder(graph))[-vertex],num.vertices)
            )
        }
    )
}

# Each vertex encounters some others at random from a vector of its "outgoing"
# neighbors, of whom he may pass information to/influence.
get.graph.neighbors.encounter.func <- function(num.vertices=0, all=FALSE) {
    return(
        function(graph,vertex) {
		if(num.vertices==0) {
			all=TRUE
		}
		outgoing.neighbors <- neighbors(graph, vertex, mode="in")
            	if(length(outgoing.neighbors) <= num.vertices || all){
                	return (outgoing.neighbors)
            } else {
                return (sample(outgoing.neighbors, num.vertices)
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

get.no.edge.update.function <- function() {
    return(
        function(g, vertex.ID){
            return(list(NULL,NULL))
        }
    )
}

# The Dave Model(tm).
# Loop through all vertex.ID's neighbors. For each one:
#   - if you agree, have victim add an edge to a random influencer's
#       neighbor (FOAF)  (the foafs.must.agree parameter controls whether that
#       FOAF must already agree with us in order to be added.)
#   - if you disagree, break the edge
get.dave.edge.update.function <- function(verbose=FALSE, 
        foafs.must.agree=FALSE) {
    return(
        function(g, vertex.ID){
            neighbors <- neighbors(g,vertex.ID,mode="in")
            new.edges <- vector()
            old.edges <- vector()
            for (neighbor in neighbors){
                if( V(g)[neighbor]$opinion == V(g)[vertex.ID]$opinion ){
                    foaf <- neighbors(g,neighbor,mode="in")
                    foaf <- foaf[foaf!=vertex.ID]
                    foaf <- foaf[!(foaf %in% neighbors)]
                    # Only add friends-of-a-friend who already agree with us.
                    if (foafs.must.agree) {
                        new.edges <- union(new.edges, 
                            foaf[(V(g)[foaf]$opinion == 
                                                    V(g)[vertex.ID]$opinion)])
                    } else {
                        new.edges <- union(new.edges, foaf)
                    }
                } else {
                    old.edges <- union(old.edges, neighbor)
                }
            }
            if (verbose) {
                cat("Updating edges from node ", vertex.ID, ":\n", sep="")
                if (length(new.edges) > 0) {
                    cat("  Adding edges from",vertex.ID,"to",new.edges,"\n")
                }
                if (length(old.edges) > 0) {
                    cat("  Removing edges from",vertex.ID,"to",old.edges,"\n")
                }
            }
            return(list(new.edges, old.edges))
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


get.expressed.latent.graph <- function(num.agents=100, prob.connected=0.2, dir=FALSE){
    if(dir){
        g <- erdos.renyi.game(num.agents, prob.connected, directed=TRUE)
    } else {
        g <- erdos.renyi.game(num.agents, prob.connected)
    }
    if(num.agents %% 4 == 0){
        # Note: "0" means B(B), "1" means B(R), "2" means R(B), "3" means
        # R(R). We are doing this to ensure we start with a population evenly
        # divided among these four opinion-pairings.
        values <- c(rep(0, num.agents/4), rep(1, num.agents/4),
                    rep(2, num.agents/4), rep(3, num.agents/4))
    }else{
        stop("You should have a divisible-by-4 number of nodes in the graph!\n")
    }
    shuffled.agent.opinions <- sample(values)
    V(g)$expressed <- 
        ifelse(shuffled.agent.opinions==0 | shuffled.agent.opinions==1, 0, 1)
    V(g)$hidden <- 
        ifelse(shuffled.agent.opinions==0 | shuffled.agent.opinions==2, 0, 1)
    if (!is.connected(g)) {
        stop("WHOA!!")
    }
    return(g)
}


                
#default number of seeds is 50 
param.sweep <- function(seeds=seq(10,500,10), prob.internalize=seq(0,1,0.05)) {

  library(doParallel)
  registerDoParallel(60)
  
  bias.list <- list()
  firstRun <- TRUE
  # for each value of peer pressure probability, compute poll bias
  for(i in 1:length(prob.internalize)){
    internal.prob <- prob.internalize[i]
    # run the same seeds for each probability
    bias.data <- foreach(num=1:length(seeds), .combine = 'cbind') %dopar% {
        set.seed(seeds[num])
        initial.graph <- get.expressed.latent.graph(num.agents = 64, prob.connected = 0.25, dir = FALSE)
        graphs <- sim.opinion.dynamics(initial.graph, num.encounters=200*vcount(initial.graph),
                                     encounter.func=list(get.mean.field.encounter.func(1),
                                       get.graph.neighbors.encounter.func(1)),
                                     victim.update.function=list(get.automatically.update.victim.function(A.is.victim=TRUE,
                                        prob.update=0.5, opinion.type="hidden"), get.peer.pressure.update.function(A.is.victim=TRUE,
                                        prob.knuckle.under.pressure=0.5, prob.internalize.expressed.opinion=internal.prob, trumpEffect=TRUE)),
                                     generate.graph.per.encounter=TRUE, verbose = TRUE,
                                     termination.function=get.never.terminate.function(),
                                     choose.randomly.each.encounter=TRUE)
        
        # compute poll bias over time
        bias <- sapply(graphs, function(graph){
            exp <- sapply(1:length(V(graph)), function(v){
              get.vertex.attribute(graph, "expressed", index=v)
            })
            hidden <- sapply(1:length(V(graph)), function(v){
              get.vertex.attribute(graph, "hidden", index=v)
            })
            percent.exp <- length(which(exp == 0)) / length(exp)
            percent.hidden <- length(which(hidden == 0)) / length(hidden)
            return(percent.exp - percent.hidden)
        })
        return(bias)
    }
    colnames(bias.data) <- seeds
    x <- list(probability=internal.prob, biasVector=bias.data)
    if(firstRun){
        bias.list <- x
        firstRun <- FALSE
    } else{
        bias.list <- list(bias.list, x)
    }
  }
  return(bias.list)
}




main <- function() {
    set.seed(11111)
    #param.sweep(get.barely.connected.polarized.graph())

    # Discrete Opinions
   #  da.graph <- get.discrete.graph(2,stubborn=TRUE)
   # graphs <<- sim.opinion.dynamics(da.graph, 
   #     num.encounters=80*vcount(da.graph), 
        #encounter.func=get.graph.neighbors.encounter.func(2),
   #     encounter.func=get.mean.field.encounter.func(3),
        #victim.update.function=get.proportional.to.in.degree.update.victim.function())
   #     victim.update.function=get.automatically.update.victim.function())

    # Continuous Opinions
    # graphs <- sim.opinion.dynamics(get.stubborn.graph(), num.encounters=200,
    #   encounter.func=get.graph.neighbors.encounter.func(4),
    #   victim.update.function=get.bounded.confidence.update.victim.function(0.5, 0.2))

    #(graphs, "opinion", delay.between.frames=.25)
}


print.transcript <- function(graphs, file.name="/tmp/transcript.txt") {
    transcript <- ""
    for (graph in graphs) {
        transcript <- paste(transcript,get.graph.attribute(graph,"message"),sep="\n")
        transcript <- paste(transcript,"\n------------------------\n")
    }
    cat(transcript,file=file.name)
}
