
library(igraph)


set.seed(1234)


# Run an agent-based simulation of political polarization with purely
# synthetic data. Return a list of graph objects for each of the num.iter
# iterations. Each graph object represents one point in time as a social
# network evolves. The vertices in each graph have an "ideology" attribute
# whose value is from 0 to 1, where 0 means "ultra-liberal" and 1
# "ultra-conservative." Edges indicate friendships between vertices. For each
# iteration of the simulation, vertices randomly encounter some number of
# other vertices, and add an edge between them (if one doesn't exist already)
# only if their ideology difference is below some threshold.
#
# init.vals -- a vector of n nodes (n is up to the caller) with the initial
# ideologies for each of the nodes.
#
# num.iter -- the number of iterations to run the simulation.
#
# num.mediators -- the number of mediators (artificially introduced nodes) to
# add each time step. This can be less than one, in which case it is
# interpreted as the probability of adding one (1) mediator each time step.
# 
# num.encounters.per.iter -- the number of randomly encountered other vertices
# (either already neighbors or no) that each vertex will encounter each
# iteration.
#
# closeness.threshold -- when one vertex encounters another, if their
# ideologies are closer than this threshold, they will become neighbors (if
# not already).
#
# Returns: a list of num.iter igraph objects, one per iteration of time.
#
run.polar <- function(init.vals=runif(50), num.iter=20, num.mediators=0,
num.encounters.per.iter=3, closeness.threshold=.2) {

    stopifnot(num.encounters.per.iter < length(init.vals))

    graphs <- list(length=num.iter)

    # For now, vanilla random graph.
    graphs[[1]] <- erdos.renyi.game(length(init.vals), p=.1)
    V(graphs[[1]])$ideology <- init.vals
    
    for (iter in 2:num.iter) {
        cat("Computing iteration",iter,"...\n")

        graphs[[iter]] <- graphs[[iter-1]]
        for (vertex in 1:gorder(graphs[[iter]])) {
            encountered.vertices <- sample(1:gorder(graphs[[iter]]),
                num.encounters.per.iter)
            encountered.vertices <- 
                encountered.vertices[encountered.vertices != vertex]
            ideologies <- V(graphs[[iter]])[encountered.vertices]$ideology
            diffs <- abs(ideologies - V(graphs[[iter]])[vertex]$ideology)
            graphs[[iter]][vertex,
                encountered.vertices[diffs < closeness.threshold]] <- 1
        }
    }
    graphs
}


# Given a list of graphs, plot them, ensuring that vertices are plotted in the
# same location from graph to graph.
#
# graphs -- a list of igraph objects as produced by run.polar.
#
# keep.vertex.positions -- if TRUE, ensure that each vertex is plotted in the
# same x,y position from frame to frame.
#
# interactive -- if TRUE, display the animation within R. If FALSE, create an
# animation file in the current directory with the name specified.
#
# delay.between.frames -- the delay, in seconds, between graph displays.
#
# animation.filename -- only relevant if interactive is FALSE.
#
# overwrite.animation.file -- controls whether to overwrite or error out if
# file already exists. Only relevant if interactive is FALSE.
#
plot.animation <- function(graphs, keep.vertex.positions=FALSE,
    delay.between.frames=.5, interactive=TRUE, animation.filename="polar.gif",
    overwrite.animation.file=FALSE) {

    if (!interactive && !overwrite.animation.file) {
        if (file.exists(animation.filename)) {
            stop(paste0("File ",animation.filename, " already exists!"))
        }
    }

    vertex.coords <- layout_(graphs[[1]],nicely())
    for (i in 1:length(graphs)) {
        if (keep.vertex.positions) {
            V(graphs[[i]])$x <- vertex.coords[,1]
            V(graphs[[i]])$y <- vertex.coords[,2]
        }
        V(graphs[[i]])$color <- 
            colorRampPalette(c("blue","white","red"))(100)[
                ceiling(V(graphs[[i]])$ideology * 100)]
        if (!interactive) {
            png(paste0("plot",rep(0,floor(log10(i)+1)),i,".png"))
        }
        plot(graphs[[i]], main=paste("Iteration",i,"of",length(graphs)))
        legend("bottomright",legend=c("Liberal","Moderate","Conservative"),
            fill=c("blue","white","red"))
        if (interactive) {
            Sys.sleep(delay.between.frames)
        } else {
            dev.off()
        }
    }
    if (!interactive) {
        system(paste0("convert -delay ",delay.between.frames*100,
            " plot*.png ", animation.filename))
        system("rm plot*.png")
    }
}


# Plot the polarization vs. time for the list of graphs passed.
#
# graphs -- a list of igraph objects, presumably created from run.polar().
#
plot.polarization <- function(graphs) {
    assortativities <- sapply(graphs, function(graph) {
        assortativity(graph,types1=V(graph)$ideology)
    })
    plot(1:length(graphs),assortativities, type="l",ylim=c(-1,1),
        main="Polarization over time", xlab="time (iteration)",
        ylab="Assortativity of ideology")
}


main <- function() {
    polar.graphs <- run.polar()
    plot.animation(polar.graphs)
    plot.polarization(polar.graphs)
}
