source("opinionDynamics.R")

# Binary Opinions
# Graph does not change

pel.graphs <- function() {

    #download.file("http://cs.umw.edu/~stephen/pelosiRyan.RData", "pelosiRyan.RData")
    #load("pelosiRyan.RData")

    #first.g <<- U
    first.g <<- erdos.renyi.game(100,0.05)
    V(first.g)$opinion <<- sample(c(0,1),vcount(first.g),replace=TRUE)
    A <<- sim.opinion.dynamics(first.g,
        num.encounters=40000,
        encounter.func=get.graph.neighbors.encounter.func(1),
        victim.update.function=get.automatically.update.victim.function(A.is.victim=FALSE),
        edge.update=FALSE,
        choose.randomly.each.encounter=FALSE)
    #plot.animation(A,"opinion",delay.between.frames=.15, 
    #    interactive=FALSE, animation.filename="graphA.gif",
    #    overwrite.animation.file=TRUE)
    #plot.polarization(pel.graphs, "opinion")
    #plot.binary.opinions(pel.graphs)

}






