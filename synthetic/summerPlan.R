source("opinionDynamics.R")

# Binary Opinions
# Graph does not change

pelryan <- function() {

    #download.file("http://cs.umw.edu/~stephen/pelosiRyan.RData", "pelosiRyan.RData")
    load("pelosiRyan.RData")

    first.g <<- U
    V(first.g)$opinion <<- sample(c(0,1),vcount(first.g),replace=TRUE)
    pel.graphs <<- sim.opinion.dynamics(first.g,
        num.encounters=5,
        encounter.func=get.graph.neighbors.encounter.func(1),
        victim.update.function=get.automatically.update.victim.function(),
        edge.update=FALSE,
        choose.randomly.each.encounter=TRUE)
    #plot.animation(pel.graphs,"opinion",delay.between.frames=.15)
    plot.polarization(pel.graphs, "opinion")
    plot.binary.opinions(pel.graphs)
}
