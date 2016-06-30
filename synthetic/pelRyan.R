source("opinionDynamics.R")

pelryan <- function() {

    #download.file("http://cs.umw.edu/~stephen/pelosiRyan.RData", "pelRyan.rData")
    #load("pelRyan.rData")

    first <<- U
    V(first)$opinion <- sample(c(0,1),vcount(first),replace=TRUE)
    pel.graphs <<- sim.opinion.dynamics(first, num.encounters=403062,
        encounter.func=get.graph.neighbors.encounter.func(vcount(first)),
        victim.update.function=get.automatically.update.function(),
        edge.update=FALSE)
    plot.animation(pel.graphs,"opinion",delay.between.frames=.15)

}
