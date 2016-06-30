source("opinionDynamics.R")

pelryan <- function() {

    #download.file("http://cs.umw.edu/~stephen/pelosiRyan.RData", "pelosiRyan.RData")
    #load("pelosiRyan.RData")

    first.g <<- pelosi.ryan
    V(first.g)$opinion <<- sample(c(0,1),vcount(first.g),replace=TRUE)
    pel.graphs <<- sim.opinion.dynamics(first.g,
        num.encounters=vcount(first.g)*4,
        encounter.func=get.mean.field.encounter.func(2),
        victim.update.function=get.automatically.update.victim.function(),
        edge.update=FALSE,
        choose.randomly.each.encounter=TRUE)
    plot.animation(pel.graphs,"opinion",delay.between.frames=.15)

}
