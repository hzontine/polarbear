source("opinionDynamics.R")
source("plotting.R")


dave <- function() {

    init.graph <<- get.plain.old.graph(opinion=sample(c(0,1),20,replace=TRUE), 
        probability.connected=0.3) 

    graphs <<- sim.opinion.dynamics(init.graph, num.encounters=40,
        edge.update.function=get.dave.edge.update.function(TRUE),
        generate.graph.per.encounter=FALSE,
        verbose=TRUE)
    
    plot.animation(graphs,"opinion",delay.between.frames=.8)
}
