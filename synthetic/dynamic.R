source("opinionDynamics.R")


dave <- function() {

    init.graph <<- get.plain.old.graph(opinion=sample(c(0,1),50,replace=TRUE), 
        probability.connected=0.3) 
    graphs <<- sim.opinion.dynamics(init.graph, num.encounters=300,
        edge.update.function=dave.edge.update.function(),
        verbose=TRUE)
    
    plot.animation(graphs,"opinion",delay.between.frames=.2)
}
