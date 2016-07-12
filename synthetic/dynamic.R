source("opinionDynamics.R")
source("plotting.R")


dave <- function() {

    init.graph <<- get.plain.old.graph(opinion=sample(c(0,1),20,replace=TRUE), 
        probability.connected=0.3) 

    graphs <<- sim.opinion.dynamics(init.graph, num.encounters=20,
        edge.update.function=get.dave.edge.update.function(verbose=TRUE,
                                                foafs.must.agree=FALSE),
        generate.graph.per.encounter=TRUE, verbose=TRUE)
    
    plot.animation(graphs,"opinion",delay.between.frames=.5)
}
