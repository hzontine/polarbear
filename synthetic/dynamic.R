source("opinionDynamics.R")
source("plotting.R")


dave <- function() {

    init.graph <<- get.plain.old.graph(opinion=sample(c(0,1),40,replace=TRUE), 
        probability.connected=0.1) 

    graphs <<- sim.opinion.dynamics(init.graph, num.encounters=30,
        edge.update.function=get.dave.edge.update.function(verbose=TRUE,
                                                foafs.must.agree=TRUE),
        generate.graph.per.encounter=TRUE, verbose=TRUE)
    
    plot.animation(graphs,"opinion",delay.between.frames=.5,
        interactive=FALSE, animation.filename="dave.gif",
        overwrite.animation.file=TRUE)
}
