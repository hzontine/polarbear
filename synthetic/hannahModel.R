
# The "Hannah model" -- in-person and anonymous interactions unfold together
# to change agents' hidden and expressed opinions.
# Relevant question: how much of a "hidden Trump vote" might there be, given a
# certain set of expressed opinions?

library(ggplot2)

source("opinionDynamics.R")


# Hidden vs. Expressed Opinions

hannahModel <- function(num=20, prob=0.25){
    init <<- get.expressed.latent.graph(num.agents=num, prob.connected=prob, dir=TRUE)
    graphs <<- sim.opinion.dynamics(init, num.encounters=2000,
        encounter.func=list(
            get.mean.field.encounter.func(1),
            get.graph.neighbors.encounter.func(1)),
        victim.update.function=list(
            get.automatically.update.victim.function(A.is.victim=TRUE,prob.update=0.5,
                opinion.type="hidden"),
            get.peer.pressure.update.function(A.is.victim=TRUE,
                prob.knuckle.under.pressure=0.5,
                prob.internalize.expressed.opinion=0.5)),
        edge.update.function=get.no.edge.update.function(),
        verbose=FALSE,
        termination.function=get.unanimity.termination.function("expressed", "hidden"),
		choose.randomly.each.encounter=TRUE)

    #num.hidden.1s <<- sapply(1:length(graphs),function(i) { sum(get.vertex.attribute(graphs[[i]],"hidden"))})
    #num.expressed.1s <<- sapply(1:length(graphs),function(i) { sum(get.vertex.attribute(graphs[[i]],"expressed"))})

    #animated.graph <- plot.animation(graphs, attribute.name="expressed", second.attribute = "hidden",
    #    delay.between.frames=1)
    #plot(animated.graph)
    #plot.binary.opinions(graphs, attribute1="expressed", attribute2="hidden")
    return(graphs)
}

