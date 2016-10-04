
# The "Hannah model" -- in-person and anonymous interactions unfold together
# to change agents' hidden and expressed opinions.
# Relevant question: how much of a "hidden Trump vote" might there be, given a
# certain set of expressed opinions?

library(ggplot2)

source("opinionDynamics.R")


# Hidden vs. Expressed Opinions

sim <- function(num=100, prob=0.25){
    init <- get.expressed.latent.graph(num.agents=num, prob.connected=prob, dir=TRUE)
    graphs <<- sim.opinion.dynamics(init, num.encounters=20000,
        encounter.func=get.mean.field.encounter.func(1),
        victim.update.function=
            get.automatically.update.victim.function(A.is.victim=TRUE,prob.update=0.3, opinion.type="hidden"),
#        encounter.func=list(
#            get.mean.field.encounter.func(1),
#            get.graph.neighbors.encounter.func(1)),
#        victim.update.function=list(
#            get.automatically.update.victim.function(A.is.victim=TRUE,prob.update=0.3,
#                opinion.type="hidden"),
#            get.peer.pressure.update.function(A.is.victim=TRUE,
#                prob.knuckle.under.pressure=0.5,
#                prob.internalize.expressed.opinion=0.5)),
        edge.update.function=get.no.edge.update.function(),
        verbose=TRUE,
        termination.function=get.never.terminate.function(),
		choose.randomly.each.encounter=TRUE)
}
