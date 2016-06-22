# Reproduce results of published papers

source("opinionDynamics.R")

binary.voter <- function() {
    # Holley and Liggett 1975        Clifford and Sudbury 1973
    # Vertex always changes her opinion to reflect that of the victim's opinion
    # Only one encounter per iteration
    # Results: Opinions will always converge to a consensus
    set.seed(111234)
    init <- get.plain.old.graph(opinion=rbinom(50,1,0.5), probability.connected=0.2)
    graphs <<- sim.opinion.dynamics(init, num.encounters=50*vcount(init),
        encounter.func=get.graph.neighbors.encounter.func(1),
        victim.update.function=get.automatically.update.victim.function(A.is.victim=TRUE), 
        choose.randomly.each.encounter=FALSE)
    plot.animation(graphs, "opinion", delay.between.frames=.15)
}


yildiz.discrete <- function(){
    # Yildoz, Acemoglu, et al. 2011
    # Agent-based where each has a binary opinion and stubbornness value
    # Results: opinions never reach a consensus because every agent is indirectly connected to
    # stubborn agents who's opinions are never changed
    set.seed(2222)
    initial.graph <- get.stubborn.graph(opinion=rbinom(30,1,0.5), probability.connected=0.08, 
        dir=TRUE, stubbornness=rbinom(30,1,0.3))
    binary.voter.stubborn.graph <<- sim.opinion.dynamics(initial.graph, 
        num.encounters=20*vcount(initial.graph),
        encounter.func=get.graph.neighbors.encounter.func(1),
        victim.update.function=get.automatically.update.victim.function(),
        choose.randomly.each.encounter=TRUE)
        # TODO  Wait -- should A.is.victim be TRUE here? (after all, Yildiz was
        # based on Holley.)
    plot.animation(binary.voter.stubborn.graph, "opinion", delay.between.frames=.25)
}

