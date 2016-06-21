# Reproduce results of published papers

source("opinionDynamics.R")

binary.voter <- function() {
    # (Holley and Liggett 1975)  (Clifford and Sudbury 1973)
    # Vertex always updates to victim's state
    # Results: Opinions will always converge to a consensus
    set.seed(111234)
    init <- get.plain.old.graph(opinion=rbinom(50,1,0.5), probability.connected=0.2)
    graphs <<- sim.opinion.dynamics(init, num.iter=20,
        encounter.func=get.graph.neighbors.encounter.func(1),
        victim.update.function=get.automatically.update.victim.function(),
        binaryVoterModel=TRUE)
    plot.animation(graphs, "opinion", delay.between.frames=.25)
}


yildiz.discrete <- function(){
    # Yildoz, Acemoglu, et al. 2011
    # Discrete Opinion Dynamics with Stubborn Agents
    
    #Binary Voter Model with Stubborn Agents
    set.seed(1111)
    initial.graph <- get.stubborn.graph(opinion=rbinom(30,1,0.5), probability.connected=0.05, 
        dir=TRUE, stubbornness=rbinom(30,1,0.4))
    binary.voter.stubborn.graph <<- sim.opinion.dynamics(initial.graph, num.iter=20,
        encounter.func=get.graph.neighbors.encounter.func(1),
        victim.update.function=get.automatically.update.victim.function(),
        binaryVoterModel=TRUE)
    plot.animation(binary.voter.stubborn.graph, "opinion", delay.between.frames=.25)
}

