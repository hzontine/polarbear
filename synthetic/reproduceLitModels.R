# Reproduce results of published papers

library(ggplot2)

source("opinionDynamics.R")


# Holley and Liggett 1975        Clifford and Sudbury 1973
# Vertex always changes her opinion to reflect that of the victim's opinion
# Only one encounter per iteration
# Results: Opinions will always converge to a consensus
#
# choose.randomly.each.encounter -- if TRUE, this is Holley's original: each
# encounter will have a completely random "encountering vertex" chosen. If
# FALSE, this is the Zontine variant: all the vertices will be chosen as the
# "encountering vertex" (in random order) before they are all chosen again
# (i.e., no repeats are allowed.)
#
binary.voter <- function(plot=TRUE, num=50, prob=0.3) {
    #set.seed(111234)
    init <- get.plain.old.graph(opinion=rbinom(num,1,0.5), 
        probability.connected=prob)
    while(!(is.connected(init))){
        init <- get.plain.old.graph(opinion=rbinom(num,1,0.5), probability.connected=0.3)
    }
    graphs <<- sim.opinion.dynamics(init.graph=init, num.encounters = Inf,
        encounter.func=get.graph.neighbors.encounter.func(1),
        victim.update.function=get.automatically.update.victim.function(A.is.victim=FALSE), 
        choose.randomly.each.encounter=TRUE)
    if (plot) {
        plot.animation(graphs, "opinion", delay.between.frames=.5)
        plot.binary.opinions(graphs, attribute1="opinion")
    }
    return(graphs)
}


# Verify that choosing each (shuffled) vertex in sequence before choosing each
# one again reaches consensus faster than the original Holley version in which
# every encounter chooses a new influencing vertex afresh.
#
# results -- if NULL, re-run the parameter sweep from scratch. If not, results
# should be the return value from a previous call to this function.
#
# num.trials -- how many simulations of each of the two variants to run.
#
# Returns: a data frame with two columns: choose.randomly (boolean; FALSE
# indicates the Zontine variant) and iter.to.consensus (the number of
# iterations it took for the graph to reach uniformity; Inf if it never did in
# the required number of iterations.)

param.sweep <- function(results=NULL, num.trials=50, init.graph) {

    library(doParallel)
    registerDoParallel(60)

	data <- matrix(nrow=1,ncol=2)
	colnames(data) <- c("num","effectual")
        a <<- foreach(trial=1:num.trials, .combine=rbind) %dopar% {
			cat(">>>>>>>>>>>>>>>>>>>",trial,"\n")
		    graphs <- sim.opinion.dynamics(init.graph[[trial]], num.encounters=Inf,
                	encounter.func=get.graph.neighbors.encounter.func(1),
			        victim.update.function=get.automatically.update.victim.function(A.is.victim=FALSE),
              		edge.update.function=get.no.edge.update.function(),
			        verbose=TRUE,
                	termination.function=get.unanimity.termination.function(),
			choose.randomly.each.encounter=TRUE)

		data[trial, "num"] <- graph_attr(graphs[[1]][[length(graphs[[1]])]], "num.encounters")
		data[trial, "effectual"] <- graphs[[2]][length(graphs[[1]])]
		cat(graphs[[2]],"\n")
            	return(data)
        }

#		    num.iter.before.consensus <- 100
#                   for (iter in 1:length(graphs)) {
#                      if (length(unique(V(graphs[[iter]])$opinion)) == 1) {
#                         num.iter.before.consensus <- iter
#                        break
#                   }
#              }
#             return(data.frame(choose.randomly, num.iter.before.consensus))




#	theme_set(theme_bw(base_size=16) +
#  		theme(plot.title=element_text(family="Times")))

#    print(ggplot(results, aes(x=choose.randomly, y=num.iter.before.consensus,
#        fill=choose.randomly)) +
#        geom_boxplot(notch=FALSE) +
#        ggtitle(paste0("Choose random users each iteration?")) +
#        ylab("# of iterations to convergence") +
#        scale_fill_discrete(name="Models", breaks=c(TRUE,FALSE),
#            labels=c("Binary Voter","Davies-Zontine")))
    save(a, file="Box.RData")
    return(a)
}

set.seed(2222)

init <- vector("list",length=200)

for(i in 1:200){
    tester <- erdos.renyi.game(100,0.06)
    while(!is.connected(tester)){
        tester <- erdos.renyi.game(100,0.06)
    }
    init[[i]] <- tester
    V(init[[i]])$opinion <- sample(c(0,1),vcount(tester),replace=TRUE)
}


#param.sweep(num.trials=1, init.graph=init)
















yildiz.binary <- function(){
    # Yildoz, Acemoglu, et al. 2013
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

