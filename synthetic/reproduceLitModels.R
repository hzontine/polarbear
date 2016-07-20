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
binary.voter <- function(choose.randomly.each.encounter=FALSE, plot=TRUE) {
    #set.seed(111234)
    init <- get.plain.old.graph(opinion=rbinom(50,1,0.5), 
        probability.connected=0.2)
    graphs <<- sim.opinion.dynamics(init, num.encounters=100*vcount(init),
        encounter.func=get.graph.neighbors.encounter.func(1),
        victim.update.function=get.automatically.update.victim.function(
                                                            A.is.victim=TRUE), 
        choose.randomly.each.encounter=choose.randomly.each.encounter,
        termination.function=get.unanimity.termination.function(),
        verbose=FALSE)
    if (plot) {
        plot.animation(graphs, "opinion", delay.between.frames=.15)
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

param.sweep <- function(results=NULL, num.trials=50) {

    library(doParallel)
    registerDoParallel(50)

    #if (is.null(results)) {
    #    pair.of.result.sets <- 
    #}
    #i <- 0
    #init.graph <- erdos.renyi.game(100,0.05)
    #V(init.graph)$opinion <- sample(c(0,1),vcount(init.graph),replace=TRUE)
    final.result <<- lapply(c(TRUE,FALSE), function(choose.randomly) {
	#lapply(c(TRUE,FALSE), function(a.is.victim) {
        #list(list(1,FALSE), list(1,TRUE)), function(num.ver, a.is.victim) {
	    results <<- foreach(trial=1:num.trials, .combine=rbind) %dopar% {
		    #graphs <- sim.opinion.dynamics(init.graph, num.encounters=30000,
            #            encounter.func=get.graph.neighbors.encounter.func(1),
			#chose a.is.victim as true because it converges faster
			#victim.update.function=get.automatically.update.victim.function(A.is.victim=TRUE),
            #            edge.update.function=get.no.edge.update.function(),
			#verbose=TRUE,
			#chose choose.randomly.each.encounter=FALSE because it converges faster
            #            choose.randomly.each.encounter=choose.randomly)
                    cat("Trial #",trial,"....\n",sep="")
 		    graphs <- binary.voter(choose.randomly, plot=FALSE)                    
		    num.iter.before.consensus <- 100
                    for (iter in 1:length(graphs)) {
                        if (length(unique(V(graphs[[iter]])$opinion)) == 1) {
                            num.iter.before.consensus <- iter
                            break
                        }
                    }
                    return(data.frame(choose.randomly, num.iter.before.consensus))
           }
	       #save(results, file="randomlySweep.RData")
     })
        #results <- as.data.frame(
        #rbind(pair.of.result.sets[[1]],pair.of.result.sets[[2]]))
        #rownames(results) <- 1:nrow(results)
        #colnames(results) <- c("A.is.victim","iter.to.consensus")

    final.result <- rbind(final.result[[1]],final.result[[2]])
    save.image(print(ggplot(final.result, aes(x=choose.randomly, y=num.iter.before.consensus,
            fill=choose.randomly)) +
        geom_boxplot(notch=FALSE) +
        ggtitle(paste0("Choose random users each iteration?")) +
        ylab("# of iterations to convergence") +
        scale_fill_discrete(name="Models", breaks=c(TRUE,FALSE),
            labels=c("Binary Voter","Davies-Zontine"))), file="randBoxplot.png")
    save(final.result, file="randomlySweep.RData")
    return(final.result)
}

param.sweep(num.trials=50)

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

