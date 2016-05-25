# Binary Ideology: 0 or 1
# With each random interaction, there is a probability of an agent flipping 
# ideologies to attractor agent's opinion.



library(igraph)

set.seed(1234)

bin <- function(nodes=rbinom(num.nodes=50, 1, ideo.prob=.45), iter=40,
	encounters.per.iter=3){
	plots <- list(length=iter)
	plots[[1]] <- sample_gnp(length(nodes), probInitialEdge=0.35)
	V(plots[[1]]$ideology) = nodes
	for (i in 2:iter){
		cat ("On iteration ",i,"...\n")
		plots[[i]] = plots[[i-1]]
		for (v in 1:gorder(plots[[i]])){
			vertices = sample(1:gorder(plots[[i]],encounters.per.iter) 	
			vertices = vertices[vertices != v]
			ideo = V(plots[[i]])[vertices]$ideology
			diff = abs(ideo - V(plots[[i]])[v]$ideology)
			if(V(plots[[i]])[v]$ideology != ideo[vals[s]]){
			     if(rbinom(1,1,probConvert=0.5) == 1){
				#Probability who will influence who
				#	plots[[i]][v, verticies[diff == 1]] <- 1
			     }
			}
		
		}
	}
}

main <- function(graphs){
    assortativities <- sapply(graphs, function(graph) {
        assortativity(graph,types1=V(graph)$ideology)
    })
    plot(1:length(graphs),assortativities, type="l",ylim=c(-1,1),
        main="Polarization over time", xlab="time (iteration)",
        ylab="Assortativity of ideology")
}
}
