# Binary Ideology: 0 or 1
# With each random interaction, there is a probability of an agent flipping 
# ideologies to attractor agent's opinion.

library(igraph)
set.seed(1234)

binary.ideologies <- function(num.nodes=50, ideo.prob=.45, iter=40, encounters.per.iter=3, probInitialEdge=0.35){
	nodes=rbinom(num.nodes, 1, ideo.prob)
	plots <- list(length=iter)
	plots[[1]] <- sample_gnp(length(nodes), probInitialEdge)
	V(plots[[1]]$ideology) = nodes
	for (i in 2:iter){
		cat ("On iteration ",i,"...\n")
		plots[[i]] = plots[[i-1]]
		for (v in 1:gorder(plots[[i]])){
			vertices = sample(1:gorder(plots[[i]],encounters.per.iter)) 	
			vert <- vertices[vertices != v]
			ideo = V(plots[[i]])[vert]$ideology
			diff = ideo - V(plots[[i]])[v]$ideology

			# for loop ?     can't compare one ideo with many ideos
			if(V(plots[[i]])[v]$ideology != V(plots[[i]])[vert]$ideology){
			     x = floor(runif(1,min=1,max=5))
			     if(x == 1){ # v converts vert
				plots[[i]][v, vert[diff == 1]] <- 1
				plots[[i]][v, vert[diff == -1]] <- 0
			     }
			     if(x == 2){ # vert convert v
				plots[[i]][v, vert[diff == 1]] <- 0
				plots[[i]][v, vert[diff == -1]] <- 1
			     }
			     # if x == 3 or 4, no converting
			}
		}
	}
}

plot.graph <- function(graphs){
    assortativities <- sapply(graphs, function(graph) {
        assortativity(graph,types1=V(graph)$ideology)
    })
    plot(1:length(graphs),assortativities, type="l",ylim=c(-1,1),
        main="Polarization over time", xlab="time (iteration)",
        ylab="Assortativity of ideology")
}

main <- function(){
	binary.graph <- binary.ideologies()
	plot.graph(binary.graph)
}
