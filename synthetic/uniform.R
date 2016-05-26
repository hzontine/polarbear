# Binary Ideology: 0 or 1
# With each random interaction, there is a probability of an agent flipping 
# ideologies to attractor agent's opinion.

library(igraph)
set.seed(1234)

binary.ideologies <- function(num.nodes=50, ideo.prob=.45, iter=40, encounters.per.iter=3, probInitialEdge=0.35){
	nodes=rbinom(num.nodes, 1, ideo.prob)
	graphs <- list(length=iter)
	graphs[[1]] <- sample_gnp(length(nodes), probInitialEdge)
	V(graphs[[1]])$ideology = nodes
	for (i in 2:iter){
		cat ("On iteration ",i,"...\n")
		graphs[[i]] = graphs[[i-1]]
		for (v in 1:gorder(graphs[[i]])){
			vertices = sample(1:gorder(graphs[[i]]),encounters.per.iter) 	
			vert <- vertices[vertices != v]
			for(b in vert){
				ideo = V(graphs[[i]])[b]$ideology
				my.ideo = V(graphs[[i]])[v]$ideology
				diff = my.ideo - ideo
                if(ideo != my.ideo){
                    x = rbinom(1,1,0.5)        # if there will be influencing
                        if ( x == 0 ) {
                            y = rbinom(1,1,0.5)     # who will influence who
                                if ( y == 0 ){  	# v influences b
                                    if (diff == 1){
                                        V(graphs[[i]])[b]$ideology <- 1
                                    }else{  if (diff == -1){
                                        V(graphs[[i]])[b]$ideology <- 0
                                    }}
                                }else{ 			# b influences v
                                    if (diff == 1){
                                        V(graphs[[i]])[v]$ideology <- 0
                                    }else{ if (diff == -1){
                                        V(graphs[[i]])[v]$ideology <- 1
                                    }}
                                }
                        }
                }
			}
		}
	}
}

plot.graphs <- function(graphs){
    assortativities <- sapply(graphs, function(graph) {
        assortativity(graph,types1=V(graph)$ideology)
    })
    plot(1:length(graphs),assortativities, type="l",ylim=c(-1,1),
        main="Polarization over time", xlab="time (iteration)",
        ylab="Assortativity of ideology")
}

main <- function(){
	binary.graphs <<- binary.ideologies(iter=20)
	plot.graphs(binary.graphs)
}
