# Stephen and Hannah
# We predict the nodes will converge into clusters of similar opinions.

library(igraph)
source("synthetic.R")
source("plotting.R")

# Run an opinion dynamics simulation with n agents for num.iter iterations.
# Returns a list of igraph objects, each representing the graph at a snapshot 
# in time.
#
# Agents have univariate opinions, which could be binary or weighted.
#
# The probability of one agent succesfully influencing another to change their 
# opinion is fixed, and not based on a homophilic threshold.
#
# init.opinions -- a vector of n nodes (n is up to the caller) with the inital
# opinions of each of the nodes. If binary=TRUE, these should be integer 
# values = 0 or = 1. If binary=FALSE, they should be reals on [0,1].
#
# num.nodes -- number of vertices in the graph
#
# num.iter -- the number of iterations to run the simulation
# 
# binary -- are opinions catergorical with two possible values (TRUE), or reals 
# on [0,1] (FALSE)?
# 
# calculate.neighbors -- a function which will take a graph, vertex ID and 
# a N value. Returns a vector of N degree (default = 1) neighboring vertex IDs 
# that vector will encounter in the current iteration.
# 
# degree -- the degree to which the encounter.func will search for neighbors
#
# prob.convert -- when an encounter between heterogeneous agents occurs, the
# probability that one agent changes their opinion to match the other.
#
# prob.connected -- the probability of edges between nodes on the initial graph
#
sim.clustering <- function(num.nodes=50,
	num.iter=20,
	binary=TRUE,
	prob.convert=0.3,
	degree=1,
	prob.connected=0.05) {

    init.opinions=sample(c(0,1),num.nodes,replace=TRUE)

    if(binary && any(!init.opinions %in% c(0,1))){
	stop("Initial Opinions are not all binary")
    }

    if(!binary && any(init.opinions < 0 | init.opinions > 1)){
	stop("Inital Opinions are not all in range")
    }
    graphs <- list(length=num.iter)
    graphs[[1]] <- erdos.renyi.game(length(init.opinions), prob.connected)
    V(graphs[[1]])$opinion <- init.opinions

    # For each iteration of the simulation...
    for (i in 2:num.iter){
	cat("Iteration ",i," of ",num.iter,"...\n",sep="")
	
	# Create new graph object to represent this point in time
	graphs[[i]] <- graphs[[i-1]]
		
	# Go through all the vertices, in random order:
  	    for (v in sample(1:gorder(graphs[[i]]))) {
		# A vector that holds all the neighbors of the current node
		# that are between one and X degrees away.
		neighboring.nodes <- calculate.neighbors(graphs[[i]], v, degree)

		for (neigh in neighboring.nodes) {
		    if (binary) {
			# If they already have the same opinion, cool!
			# If they don't, roll dice to see whether they
			# influence, and if so, choose a random one
			# to influence the other.
			if(V(graphs[[i]])[v]$opinion !=
			   V(graphs[[i]])[neigh]$opinion) {
				if(runif(1) < prob.convert){
				    convert <- runif(1) < .5
					if(convert){
	  				    V(graphs[[i]])[v]$opinion <-
			 		    V(graphs[[i]])[neigh]$opinion
					} else {
					    V(graphs[[i]])[neigh]$opinion <-
					    V(graphs[[i]])[v]$opinion
					}
				}
			}
		     }else{
			stop("non-binary not supported yet")
		     }
		  }
	    	}
    }
    return(graphs)
}

calculate.neighbors <- function(graphs, v, N){
	return(neighbors(graphs, V(graphs)[v], mode=N))
}

main <- function() {
	graph.sim <<- sim.clustering(num.iter=10)
	plot.animation(graph.sim, "opinion", interactive=FALSE, delay.between.frames=.2, 
		animation.filename="clustering.gif")
	plot.binary.opinions(graph.sim)
}
