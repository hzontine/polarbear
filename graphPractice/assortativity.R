
# Run this code, look at the graph, and inspect the contents of students.csv.
# Then ask yourself: of gender, major, and hairLength, which attribute do you
# think will have the highest assortativity? Which do you think will have the
# lowest?

library(igraph)

students.df <- read.csv("students.csv",header=TRUE,stringsAsFactors=FALSE)

friends.df <- read.csv("friends.csv",header=FALSE,stringsAsFactors=FALSE)

friends <- graph.data.frame(friends.df, directed=FALSE, vertices=students.df)

plot(friends, 
    vertex.size=V(friends)$hairLength * 4, 
    vertex.color=ifelse(V(friends)$gender=="F","pink","lightblue"),
    vertex.shape=ifelse(V(friends)$major=="CPSC","circle",
        ifelse(V(friends)$major=="MATH","square","rectangle")))


# Uncomment this once you think you have the answer. :)
# cat("hair length assortativity =", assortativity(friends, types1=V(friends)$hairLength),"\n")
# cat("gender assortativity =", assortativity.nominal(friends, types=as.factor(V(friends)$gender)),"\n")
# cat("major assortativity =", assortativity.nominal(friends, types=as.factor(V(friends)$major)),"\n")
