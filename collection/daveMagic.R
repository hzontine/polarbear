
# Random stuff from Reading Group 6/8/16, of which Stephen understands about
# half.
#
# Dave says "u is encoding incoming information, and v is encoding outgoing
# information."
# The thing that's defining our colors is outgoing information. Hence, it
# makes sense that v is the thing that contains information relevant to the
# colors.

library(rARPACK)
library(igraph)
#library(rgl)

# comstock.wittman

colors <- rep("black",vcount(comstock.wittman))
peeps <- ego(comstock.wittman,1,c(250,50),mode="out",mindist=1)
comstock.followers <- peeps[[1]]
wittman.followers <- peeps[[2]]
both.followers <- intersect(comstock.followers, wittman.followers)
colors[comstock.followers] <- "blue"
colors[wittman.followers] <- "red"
colors[both.followers] <- "green"
sizes <- rep(4,vcount(comstock.wittman))
sizes[250] <- 3 
sizes[50] <- 3 
pchs <- rep(".",vcount(comstock.wittman))
pchs[250] <- "C"
pchs[50] <- "W"

A <- as_adjacency_matrix(comstock.wittman)    # or whatever the graph is called
s <- svds(A,k=4)
par(mfrow=c(1,2))
plot(s$u[colors=="red",c(1,2)],pch=20,xlab="",ylab="",col="red",main="u",xlim=c(0.0,0.03),ylim=c(-.05,.05))
plot(s$v[colors=="red",c(1,2)],pch=20,xlab="",ylab="",col="red",main="v",xlim=c(0.0,0.03),ylim=c(-.05,.05))
#plot(s$u[,c(1,2)],pch=20,xlab="",ylab="",col=alpha(colors,.1),cex=sizes,main="u")
#plot(s$v[,c(1,2)],pch=20,xlab="",ylab="",col=alpha(colors,.1),cex=sizes,main="v")
#pairs(s$u,pch=20,col=alpha(colors,.1))
#plot3d(s$v[,1:3],pch=20,xlab="",ylab="",col=colors)

out <- knn.cv(s$u[,3:5],as.factor(colors),k=31)
print(table(out,colors))
cat("Error rate: ",sum(out!=colors)/length(colors),"\n",sep="")
