
load("Box.RData")
A <- data.frame(num.iter=A.three, selection="with", direction="neighbor")
B <- data.frame(num.iter=B.three, selection="without", direction="neighbor")
C <- data.frame(num.iter=C.three, selection="with", direction="node")
D <- data.frame(num.iter=D.three, selection="without", direction="node")

data <- rbind(A,B,C,D)

