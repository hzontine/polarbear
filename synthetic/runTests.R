
source("munge.R")

print("Selection method t.test:")
print(t.test(A$num.iter,B$num.iter))
cat("Stdevs: A=",sd(A$num.iter)," B=",sd(B$num.iter),"\n")
readline("Press ENTER.")
print("Influence direction t.test:")
print(t.test(B$num.iter,D$num.iter))
cat("Stdevs: B=",sd(B$num.iter)," D=",sd(D$num.iter),"\n")
