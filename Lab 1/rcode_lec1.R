rm(list=ls())
# remove all objects
set.seed(123)
# initialize random number generator (to reproduce results)
treatments<-rep(c("A", "B"), 15)
# assign groups of even and odd numbers
treatment.ord <-sample(treatments)
# permutate rats randomly
treatment.ord

# four treatments, unbalanced design with 20 units
treatments<- sample(c("A", "B", "C", "D"), size=20, replace = TRUE)
treatment.ord<-sample(treatments) ## random permutation
treatment.ord
table(treatments) # gives group sizes for each treatment

# four treatments, balanced design with 20 units
treatments<- rep(c("A", "B", "C", "D"), each = 5)
treatment.ord<-sample(treatments) ## random permutation
treatment.ord
table(treatments) # gives group sizes for each treatment