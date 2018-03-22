
library(BreedR)
# set wroking directory
setwd("/Users/case0197/Documents/git_BreedR/exmple data")

#downloaodexample data
write.csv(read.table("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/rep.by.rep.txt", header=T), "rep.by.rep.csv", row.names =F)

rep.rep<-read.csv("rep.by.rep.txt", head=T)
rep.rep

out<-rep.by.rep(data = rep.rep, ids = "lines", plot = "plots" )
out
write.csv(out, "rep.rep.data.csv")
