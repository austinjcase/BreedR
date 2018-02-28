
library(BreedR)
# set wroking directory
setwd("/Users/case0197/Documents/git_BreedR/exmple data")

#downloaodexample data
write.csv(read.table("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/rep.by.pre.txt", header=T), "rep.by.rep.csv", row.names =F)

x<-read.table("rep.by.rep.txt", head=T)
x
out<-rep.by.rep(data = dfa, ids = "lines", plot = "plots" )
out
write.csv(out, "data.csv")
