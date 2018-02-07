
# rep by rep generator

# example data

df<-data.frame(line=c(rep(c("DEON", "SABER", "SHELBY"), 2),"DEON"),
rep=c(1,1,1,2,2,2,2),
plot=(1:7))
df$line<-as.character(df$line)
#

library(plyr)
# user inputs
ids<-"line" # name of genotypes id
plots<-"plot" # name of the plots to aggergate by 
#
list<-unique(df[,ids]) # list of lines
boot<-NULL
#
for(i in 1:length(list)){
y<-subset(df, line == list[i]) %>% t() # subset by fist line
x<-c(list[i], as.numeric(y[plots,])) # get the list of plots
x<-t(as.data.frame(x)) # turn it 
x<-as.data.frame(x) # make it a data frame
boot<-rbind.fill(boot,x) # bind out 
}
boot
df
