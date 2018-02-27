#' rep by rep function
#' 
#' This function allows you to make vt trials
#' @description this makes rep by rep line ups
#' @param data REQUIRED data.frame to work with
#' @param  ids REQUIRED ids of lines to aggreage by i.e. by plot, , must match column names 
#' @param  plot REQUIRED  names of the things to aggreagate i.e. plot, must match column names 
#' @examples 
#' 
#' dfa<-data.frame(lines=c(rep(c("DEON", "SABER", "SHELBY"), 2),"DEON"),
#' rep=c(1,1,1,2,2,2,2),
#' plots=(1:7))
#'
#' out<-rep.by.rep(data = dfa, ids = "lines", plot = "plots" )
#' out
#' @export
#' rep.by.rep


rep.by.rep<-function (
  data = NULL,
  ids = NULL,
  plot = NULL
){
  library(plyr)
  df<-data # data file with plots and line names
  df<-as.data.frame(df)
  df[,ids]<-as.character(df[,ids])
  list<-unique(df[,ids]) # list of lines
  boot<-NULL
  for(i in 1:length(list)){
  y<-subset(df, df[,ids] == list[i] ) 
    w<-y[,plot]
    w<-c(list[i], w)
    w<-t(w)
    w<-as.data.frame(w)
    boot<-as.data.frame(boot)
    boot<-rbind.fill(boot,w) # bind out 
   rm(y)
  rm(w)
  }
return(boot)
}
