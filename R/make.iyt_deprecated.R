#' make.iyt function
#' 
#' DEPRECATED. Please use the make.rcbd function or make.aibd function.
#' 
#' @param loc.to.use REQUIRED is the name of the location useing hte code naem ie "crk"
#' @param  loc.ids REQUIRED is a csv file with the first column being "loc.number"which is the zurn number for the location, the second column"location" is the full name of the locatoin, the third column has the codeded location name i.e. "crk" must match input to loc.to.use, the fourth column "beds" has the number of beds i.e. rows of that location
#' @param  trial.ids REQUIRED is a csv file with teh first column "trial.number" has the trial id's fromt he zunr number,second column has the coded trail name i.e. "VT"
#' @param  experiment REQUIRED is the name of the experiment you are making ie "VT" must match trail.ids coded name  
#' @param  entries REQUIRED is a csv file i.e.("18 VT 9jan18.csv") with one column "line" which has the list of the entries for the VT
#' @param  plot.start OPTIONAL the number of the first plot i.e. 0 or 1 or 1000 which ever, Default is 1
#' @param  number.blocks OPTIONALis the number of blocks to use i.e. 0 or 1 or 2 or 3. Defalut is 3
#' @param  year REQUIRED is the year of the planting i.e. 2018
#' @param zurn.seed REQUIRED is the zurn code for the crop i.e. 5 for oat or 3 for barley 
#' @param checks REQUIRED a csv file i.e.("18 VT 9jan18.csv") with one column "check" which has the list of the chekcs 
#' 
#' @examples 
#' 
#' @import plyr
#' @import stringr
#' 
#' @export


make.iyt <- function(
  loc.to.use=NULL,
  loc.ids=NULL, 
  trial.ids=NULL,
  experiment=NULL,
  entries= NULL, 
  plot.start=NULL, 
  number.blocks=NULL,
  num.beds=NULL,
  year=NULL, 
  zurn.seed=NULL, 
  checks =NULL, 
  num.reps.chk=NULL){
  #
  
  # from user inputs
  loc.to.use<-loc.to.use
  loc.ids<-read.csv(loc.ids)
  trial.ids<-read.csv(trial.ids)
  experiment<-experiment
  entries<-read.csv(entries)
  plot.start<-plot.start
  number.blocks<-number.blocks
  year<-year
  zurn.seed<-zurn.seed
  checks<-read.csv(checks)
  num.beds<-num.beds
  num.reps.chk<-num.reps.chk
  
  stop("This function is deprecated. Please use the 'make.aibd' or 'make.rcbd' functions instead.")
  
  
  
  #
  # deduced from user inputs
  location.list<-loc.ids$environment # will make a file for each locatio i nthe loc.ids file 
  #bed.list<-loc.ids$beds
  #bed.list<-num.beds
  
  
  ### testing if the number of checks was good between 10% and 25%
  # interval test function
  in_interval <- function(x, interval){ 
    stopifnot(length(interval) == 2L)
    interval[1] <= x & x <= interval[2]
  }

 # if(in_interval(dim(checks)[1], c(ceiling ((dim(entries)[1] *.1)/num.reps.chk), ceiling ((dim(entries)[1] *.25)/num.reps.chk))) == F ) {
  #  stop("suggested number of checks is ",ceiling ((dim(entries)[1] *.1)/num.reps.chk), " to ", ceiling ((dim(entries)[1] *.25)/num.reps.chk), 
  #       " change value of num.reps.chk or number of checks")}
  
  
  #checking if the checks are not a entry
  conflicts<-checks$line %in% entries$line
  conflict.chk<-checks$line [which(conflicts == TRUE)]
  if(any(conflicts == TRUE)) {warning("checks must not appear in the entry list, suggest add '_chk' to ", conflict.chk)}
  if(any(conflicts == TRUE)) {stop(" ")}
  #
    
  ###
  
  
  i<-which(location.list ==loc.to.use) # get postion of the name of the locatoin 
  enviro<-location.list[i] # geting the enviroemnt to use
  rcbd <- design.rcbd(enviro = location.list[i], exp.name = experiment, chks = checks, nChkReps =num.reps.chk,
                      nBlks = number.blocks, entries = entries, nChks = 0, nFieldRows = num.beds, 
                      plot.start = plot.start, fillWithEntry = FALSE, fillWithChk = TRUE)
  # makin the map file
  plot.lay<-rcbd$plot.layout
  plot.lay<-as.data.frame(plot.lay)
  rep.lay<-rcbd$rep.layout
  rep.lay<-as.data.frame(rep.lay)
  check.lay<-rcbd$check.layout
  check.lay<-as.data.frame(check.lay)
  map<-cbind(loc=rep(enviro, nrow(plot.lay)), plot.lay, rep.lay,check.lay )
  #
  #making the data file 
  dgn<-rcbd$rcbd.dg
  dgn$environment<-as.character(dgn$environment)
  dgn<-join(dgn, loc.ids, by= "environment")
  dgn$experiment<-rep(experiment, nrow(dgn))
  dgn<-join(dgn, trial.ids, by= "experiment")
  #makes the zurn code
  dgn$zurn<-paste((zurn.seed*100)+dgn$loc.number,formatC(dgn$trial.number, width=2, flag="0"), dgn$replication, str_pad(dgn$plot, 4, pad = 0), sep="")
  dgn <- within(dgn, zurn[replication == "FILLER"] <- NA) # if replication is FILLER than make zurn code NA
  #dgn <- within(dgn, replication[replication == "FILLER"] <- NA) # if replication is FILLER than make it NA
  #
  # making a plating order 
  junk<-dgn
  junk<-junk[,c("row", "column", "plant_order")]
  junk2<-reshape(junk, idvar = "row", timevar = "column", direction = "wide")
  junk2<-junk2[ order(-junk2[,1]), ]
  rm(junk)
  #
  map<-map[-nrow(map),]
  map2<-cbind(map, junk2)
  number_cols<- 1:length(unique(dgn$column))
  colnames(map2)<-c("loc","row",paste("column",number_cols,  sep="_"), "row", paste("column",number_cols,  sep="_"),"row", 
                    paste("column",number_cols,  sep="_"), "row", paste("column",number_cols,  sep="_") )
  
  #colnames(map2)<-c("loc","row",number_cols, "row", number_cols,"row", number_cols )
  map<-map2
  map2<-map
  map2[] <- lapply(map2, as.character)
  map2[nrow(map2) + 1,] = c(
    NA,NA,rep("plot_order", max(number_cols)),
    NA, rep("rep_order", max(number_cols)),
    NA, rep("check_order", max(number_cols)),
    NA, rep("plant_order", max(number_cols)))
  map<-map2
  rm(map2)
  #######################################################
   # house keeping making the files better to order
  dgn$trial<-paste(c(experiment,year,as.character(enviro[1])),collapse="_") # adding data based trail naems
  dgn<-dgn[,-c(2, 4, 12  ,13  ,14,   15 )] # droping unndded cols
  #dgn<-dgn[,-c(2, 4, 12  ,13  ,14,   15,  16 )] # droping unndded cols
  dgn<-dgn[ order(dgn$line_name,dgn$plant_order ) , ] # reorder for packing 


  
  return(list(data.book=dgn, map.file=map))
  
}


