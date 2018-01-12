setwd("/Volumes/CFANS/AGRO/Oat_Lab/R CODES for planting/2018/")# set working directory 

# from user inputs
loc.to.use<-"mos"
loc.ids<-"loc ids.csv"
trial.ids<-"trial ids.csv"
experiment<-"VT"
entries<-"18 VT 9jan18.csv"
plot.start<-0
number.blocks<-3
year<-2018
#




######################################################
############ DO NOT EDIT IN THIS BOX##################

# from user inputs
loc.to.use<-loc.to.use
loc.ids<-read.csv(loc.ids)
trial.ids<-read.csv(trial.ids)
experiment<-experiment
entries<-read.csv(entries)
plot.start<-plot.start
number.blocks<-number.blocks
year<-year

#
# deduced from user inputs
location.list<-loc.ids$environment # will make a file for each locatio i nthe loc.ids file 
bed.list<-loc.ids$beds
i<-which(location.list ==loc.to.use) # get postion of the name of the locatoin 
enviro<-location.list[i] # geting the enviroemnt to use

rcbd <- design.rcbd(enviro = location.list[i], exp.name = experiment, nChkReps =0,
                    nBlks = 3, entries = entries, nChks = 0, nFieldRows = bed.list[i], 
                    plot.start = plot.start, fillWithEntry = TRUE, fillWithChk = FALSE)

# makin the map file
plot.lay<-rcbd$plot.layout
plot.lay<-as.data.frame(plot.lay)
#plot.lay$loc<-rep(enviro, nrow(plot.lay))
rep.lay<-rcbd$rep.layout
rep.lay<-as.data.frame(rep.lay)
#rep.lay$loc<-rep(enviro, nrow(rep.lay))
map<-cbind(loc=rep(enviro, nrow(plot.lay)), plot.lay, rep.lay)
map
#
#
#making the data file 
dgn<-rcbd$rcbd.dg
dgn$environment<-as.character(dgn$environment)
dgn<-join(dgn, loc.ids, by= "environment")
dgn$experiment<-rep(experiment, nrow(dgn))
dgn<-join(dgn, trial.ids, by= "experiment")
#makes the zurn code
dgn$zurn<-paste(500+dgn$loc.number,formatC(dgn$trial.number, width=2, flag="0"), dgn$replication, str_pad(dgn$plot, 4, pad = 0), sep="")
dgn <- within(dgn, zurn[replication == "FILLER"] <- NA) # if replication is FILLER than make zurn code NA
#dgn <-dgn[ order(dgn$line_name) , ]
#dgn<-dgn[ order(dgn$line_name,dgn$plant_order ) , ]
#

# making a plating order 
junk<-dgn
junk<-junk[,c("row", "column", "plant_order")]
junk2<-reshape(junk, idvar = "row", timevar = "column", direction = "wide")
junk2<-junk2[ order(-junk2[,1]), ]
head(junk2)
rm(junk)

##
map<-map[-nrow(map),]
map2<-cbind(map, junk2)
number_cols<- 1:length(unique(dgn$column))
colnames(map2)<-c("loc","row",number_cols, "row", number_cols,"row", number_cols )
map<-map2
map2<-map
map2[] <- lapply(map2, as.character)
map2[nrow(map2) + 1,] = c(
  NA,NA,rep("plot_order", max(number_cols)),
  NA, rep("rep_order", max(number_cols)),
  NA, rep("plant_order", max(number_cols)))
map<-map2
rm(map2)
#######################################################

dgn$trial<-paste(c(experiment,year,as.character(enviro[1])),collapse="_")
dgn<-dgn[,-c(2, 4, 11, 13,14,15, 16, 17 )]
head(dgn)

map


#write.csv(dgn, paste(enviro, experiment, "planting.file.csv", sep = "_"), row.names=F) # plating output file
#write.csv(map, paste(enviro, experiment, "planting.layout.file.csv", sep = "_"), row.names=F) # plating map file output

