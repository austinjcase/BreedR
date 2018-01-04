#' vt.report function
#'
#' This function allows you to pull data from the oat data base or similar
#' @param expt REQUIRED \code{data.frame} of experient descirptions in the OAT DATA BASE
#' @param field REQUIRED \code{data.frame} of field data in the OAT DATA BASE
#' @param post REQUIRED \code{data.frame} of post harvest data in the OAT DATA BASE
#' @param trail OPTIONAL\code{character string} a type of trial as described in the OAT DATA BASE
#'
#' @param trail OPTIONAL\code{character string} a type of trial as described in the OAT DATA BASE
#'
#'
#'
#' @keywords data.pull
#' @export
#' @examples
#' vt.report

data.pull <- function(experiment=NULL,field.pheno=NULL,
                      qual.pheno=NULL, trial.type =NULL,
                      drops=NULL, yr1=x, yr2=y,
                      yr3=z, trait = trait.data
                  )
{

library("BreedR")
library("sqldf") # does joins for gathering data
library("data.table") # data table for working with data
library("predictmeans") # for doing LSD tests

  all_vt<-data.pull(expt=experiment, field=field.pheno, post=qual.pheno, trial=trial.type)
  vt<-all_vt

# get list of lines by year
vt1_list<-unique(subset(vt, year == yr1, select =line))
vt2_list<-unique(subset(vt, year == yr2, select =line))
vt3_list<-unique(subset(vt, year == yr3, select =line))
vt2b_list<-Reduce(intersect, list(vt1_list$line,vt3_list$line)) #where 2b is yr1 and yr3 data
vt2b_list<-setDT(as.data.frame(vt2b_list))
names(vt2b_list)[1] <-"line"
vt2c_list<-Reduce(intersect, list(vt2_list$line,vt3_list$line)) #where 2c is yr2 and yr3 data
vt2c_list<-setDT(as.data.frame(vt2c_list))
names(vt2c_list)[1] <-"line"

# merge them to get 1yr 2yr and 3yr lines
vt_lists<-sqldf("select * from vt1_list
                left join vt2_list on vt1_list.line = vt2_list.line
                left join vt3_list on vt1_list.line = vt3_list.line")

## cbind ragged cbind fill function
cbind.fill<-function(...){
  nm <- list(...)
  nm<-lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
}

vt_lists<-cbind.fill(vt1_list, vt2_list, vt3_list, vt_lists, vt2b_list, vt2c_list) # combine them
colnames(vt_lists)<-c("list_1","list_2","list_3","yr1","yr2", "yr3", "yr2b", "yr2c") # rename them
vt_lists<-as.matrix(vt_lists)

#data prep
#####
vt<-all_vt[,1:13]
vt$year<-all_vt$year
vt$location<-all_vt$location
vt$trial_obj<-all_vt$trial_obj
vt$rep<-as.factor(vt$rep)
vt$line<-as.factor(vt$line)
vt$location<-as.factor(vt$location)
vt$year<-as.factor(vt$year)
vt$trial_name<-as.factor(vt$trial_name)
q<-which( colnames(all_vt)== trait)  ## this could work ....
vt$trait<-all_vt[,..q]
vt$trait<-as.numeric(as.character(vt$trait))
#####

#years to drop
#####
vt2<- subset(vt, trial_obj == "agro") #drop all no agro trial_obj == agro
if(is.null(drops)) {vt2<-vt2} else {vt2<-subset(vt2, ! trial_name %in% drops )} # drop out trails to exclude

#need to drop locatiosn with no data for the trait
#####
qx<-aggregate(trait ~ trial_name , data=vt2, FUN=mean, na.rm=F, na.omit =F) # calc mean by loc
ins<-qx$trial_name # get locs with data
qxy<-subset(vt2, trial_name %in% ins) #sub set the locatiosn tih data means
ins<-unique(qxy$trial_name) # list of locs with trait data
qxys<-subset(vt2, ! trial_name %in% ins) # locs with no trait data
outs<-unique(qxys$trial_name) # list of locs with no trait dat
outs<-as.data.frame((outs)) # bind
names(outs) [1]<-"trial_name" # processing
means.table<-rbind.fill(outs,qx) # gives means table by trait
names(means.table)[2]<-trait
vt2<-subset(vt2, trial_name %in% ins) # filter by locs with trait data

#####


# yr1's
#####
sel<-yr1 # year to select first year
data<-subset(vt2, year %in% sel) # filter by year
locs<-unique(data$location) # list of locaions matching year criteria
vt_lists<-as.data.frame(vt_lists)
lines1yr<- vt_lists$yr1 # list of the lines in both yr1
data2<-subset(data, line %in% lines1yr )
data2<-droplevels(data2)
data<-data2

# loop to get cv's
boot<-NULL # null boot vector to gather data
for (i in 1:length(locs)) {
  loc<-locs[i]
  data2<-subset(data, location %in% loc) # filter by location
  out<-aov(trait ~ line+rep  , data=data2) #aov
  means<-predictmeans(out, "line", plot=F, level=0.10) # get lsd and means p=.1
  lsd<-means$LSD [3] # get mean lsd
  f<-means$`Predicted Means`# get line means
  f<-as.data.frame(f)  # reformat as data frame
  q<-mean(f$Freq) # get overall mean
  final<-rbind(f, q, lsd) #bind lsd data and mean
  final$line<-as.character(final$line) # fromatting
  final$line[nrow(final)-1]<-"mean" # naming
  final$line[nrow(final)]<-"lsd0.1" # naming
  final<-cbind(final, rep(capture.output(cat(as.character(loc), sel, sep="_")), nrow(final)))
  names(final)[2]<-trait
  names(final)[3]<-"location"
  boot<-rbind(boot, final)
}
yr1_trait<-boot
yr1_trait$location<-gsub("\\_.*","",yr1_trait$location)
rm(boot)


## now get state wide for yr1
data2<-subset(data, location %in% locs) # get all yr1 locs
out<-aov(trait ~ line+rep  , data=data2) #aov
means<-predictmeans(out, "line", plot=F, level=0.10) # get lsd and means p=.1
lsd<-means$LSD [3] # get mean lsd
f<-means$`Predicted Means`# get line means
f<-as.data.frame(f)  # reformat as data frame
q<-mean(f$Freq) # get overall mean
final<-rbind(f, q, lsd) #bind lsd data and mean
final$line<-as.character(final$line) # fromatting
final$line[nrow(final)-1]<-"mean" # naming
final$line[nrow(final)]<-"lsd0.1" # naming
state<-cbind(final, "state")
names(state)[3]<-"location"
names(state)[2]<-trait
yr1_trait_state<-state

#####

# yr2's
#####
# so yr2  = yr1 and yr2 so 2017-2016
# or yr2b = yr1 and yr3 so 2017-2015
# or yr2c = yr2 and yr3 so 2016-2015

## yr2 regular
#2 years average by location
# so yr2  = yr1 and yr2 so 2017-2016

vt3<-subset(vt2, year %in% c(yr1, yr2))
sel<-aggregate(trait ~ location  + year + line+rep  , data=vt3, FUN=mean, na.rm=F)
# two years x locations
boot<-NULL # null boot vector to gather data
vt_lists<-as.data.frame(vt_lists)
lines2yr<- vt_lists$yr2 # list of the lines in both yr1 and yr2
data2<-subset(sel, line %in% lines2yr )
data2<-droplevels(data2)
locs<-table(data2$location, data2$year )
locs<-as.data.frame.matrix(locs)
row_sub = apply(locs, 1, function(row) all(row !=0 ))
locs<-locs[row_sub,]
locs<-row.names(locs)  # gets list of locatiosn with boty yr1 and yr2 data

for (i in 1:length(locs)) {
  loc<-locs[i]
  data3<-subset(data2, location %in% loc) # filter by location
  out<-aov(trait ~ line+ rep %in% year  , data=data3) #aov with rep nexted in year
  means<-predictmeans(out, "line", plot=F, level=0.10) # get lsd and means p=.1
  lsd<-means$LSD [3] # get mean lsd
  f<-means$`Predicted Means`# get line means
  f<-as.data.frame(f)  # reformat as data frame
  q<-mean(f$Freq) # get overall mean
  final<-rbind(f, q, lsd) #bind lsd data and mean
  final$line<-as.character(final$line) # fromatting
  final$line[nrow(final)-1]<-"mean" # naming
  final$line[nrow(final)]<-"lsd0.1" # naming
  final<-cbind(final, rep(loc, nrow(final)))
  names(final)[2]<-trait
  names(final)[3]<-"location"
  boot<-rbind(boot, final)
}
yr2_trait<-boot
rm(boot)

### now state wide yr1 and yr2
data3<-subset(data2, location %in% locs) # get all yr1 and 2  locs
out<-aov(trait ~ line + rep %in% location %in% year  , data=data3) #anova for rep in location in year
means<-predictmeans(out, "line", plot=F, level=0.10) # get lsd and means p=.1
lsd<-means$LSD [3] # get mean lsd
f<-means$`Predicted Means`# get line means
f<-as.data.frame(f)  # reformat as data frame
q<-mean(f$Freq) # get overall mean
final<-rbind(f, q, lsd) #bind lsd data and mean
final$line<-as.character(final$line) # fromatting
final$line[nrow(final)-1]<-"mean" # naming
final$line[nrow(final)]<-"lsd0.1" # naming
state<-cbind(final, "state")
names(state)[3]<-"location"
names(state)[2]<-trait
yr2_trait_state<-state
#####

## yr2b
#2 years average by location
# or yr2b = yr1 and yr3 so 2017-2015

vt3<-subset(vt2, year %in% c(yr1, yr3))
sel<-aggregate(trait ~ location  + year + line+rep  , data=vt3, FUN=mean, na.rm=F)
# two years x locations
boot<-NULL # null boot vector to gather data
vt_lists<-as.data.frame(vt_lists)
lines2yrb<- vt_lists$yr2b # list of the lines in both yr1 and yr3
data2<-subset(sel, line %in% lines2yrb )
data2<-droplevels(data2)
locs<-table(data2$location, data2$year )
locs<-as.data.frame.matrix(locs)
row_sub = apply(locs, 1, function(row) all(row !=0 ))
locs<-locs[row_sub,]
locs<-row.names(locs)  # gets list of locatiosn with boty yr1 and yr3 data

for (i in 1:length(locs)) {
  loc<-locs[i]
  data3<-subset(data2, location %in% loc) # filter by location
  out<-aov(trait ~ line + rep %in% year , data=data3) #aov
  means<-predictmeans(out, "line", plot=F, level=0.10) # get lsd and means p=.1
  lsd<-means$LSD [3] # get mean lsd
  f<-means$`Predicted Means`# get line means
  f<-as.data.frame(f)  # reformat as data frame
  q<-mean(f$Freq) # get overall mean
  final<-rbind(f, q, lsd) #bind lsd data and mean
  final$line<-as.character(final$line) # fromatting
  final$line[nrow(final)-1]<-"mean" # naming
  final$line[nrow(final)]<-"lsd0.1" # naming
  final<-cbind(final, rep(loc, nrow(final)))
  names(final)[2]<-trait
  names(final)[3]<-"location"
  boot<-rbind(boot, final)
}
yr2b_trait<-boot
rm(boot)

#####

## yr2c
#2 years average by location
# or yr2c = yr2 and yr3 so 2016-2015

vt3<-subset(vt2, year %in% c(yr2, yr3))
sel<-aggregate(trait ~ location  + year + line +rep  , data=vt3, FUN=mean, na.rm=F)
# two years x locations
boot<-NULL # null boot vector to gather data
vt_lists<-as.data.frame(vt_lists)
lines2yrc<- vt_lists$yr2c # list of the lines in both yr1 and yr3
data2<-subset(sel, line %in% lines2yrc )
data2<-droplevels(data2)
locs<-table(data2$location, data2$year )
locs<-as.data.frame.matrix(locs)
row_sub = apply(locs, 1, function(row) all(row !=0 ))
locs<-locs[row_sub,]
locs<-row.names(locs)  # gets list of locatiosn with boty yr1 and yr3 data

for (i in 1:length(locs)) {
  loc<-locs[i]
  data3<-subset(data2, location %in% loc) # filter by location
  out<-aov(trait ~ line + rep %in% year  , data=data3) #aov
  means<-predictmeans(out, "line", plot=F, level=0.10) # get lsd and means p=.1
  lsd<-means$LSD [3] # get mean lsd
  f<-means$`Predicted Means`# get line means
  f<-as.data.frame(f)  # reformat as data frame
  q<-mean(f$Freq) # get overall mean
  final<-rbind(f, q, lsd) #bind lsd data and mean
  final$line<-as.character(final$line) # fromatting
  final$line[nrow(final)-1]<-"mean" # naming
  final$line[nrow(final)]<-"lsd0.1" # naming
  final<-cbind(final, rep(loc, nrow(final)))
  names(final)[2]<-trait
  names(final)[3]<-"location"
  boot<-rbind(boot, final)
}

yr2c_trait<-boot
rm(boot)

#####


# yr3's
#3 years average by location

vt3<-subset(vt2, year %in% c(yr1, yr2, yr3))
sel<-aggregate(trait ~ location  + year + line +rep  , data=vt3, FUN=mean, na.rm=F)
# two years x locations
boot<-NULL # null boot vector to gather data
vt_lists<-as.data.frame(vt_lists)
lines3yr<- vt_lists$yr3 # list of the lines in both yr1 and yr2 and yr3
data2<-subset(sel, line %in% lines3yr )
data2<-droplevels(data2)
locs<-table(data2$location, data2$year )
locs<-as.data.frame.matrix(locs)
row_sub = apply(locs, 1, function(row) all(row !=0 ))
locs<-locs[row_sub,]
locs<-row.names(locs)  # gets list of locatiosn with boty yr1 and yr2 data

for (i in 1:length(locs)) {
  loc<-locs[i]
  data3<-subset(data2, location %in% loc) # filter by location
  out<-aov(trait ~ line + rep %in% year  , data=data3) #aov
  means<-predictmeans(out, "line", plot=F, level=0.10) # get lsd and means p=.1
  lsd<-means$LSD [3] # get mean lsd
  f<-means$`Predicted Means`# get line means
  f<-as.data.frame(f)  # reformat as data frame
  q<-mean(f$Freq) # get overall mean
  final<-rbind(f, q, lsd) #bind lsd data and mean
  final$line<-as.character(final$line) # fromatting
  final$line[nrow(final)-1]<-"mean" # naming
  final$line[nrow(final)]<-"lsd0.1" # naming
  final<-cbind(final, rep(loc, nrow(final)))
  names(final)[2]<-trait
  names(final)[3]<-"location"
  boot<-rbind(boot, final)
}

yr3_trait<-boot
rm(boot)

## get strate wide yr1 yr2 yr3

data3<-subset(data2, location %in% locs) # get all yr1 and 2  locs
out<-aov(trait ~ line + rep %in% location %in% year  , data=data3) #anova for rep in location in year
means<-predictmeans(out, "line", plot=F, level=0.10) # get lsd and means p=.1
lsd<-means$LSD [3] # get mean lsd
f<-means$`Predicted Means`# get line means
f<-as.data.frame(f)  # reformat as data frame
q<-mean(f$Freq) # get overall mean
final<-rbind(f, q, lsd) #bind lsd data and mean
final$line<-as.character(final$line) # fromatting
final$line[nrow(final)-1]<-"mean" # naming
final$line[nrow(final)]<-"lsd0.1" # naming
state<-cbind(final, "state")
names(state)[3]<-"location"
names(state)[2]<-trait
yr3_trait_state<-state

### finish traits

yr1_trait$set<-yr1
yr1_trait_state$set<-yr1

yr2_trait$set<-c("yr1_yr2")
yr2_trait$set<-gsub("yr1",yr1,yr2_trait$set)
yr2_trait$set<-gsub("yr2",yr2,yr2_trait$set)

yr2_trait_state$set<-c("yr1_yr2")
yr2_trait_state$set<-gsub("yr1",yr1,yr2_trait_state$set)
yr2_trait_state$set<-gsub("yr2",yr2,yr2_trait_state$set)

yr2b_trait$set<-c("yr1_yr3")
yr2b_trait$set<-gsub("yr1",yr1,yr2b_trait$set)
yr2b_trait$set<-gsub("yr3",yr3,yr2b_trait$set)

yr2c_trait$set<-c("yr2_yr3")
yr2c_trait$set<-gsub("yr2",yr2,yr2c_trait$set)
yr2c_trait$set<-gsub("yr3",yr3,yr2c_trait$set)

yr3_trait$set<-c("yr1_yr2_yr3")
yr3_trait$set<-gsub("yr1",yr1,yr3_trait$set)
yr3_trait$set<-gsub("yr2",yr2,yr3_trait$set)
yr3_trait$set<-gsub("yr3",yr3,yr3_trait$set)

yr3_trait_state$set<-c("yr1_yr2_yr3")
yr3_trait_state$set<-gsub("yr1",yr1,yr3_trait_state$set)
yr3_trait_state$set<-gsub("yr2",yr2,yr3_trait_state$set)
yr3_trait_state$set<-gsub("yr3",yr3,yr3_trait_state$set)

final_trait<-rbind(yr1_trait, yr1_trait_state,yr2_trait, yr2_trait_state, yr2b_trait, yr2c_trait, yr3_trait, yr3_trait_state)

result.inc.table<-(as.data.frame.matrix(with(final_trait, table(set, location))))
result.inc.table<-as.data.frame(result.inc.table)
result.inc.table$years<-row.names(result.inc.table)


data.inc.table<-(as.data.frame.matrix(with(vt2, table(year, location))))
data.inc.table<-as.data.frame(data.inc.table)
data.inc.table$years<-row.names(data.inc.table)

# return
#means.table
#data.inc.table
#result.inc.table
#final_trait

###############

## not sure hwo tot get it to return data now? does this work?
return(list(means.table , data.inc.table , result.inc.table , final_trait))

}




