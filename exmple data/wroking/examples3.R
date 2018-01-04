library(BreedR)


###################################
# laoding the raw files

exp<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/expt_7dec17.csv", head=T,stringsAsFactors = F) # experiemntial description file
fld<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/field_7dec17.csv", head=T, stringsAsFactors = F) # field phnoetype  file
qual<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/post_harvest_7dec17.csv", head=T, stringsAsFactors = F) # quality phnoetype  file
lines<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/Line_attributes_7dec17.csv", head=T, stringsAsFactors = F) # line attibutes  file

###################################
# make data files

all_vt<-data.pull(expt=exp, field=fld, post=qual, trial="vt")
data.sum(all_vt, by.loc =T)
names(all_vt)

#############################################################################################################################
#############################################################################################################################
# new parms
#drops<-c("vt_2017_stp", "vt_2016_stp", "vt_2017_kim", "vt_2015_was" ,"vt_2017_roc" ) # default =NULL
drops<-NULL

yr1<-2017
yr2<-2016
yr3<-2015

trait<-"yield" # required trait to work on defined by user
#trait<-"test_weight" # required trait to work on defined by user


library("sqldf") # does joins for gathering data
library("data.table") # data table for working with data
library("predictmeans") # for doing LSD tests
library("SpATS") # Does spatial anlysis by Splines
###################################
# DATA ANALYSIS
###################################

vt<-all_vt

# get list of lines by year
vt1_list<-unique(subset(vt, year == yr1, select =line))
vt2_list<-unique(subset(vt, year == yr2, select =line))
vt3_list<-unique(subset(vt, year == yr3, select =line))

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

vt_lists<-cbind.fill(vt1_list, vt2_list, vt3_list, vt_lists) # combine them
cbind.fill(vt1_list, vt2_list, vt3_list, vt_lists)
colnames(vt_lists)<-c("list_1","list_2","list_3","yr1","yr2", "yr3") # rename them



# yield table
###################################

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

#####
#need to drop locatiosn with no data for the trait
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

## preping years to get the cv's
# data preps

# yr1's
#####
sel<-yr1 # year to select first year
data<-subset(vt2, year %in% sel) # filter by year
locs<-unique(data$location) # list of locaions matching year criteria
# loop to get cv's
boot<-NULL # null boot vector to gather data
for (i in 1:length(locs)) {
  loc<-locs[i]
  out<-aov(trait ~ line+rep  , data=data) #aov
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
#####



# yr2's
#####

#2 years average by location

vt3<-subset(vt2, year %in% c(yr1, yr2))
sel<-aggregate(trait ~ location  + year + line  , data=vt3, FUN=mean, na.rm=F)
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
  out<-aov(trait ~ line+year  , data=data3) #aov
  str(data3)
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
  final
  boot<-rbind(boot, final)
}
yr2_trait<-boot
rm(boot)
#####



# yr3's
#3 years average by location

vt3<-subset(vt2, year %in% c(yr1, yr2, yr3))
sel<-aggregate(trait ~ location  + year + line  , data=vt3, FUN=mean, na.rm=F)
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
  out<-aov(trait ~ line+year  , data=data3) #aov
  str(data3)
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
  final
  boot<-rbind(boot, final)
}

yr3_trait<-boot
yr3_trait
### finish traits

data.sum(all_vt, by.loc=T)

unique(yr3_trait$location)
unique(yr2_trait$location)
unique(yr1_trait$location)

yr1_trait$set<-yr1
head(yr1_trait)

yr2_trait$set<-c("yr1_yr2")
yr2_trait$set<-gsub("yr1",yr1,yr2_trait$set)
yr2_trait$set<-gsub("yr2",yr2,yr2_trait$set)
head(yr2_trait)


head(yr3_trait)
yr3_trait$set<-c("yr1_yr2_yr3")
head(yr3_trait)
yr3_trait$set<-gsub("yr1",yr1,yr3_trait$set)
yr3_trait$set<-gsub("yr2",yr2,yr3_trait$set)
yr3_trait$set<-gsub("yr3",yr3,yr3_trait$set)
head(yr3_trait)

final_trait<-rbind(yr1_trait, yr2_trait, yr3_trait)
head(final_trait)

line.inc.table<-(as.data.frame.matrix(with(final_trait, table(set, location))))
#names(line.inc.table)[c(2:3)] <-c("years", "numer_lines")
line.inc.table<-as.data.frame(line.inc.table)
names(line.inc.table)

with(final_trait, table(location, set))
with(final_trait, table(set, location))

# 2yr means could be yr1 and yr2 which is done
# so yr2  = yr1 and yr2 so 2017-2016
# or yr2b = yr1 and yr3 so 2017-2015
# or yr2c = yr2 and yr3 so 2016-2015

means.table

