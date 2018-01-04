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
data.sum(all_vt)
names(all_vt)

#############################################################################################################################
#############################################################################################################################
# new parms
drops<-c("vt_2017_stp", "vt_2016_stp", "vt_2017_kim", "vt_2015_was" ,"vt_2017_roc" ) # default =NULL
#drops<-NULL

yr1<-2017
yr2<-2016
yr3<-2015



library("sqldf") # does joins for gathering data
library("data.table") # data table for working with data
library("predictmeans") # for doing LSD tests
library("SpATS") # Does spatial anlysis by Splines
###################################
# DATA ANALYSIS
###################################

vt<-all_vt

# get the mean of trais of interest height by location, line and year
# summary valeus
summary<-vt[,.(
  yield_mean=mean(yield, na.rm=T), # mean of yield
  height_mean=mean(height, na.rm=T),# mean of yield
  log_sev=mean(log_sev, na.rm=T), # mean of lodging severity
  heading=mean(as.numeric(as.Date(heading_date, format = "%m/%d/%y") # heading days after planting
                          - as.Date(planting_date, format = "%m/%d/%y")),na.rm=T),
  test_weight=mean(test_weight, na.rm=T),# mean of test weight
  groat=mean(groat_percent, na.rm=T),# mean of graot%
  glucan= mean(groat_glucan, na.rm=T),# mean glucan by the groat
  grain_protein=mean(grain_protein, na.rm=T), # mean protein by NIR
  graot_protein=mean(groat_protein, na.rm=T), # mean protein by NIR  on groat
  groat_oil=mean(groat_fat, na.rm=T), # mean oil by NIR  on graot
  grain_oil=mean(grain_oil, na.rm=T),# mean oil by NIR
  cr_sev=mean(cr_sev, na.rm=T), # mean crown rust seveirty
  smut=mean(smut, na.rm=T), # mean sumt seveirty
  bydv=mean(bydv, na.rm=T) # mean BYDV sevieryt
),
by= c("line","location", "year")] # will give it by location and hyear



unique(vt$year)

# get list of lines by year
vt1_list<-unique(subset(summary, year == yr1, select =line))
vt2_list<-unique(subset(summary, year == yr2, select =line))
vt3_list<-unique(subset(summary, year == yr3, select =line))

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


# agrnomics table
###################################
agro_averages<-c("heading", "height_mean", "log_sev") #someting for the user to select
agro_averages<-c("line","location","year", agro_averages) #this get appended
agro<-summary[, ..agro_averages]
## combine all locations within a year
agro<-agro[,.(
  heading= mean(heading, na.rm=T),
  log= mean(log_sev, na.rm=T),
  height = mean (height_mean, na.rm=T)
),
by=c("line", "year") ]


# get lists
#####
# get by year
agro_1<-subset(agro, year ==yr1)
agro_2<-subset(agro, year ==yr2)
agro_3<-subset(agro, year ==yr3)

# join to yr1 list
agro<-sqldf("select * from agro_1
            left join agro_2 on agro_1.line = agro_2.line
            left join agro_3 on agro_1.line = agro_3.line")

agro<-agro[,-c(2,6,7,11,12)]

names(agro)<-c("line","heading_yr1", "log_yr1", "height_yr1",
               "heading_yr2", "log_yr2", "height_yr2",
               "heading_yr3", "log_yr3", "height_yr3")
#####

# get 1,2,3 yr avearges
#####
yr3_head<-rowMeans(agro[,c("heading_yr1", "heading_yr2", "heading_yr3")], na.rm=F)
yr2_head<-rowMeans(agro[,c("heading_yr1", "heading_yr3")], na.rm=F)
yr1_head<-agro[,c("heading_yr1")]

yr3_log<-rowMeans(agro[,c("log_yr1", "log_yr2", "log_yr3")], na.rm=F)
yr2_log<-rowMeans(agro[,c("log_yr1", "log_yr2")], na.rm=F)
yr1_log<-agro[,c("log_yr1")]

yr3_ht<-rowMeans(agro[,c("height_yr1", "height_yr2", "height_yr3")], na.rm=F)
yr2_ht<-rowMeans(agro[,c("height_yr1", "height_yr2")], na.rm=F)
yr1_ht<-agro[,c("height_yr1")]
#####

# final agro tables
agro_final_avgs<-cbind(vt1_list, yr1_head, yr2_head, yr3_head, yr1_log, yr2_log, yr3_log, yr1_ht, yr2_ht, yr3_ht)



# yield table
###################################

#data prep
#####
vt$rep<-as.factor(vt$rep)
vt$line<-as.factor(vt$line)
vt$location<-as.factor(vt$location)
vt$year<-as.factor(vt$year)
vt$trial_name<-as.factor(vt$trial_name)
vt$yield<-as.numeric(as.character(vt$yield))
#####

#years to drop
#####
vt2<- subset(vt, trial_obj == "agro") #drop all no agro trial_obj == agro
if(is.null(drops)) {vt2<-vt2} else {vt2<-subset(vt2, ! trial_name %in% drops )} # drop out trails to exclude
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
  data2<-subset(data, location %in% loc) # filter by location
  out<-aov(yield ~ line+rep  , data=data2) #aov
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
  names(final)[2]<-"yield"
  names(final)[3]<-"location"
  final
  boot<-rbind(boot, final)
}
yr1_yield<-boot
yr1_yield$location<-gsub("\\_.*","",yr1_yield$location)
rm(boot)
#####

# yr2's
#####

#2 years average by location

vt3<-subset(vt2, year %in% c(yr1, yr2))
sel<-aggregate(yield ~ location  + year + line  , data=vt3, FUN=mean, na.rm=F)
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
  out<-aov(yield ~ line+year  , data=data3) #aov
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
  names(final)[2]<-"yield"
  names(final)[3]<-"location"
  final
  boot<-rbind(boot, final)
}
yr2_yield<-boot
rm(boot)
#####


# yr3's
#3 years average by location

vt3<-subset(vt2, year %in% c(yr1, yr2, yr3))
sel<-aggregate(yield ~ location  + year + line  , data=vt3, FUN=mean, na.rm=F)
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
  out<-aov(yield ~ line+year  , data=data3) #aov
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
  names(final)[2]<-"yield"
  names(final)[3]<-"location"
  final
  boot<-rbind(boot, final)
}

yr3_yield<-boot

### finish yields




