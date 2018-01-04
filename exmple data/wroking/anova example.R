#remove.packages(BreedR)

library(BreedR)
library(plyr)
getwd()

###################################
# laoding the raw files

exp.data<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/expt_7dec17.csv", head=T,stringsAsFactors = F) # experiemntial description file
fld.data<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/field_7dec17.csv", head=T, stringsAsFactors = F) # field phnoetype  file
qual.data<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/post_harvest_7dec17.csv", head=T, stringsAsFactors = F) # quality phnoetype  file
lines.data<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/Line_attributes_7dec17.csv", head=T, stringsAsFactors = F) # line attibutes  file

###################################
# make data files
all_vt<-data.pull(expt = exp.data, field = fld.data, qual = qual.data, trial = "vt",
          years = c(2017, 2016, 2015))

#get agro trials
all_vt<-subset(all_vt, trial_obj == "agro")
table(all_vt$trial_name)

#rework facotrs
vt<-all_vt
vt$trial_name<-as.factor(vt$trial_name)
vt$rep<-as.factor(vt$rep)
vt$line<-as.factor(vt$line)
vt$location<-as.factor(vt$location)
vt$year<-as.factor(vt$year)
vt$trial_name<-as.factor(vt$trial_name)
vt$yield<-as.numeric(as.character(vt$yield))


#drop locs with no data
qx<-aggregate(yield ~ trial_name , data=vt, FUN=mean, na.rm=F, na.omit =F) # calc mean by loc
ins<-qx$trial_name # get locs with data
qxy<-subset(vt, trial_name %in% ins) #sub set the locatiosn tih data means
ins<-unique(qxy$trial_name) # list of locs with trait data
qxys<-subset(vt, ! trial_name %in% ins) # locs with no trait data
outs<-unique(qxys$trial_name) # list of locs with no trait dat
outs<-as.data.frame((outs)) # bind
names(outs) [1]<-"trial_name" # processing
means.table<-rbind.fill(outs,qx) # gives means table by trait
#names(means.table)[2]<-trait
vt1<-subset(vt, trial_name %in% ins) # filter by locs with trait data
class(vt1)

## drop all rows with 0 values .. # this shuld be an option (option value for replacing nonsnecial valeus)
## same sould go for if bigger than...
vt1[yield < 1, yield := NA] # repalce if less than 1
vt1[yield > 1000, yield := NA] # replace if greater than 1000
hist(vt1$yield)
max(vt1$yield, na.rm=T)
min(vt1$yield, na.rm=T)

#single years
#####
sel<-2017

data<-subset(vt1, year %in% sel) # filter by year
locs<-unique(data$location) # list of locaions matching year criteria

i<-4
  loc<-locs[i]
  loc
  data2<-subset(data, location %in% loc) # filter by location
  table(data2$line)

  # get average of line by rep
  data3<-aggregate(yield ~ line +rep,FUN=mean, data=data2, na.rm=F, na.omit =F) #average by rep if more than one per rep
  data3<-droplevels(data3)
  table(data3$line)
  str(data3)
  out<-lm(yield ~ line + rep   , data=data3) #aov
  anova<-anova(out)
  anova

  ## to get lsd values

  ##
  qnorm() *MSE * squr(2/3)  # sqrt(1/reps + 1/reps)

  qnorm(p=0.975) * sqrt((69.169*((1/3) + (1/3))))  ## 3 = reps and 69 is MSE error

  qnorm(p=0.975)

  ##



  library(predictmeans)
  rm(q2)
  q2<-predictmeans(out, "line", plot=F, level=0.05) # get lsd and means p=.1
  cv<-NA
  cv<-q2$LSD[2]
  means<-as.data.frame(q2$`Predicted Means`)


  trial_mean$line<-names(trial_mean)
  names(trial_mean)[1]<-"yield"
  cv$line<-names(cv)
  names(cv)[1]<-"yield"
  cv$line<-"LSD"
  cv<-as.data.frame(cv)
  rm(result)
  result<-rbind(means, trial_mean, cv)
result

#####

# single year multiple locations (single year state wide average)
#####
sel<-c(2017)
data<-subset(vt1, year %in% sel) # filter by year
data<-droplevels(data)
table(data$trial_name)
locs<-unique(data$location) # list of locaions matching year criteria

hist(data$yield)
min(data$yield, na.rm=T)

# get average of line by rep
data3<-aggregate(yield ~ line +rep+trial_name,FUN=mean, data=data, na.rm=F, na.omit =F) #average by rep if more than one per rep
data3<-droplevels(data3)
table(data3$line)
data3$rep<-as.character(data3$rep)
str(data3)
out<-lm(yield ~ line *  rep%in%trial_name * trial_name  , data=data3) #aov
anova<-anova(out)
anova$`Mean Sq`[4]
number_enviro<-length(unique(data3$trial_name))
number_reps<-length(unique(data$rep))
lsd_05<-qnorm(p=0.975) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.05)valeus
lsd_10<-qnorm(p=0.95) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.10)valeus

#qnorm(p=0.975) * sqrt((1036*((2/30)))) ## 3 = reps and 69 is MSE error #sqrt of 2/number of obs #2017 16.288
#1/(number of obs per line) + 1/(number of enviroments * numbers of )

100-95

x<-1036/292
x
# f test for line x enviro
df(x=x, df1=234, df2=509)

q2<-NULL
q2<-predictmeans(out, "line", plot=F, level=0.05) # get lsd and means p=.1


out<-lm(yield ~ line * rep %in%trial_name *trial_name  , data=data3) #aov
anova<-anova(out)
anova

# 2/ ( numb of enviro * 3 ) 2/(30)

table(data3$trial_name)
table(data3$line)
anova$`Mean Sq`
MSerror<-anova$`Mean Sq`[4]
DFerror<-anova$'Df'[4]
q<-LSD.test(out, "line", DFerror, MSerror,0.1)
cv<-NA
cv<-q$statistics[6]
means<-as.data.frame(q$groups[1])
means$line<-row.names(means)
trial_mean<-q$statistics[3]
trial_mean$line<-names(trial_mean)
names(trial_mean)[1]<-"yield"
cv$line<-names(cv)
names(cv)[1]<-"yield"
rm(result)
result<-rbind(means, trial_mean, cv)
result
#####

# multiple years location (location average)
#####
sel<-c(2017, 2016)
data<-subset(vt1, year %in% sel) # filter by year
data<-droplevels(data)
table(data$trial_name)

## filter by location
locs<-unique(data$location) # list of locaions matching year criteria
i<-3
loc<-locs[i]
data2<-subset(data, location %in% loc) # filter by location

# get average of line by rep
data3<-aggregate(yield ~ line +rep+trial_name,FUN=mean, data=data2, na.rm=F, na.omit =F) #average by rep if more than one per rep
data3<-droplevels(data3)
table(data3$line)
data3$rep<-as.character(data3$rep)

#drop lines which are not in both years
keeps<-table(data3$line ,data3$trial_name)
keeps<-as.data.frame.matrix(keeps)
row_sub = apply(keeps, 1, function(row) all(row !=0 ))
keeps<-keeps[row_sub,]
keeps<-row.names(keeps)  # gets list of locatiosn with boty yr1 and yr3 data
data3<-subset(data3, line %in% keeps)
data3<-droplevels(data3)

#anovas

out<-lm(yield ~ line +  rep + trial_name  , data=data3) #aov
anova<-anova(out)
anova

table(data3$trial_name)
table(data3$line)

anova$`Mean Sq`
MSerror<-anova$`Mean Sq`[4]
DFerror<-anova$'Df'[4]
q<-LSD.test(out, "line", DFerror, MSerror, alpha = 0.10)
cv<-NA
cv<-q$statistics[6]
means<-as.data.frame(q$groups[1])
means$line<-row.names(means)
trial_mean<-q$statistics[3]
trial_mean$line<-names(trial_mean)
names(trial_mean)[1]<-"yield"
cv$line<-names(cv)
names(cv)[1]<-"yield"
result<-rbind(means, trial_mean, cv)
rm(result)
result
#####
