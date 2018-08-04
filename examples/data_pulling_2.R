# laoding the raw files

library(BreedR)

#####
exp_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/expt_26apr18.csv", head=T,stringsAsFactors = F) # experiemntial description file
fld_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/field_2may18.csv", head=T, stringsAsFactors = F) # field phnoetype  file
qual_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/post_harvest_27apr18.csv", head=T, stringsAsFactors = F) # quality phnoetype  file
#####


## pull out data with respect to a number of liens

#reqired imputs
### USR INPUTS
# required
expt<-exp_odb
field<-fld_odb
qual<-qual_odb
traits<-c('yield', 'test_weight', 'groat_glucan', 'cr_sev')

# optional
year<-c(2017, 2016)
lines<-c('DEON','SABER')
trial<-c('vt','pyt')
###


line_means<-function(expt=NULL,
                     field=NULL,
                     qual=NULL,
                     traits=NULL,
                     year=NULL,
                     lines=NULL,
                     trial=NULL){
### function START
expt<-expt
field<-field
qual<-qual
traits<-traits
year<-year
lines<-lines
trial<-trial
###
out1<-data.pull(expt =expt, 
                      field= field, 
                      qual = qual, 
                      year=year, 
                      lines =lines ,
                      trial = trial )
traits<-c('line', traits)
out<-aggregate(. ~ line, data=out1[,..traits], mean, na.rm=TRUE, na.action= NULL)
return(out)
}

out2<-line_means(expt=exp_odb,
           field=fld_odb,
           qual=qual_odb,
           traits=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'),
           year=c(2017, 2016),
           lines=c('DEON','SABER'),
           trial=c('vt','pyt'))

out2


out3<-line_means(expt=exp_odb,
                 field=fld_odb,
                 qual=qual_odb,
                 traits=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'),
                 lines=c('DEON','SABER'),
                 trial=c('vt','pyt'))

out3



out4<-line_means(expt=exp_odb,
                 field=fld_odb,
                 qual=qual_odb,
                 traits=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'),
                 lines=c('DEON','SABER'))

out4



out5<-line_means(expt=exp_odb,
                 field=fld_odb,
                 qual=qual_odb,
                 traits=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'))

out5

out6<-line_means(expt=exp_odb,
                 field=fld_odb,
                 qual=qual_odb,
                 traits=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'),
                 trial=c('vt'))

out6




###

