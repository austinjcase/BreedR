library(roxygen2)
library(BreedR)

#####
exp_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/expt_26apr18.csv", head=T,stringsAsFactors = F) # experiemntial description file
fld_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/field_2may18.csv", head=T, stringsAsFactors = F) # field phnoetype  file
qual_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/post_harvest_27apr18.csv", head=T, stringsAsFactors = F) # quality phnoetype  file
#####

# minimum inputs
# will give means of all trails for thoes traits
out2<-trl_means(expt.impt=exp_odb,
                 field.impt=fld_odb,
                 qual.impt=qual_odb,
                 traits.impt=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'))
out2


# will give means of all trails for thoes traits in 2016 and 2017
out2<-trl_means(expt.impt=exp_odb,
                field.impt=fld_odb,
                qual.impt=qual_odb,
                traits.impt=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'), 
                year.impt= c(2016, 2017))
out2

# will give means of all trails for thoes traits in 2016 and 2017 of type vt
out2<-trl_means(expt.impt=exp_odb,
                field.impt=fld_odb,
                qual.impt=qual_odb,
                traits.impt=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'), 
                year.impt= c(2016, 2017), 
                trial.impt= c('vt'))
out2

# will give means of all trails for thoes traits in 2016 and 2017 of type vt
# for just lines SABER AND DEON
out2<-trl_means(expt.impt=exp_odb,
                field.impt=fld_odb,
                qual.impt=qual_odb,
                traits.impt=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'), 
                year.impt= c(2016, 2017), 
                trial.impt= c('vt'), 
                lines.impt= c('DEON', 'SABER'))
out2

# will give means of all trails for thoes traits in 2016 and 2017 of type vt
# for just lines SABER AND DEON and drop vt_2016_ste
out2<-trl_means(expt.impt=exp_odb,
                field.impt=fld_odb,
                qual.impt=qual_odb,
                traits.impt=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'), 
                year.impt= c(2016, 2017), 
                trial.impt= c('vt'), 
                lines.impt= c('DEON', 'SABER'), 
                trial.drop.impt =c('vt_2016_ste'))
out2




