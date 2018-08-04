library(roxygen2)
library(BreedR)

#####
exp_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/expt_26apr18.csv", head=T,stringsAsFactors = F) # experiemntial description file
fld_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/field_2may18.csv", head=T, stringsAsFactors = F) # field phnoetype  file
qual_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/post_harvest_27apr18.csv", head=T, stringsAsFactors = F) # quality phnoetype  file
#####

out2<-line_means(expt.impt=exp_odb,
                 field.impt=fld_odb,
                 qual.impt=qual_odb,
                 traits.impt=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'),
                 year.impt=c(2017, 2016),
                 lines.impt=c('DEON','SABER'),
                 trial.impt=c('vt','pyt'), 
                 trial.drop.impt=c('vt_2017_stp'))

out2


# minimum inputs
out2<-line_means(expt.impt=exp_odb,
                 field.impt=fld_odb,
                 qual.impt=qual_odb,
                 traits.impt=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'))
out2

# can select by line 
out2<-line_means(expt.impt=exp_odb,
                 field.impt=fld_odb,
                 qual.impt=qual_odb,
                 traits.impt=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'),
                 lines.impt=c('DEON','SABER'))
out2

# can select by line and trials
out2<-line_means(expt.impt=exp_odb,
                 field.impt=fld_odb,
                 qual.impt=qual_odb,
                 traits.impt=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'),
                 lines.impt=c('DEON','SABER'), 
                 trial.impt=c('vt','pyt'))
out2

# can select by line and trials and can excude known trials of bad quality
out2<-line_means(expt.impt=exp_odb,
                 field.impt=fld_odb,
                 qual.impt=qual_odb,
                 traits.impt=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'),
                 lines.impt=c('DEON','SABER'), 
                 trial.impt=c('vt','pyt'), 
                 trial.drop.impt=c('vt_2017_stp'))
out2




####

out2<-trl_means(expt.impt=exp_odb,
                 field.impt=fld_odb,
                 qual.impt=qual_odb,
                 traits.impt=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'))
out2


