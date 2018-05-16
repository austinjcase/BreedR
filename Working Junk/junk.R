
library(BreedR)

#####
exp_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/expt_26apr18.csv", head=T,stringsAsFactors = F) # experiemntial description file
fld_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/field_2may18.csv", head=T, stringsAsFactors = F) # field phnoetype  file
qual_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/post_harvest_27apr18.csv", head=T, stringsAsFactors = F) # quality phnoetype  file
#####


?line_means()

out2<-line_means(expt.impt=exp_odb,
                 field.impt=fld_odb,
                 qual.impt=qual_odb,
                 traits.impt=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'),
                 year.impt=c(2017, 2016),
                 lines.impt=c('DEON','SABER'),
                 trial.impt=c('vt','pyt'))

out2





  expt<-exp_odb
  field<-fld_odb
  qual<-qual_odb
  traits<-c('yield', 'test_weight', 'groat_glucan', 'cr_sev')
  year<-c(2017, 2016)
  lines<-c('DEON','SABER')
  trial<-c('vt','pyt')
  ###
  out1<-data.pull(expt =expt, 
                  field= field, 
                  qual = qual, 
                  year=year, 
                  lines =lines ,
                  trial = trial )
  traits<-c('line', traits)
  q<-out1[,..traits]
  class(out1)
  
  out<-aggregate(. ~ line, data=q, mean, na.rm=TRUE, na.action= NULL)
  return(out)
}





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



