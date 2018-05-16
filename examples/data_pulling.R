# laoding the raw files

library(BreedR)

#####
exp_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/expt_26apr18.csv", head=T,stringsAsFactors = F) # experiemntial description file
fld_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/field_2may18.csv", head=T, stringsAsFactors = F) # field phnoetype  file
qual_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/post_harvest_27apr18.csv", head=T, stringsAsFactors = F) # quality phnoetype  file
#####

# pulls out all data
all<-data.pull(expt =exp_odb, field= fld_odb, qual = qual_odb )
head(all)
dim(all)

# pulls out  data with respect to year
all_17<-data.pull(expt =exp_odb, field= fld_odb, qual = qual_odb, year=2017 )
head(all_17)
dim(all_17)

# pulls out  data with respect to line
deon_17<-data.pull(expt =exp_odb, field= fld_odb, qual = qual_odb, year=2017, lines ='DEON' )
head(deon_17)
dim(deon_17)
unique(all_17$trial_name)


# pulls out  data with respect to trail type
deon_17_vt<-data.pull(expt =exp_odb, field= fld_odb, qual = qual_odb, year=2017, lines ='DEON', trial ="vt" )
head(deon_17_vt)
dim(deon_17_vt)
unique(deon_17_vt$trial_name)


## pull out data with respect to a number of liens
multi_17_vt<-data.pull(expt =exp_odb, 
                      field= fld_odb, 
                      qual = qual_odb, 
                      year=2017, 
                      lines =c('DEON','SABER') ,
                      trial ="vt" )




