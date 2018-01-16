# laoding the raw files

library(BreedR)

#####
exp_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/expt_7dec17.csv", head=T,stringsAsFactors = F) # experiemntial description file
fld_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/field_9jan18.csv", head=T, stringsAsFactors = F) # field phnoetype  file
qual_odb<-read.csv("/Volumes/CFANS/AGRO/Oat_Lab/Oat_Database/ODB/post_harvest_9jan18.csv", head=T, stringsAsFactors = F) # quality phnoetype  file
#####


?vt_analysis()
head(fld_odb)
# running the function

#with the cut offs
test_out<-vt_analysis(exp_data = exp_odb, fld_data = fld_odb, qual_data = qual_odb,
            trial_type = "vt", yr1 = 2017, yr2 = 2016, yr3 = 2015,
            trait_sel = "yield",
            exclude = c("vt_2017_stp", "vt_2016_stp", "vt_2017_kim", "vt_2015_was" ,"vt_2017_roc" ),
            expt_obj = "agro", cut_offs = c(50, 300))
rm(test_out)

#without cut offs
test_out<-vt_analysis(exp_data = exp_odb, fld_data = fld_odb, qual_data = qual_odb,
                        trial_type = "vt", yr1 = 2017, yr2 = 2016, yr3 = 2015,
                        trait_sel = "yield",
                        exclude = c("vt_2017_stp", "vt_2016_stp", "vt_2017_kim", "vt_2015_was" ,"vt_2017_roc" ),
                        expt_obj = "agro", cut_offs = NULL)
rm(test_out)

#test weight
test_out<-vt_analysis(exp_data = exp_odb, fld_data = fld_odb, qual_data = qual_odb,
                      trial_type = "vt", yr1 = 2017, yr2 = 2016, yr3 = 2015,
                      trait_sel = "test_weight",
                      exclude = c("vt_2017_stp", "vt_2016_stp", "vt_2017_kim", "vt_2015_was" ,"vt_2017_roc" ),
                      expt_obj = "agro", cut_offs = NULL)
rm(test_out)


# just one year
test_out<-vt_analysis(exp_data = exp_odb, fld_data = fld_odb, qual_data = qual_odb,
                      trial_type = "vt", yr1 = 2017,
                      trait_sel = "yield",
                      expt_obj = "agro", cut_offs = c(50, 300))
rm(test_out)

# just two years
test_out<-vt_analysis(exp_data = exp_odb, fld_data = fld_odb, qual_data = qual_odb,
                      trial_type = "vt", yr1 = 2017, yr2=2016,
                      trait_sel = "yield",
                      expt_obj = "agro", cut_offs = c(50, 300))
rm(test_out)

