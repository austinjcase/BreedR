###
setwd("/Volumes/CFANS/AGRO/Oat_Lab/R CODES for planting/2018/")# set working directory 

# download the working example data to the working directory
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/oat_loc_ids.csv"),"locs.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/oat_trial_ids.csv"),"trial.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/example_pyt_chk.csv"), "pyt_chk.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/example_pyt_entry.csv"),"pyt_entry.csv", row.names =F)
#


y<-make.pyt( loc.to.use="lam",
           loc.ids="locs.csv",
           trial.ids="trial.csv",
           experiment="PYT",
           entries= "pyt_entry.csv",
           plot.start=1000,
           year="2018",
           zurn.seed=5,
           checks ="pyt_chk.csv",
           chk2rep =3,
           nBlk = 6 )
    
dat<-y$data.book
dat
map<-y$map.file
map

write.csv(map, paste(dat$trial[1],"map.csv", sep = "_"), row.names=F)
write.csv(dat, paste(dat$trial[1],"data_book.csv", sep = "_"), row.names=F)
