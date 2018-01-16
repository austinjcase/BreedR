######################################################
##############################
# DESIGN NOTES#
## RCBD (Randomized Complete Block Design)
# no checks
# each entry apperes onece in the block
# three blocks per location

# required pacakges
library(devtools) 
install_github("austinjcase/BreedR")
library(BreedR)
library(plyr)
library(stringr)


# here set your workding direcotry
setwd("/Volumes/CFANS/AGRO/Oat_Lab/R CODES for planting/2018/")# set working directory 

# download the working example data to the working directory
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/example_IYT_chk.csv"), "iyt_chk.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/example_IYT_entry.csv"),"iyt_entry.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/example_VT_entry.csv"),"vt_entry.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/oat_loc_ids.csv"),"locs.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/oat_trial_ids.csv"),"trial.csv", row.names =F)


#VTs
######################################################
##############################
# DESIGN NOTES#
## RCBD (Randomized Complete Block Design)
# no checks
# each entry apperes onece in the block
# three blocks per location
# fills with entry is true meaning may be duplicate entery in a block

# help function
?make.vt

x<- make.vt(loc.to.use="mos", # this is the location use the codes
                      loc.ids="locs.csv", # ths is th csv file with loc id's
                     trial.ids="trial.csv", # this is the csv file with the trail ids
                      experiment="VT", # name of the expeiremnt must match trail names
                     entries="vt_entry.csv", # csv file with the entry list
                     plot.start=1, # number to start teh first plot on 
                     number.blocks=3, # number of blocks 
                     year=2018, # the planting year
            zurn.seed=5) # the zurn crop number for oat this is 5, barley is 3

maps<-x$map.file # map files
maps
files<-x$data.book # data sheet files
files

#write output files, remove the "#" to run
write.csv(maps, paste(files$trial[1],"map.csv", sep = "_"), row.names=F)
write.csv(files, paste(files$trial[1],"data_book.csv", sep = "_"), row.names=F)


## IYT's
######################################################
##############################
# DESIGN NOTES#
## RCBD (Randomized Complete Block Design)
# checks checks
# each entry apperes onece in the block each check twice in each block
# three blocks per location
# fill with chekcs

?make.iyt() #help files
y<-make.iyt(loc.to.use="mos", # this is the location use the codes
          loc.ids="locs.csv", # ths is th csv file with loc id's
          trial.ids="trial.csv", # this is the csv file with the trail ids
          experiment="IYT", # name of the expeiremnt 
          entries="iyt_entry.csv", # csv file with the entry list
          plot.start=1000, # plot 1 stat number
          number.blocks=3, # number of blocks 
          year=2018,# plating year
          zurn.seed=5,  #zurn.seed nubmer 5 for oat 3 for barley
          checks ="iyt_chk.csv") 

maps2<-y$map.file # map files
maps2

files2<-y$data.book # data sheet files
files2

#write output files, remove the "#" to run
write.csv(maps2, paste(files2$trial[1],"map.csv", sep = "_"), row.names=F)
write.csv(files2, paste(files2$trial[1],"data_book.csv", sep = "_"), row.names=F)


