
# required pacakges
# notes add beds as user setting
# make the outputs into a better format

# install devtools
install.packages("devtools")
library(devtools)

#install BreedR
devtools::install_github("austinjcase/BreedR")
library(BreedR)

# install plyr
install.packages("plyr")
library(plyr)

# install stringr
install.packages("stringr")
library(stringr)

# here set your workding direcotry
# must be modifed
# all files must be in working directory

setwd("/Volumes/CFANS/AGRO/Oat_Lab/R CODES for planting/R JUNK/") # set working directory 

setwd("/Volumes/CFANS/AGRO/Oat_Lab/2018/FIELD/2018_R_designs//") # set working directory 
#setwd("/Users/case0197/Desktop") # Or desktop

# download the working example data to the working directory
write.csv(read.table("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/iyt_chk.txt", header=T), "iyt_chk.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/example_IYT_entry.csv"),"iyt_entry.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/example_VT_entry.csv"),"vt_entry.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/oat_loc_ids.csv"),"locs.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/oat_trial_ids.csv"),"trial.csv", row.names =F)
write.csv(read.table("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/pyt_chk.txt", header=T), "pyt_chk.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/example_pyt_entry.csv"),"pyt_entry.csv", row.names =F)




#VTs /MAT or COPS
#########################################################################################
#########################################################################################
# DESIGN NOTES#
## RCBD (Randomized Complete Block Design)
# no checks
# each entry apperes onece in the block
# three blocks per location
# fills with entry is true meaning may be duplicate entery in a block

?make.vt() # help function
vt.out<- make.vt(
            # MUST CHANGE THESE THINGS BY LOCATION
            loc.to.use="mos", # this is the location use the codes
            experiment="VT",  # name of the expeiremnt must match trail names
            num.beds=10,       # number of beds, i.e. number of field rows
            #
            # MUST CHANGE THESE TINGS BY TRIAL i.e. MIT/MAT/IYT...
            entries="vt_entry.csv", # csv file with the entry list
            #
            # THIS IS THE DEFAULTS,DONT NEED TO CHANGE THEM BUT CHECK THEM FIRST TIME
            plot.start=1,    # number to start the first plot on 
            number.blocks=3, # number of blocks 
            year=2018,       # the planting year
            zurn.seed=5,
            fill.entry=FALSE, # fill are done with entry, default is TRUE, one and only of may must be true 
            fill.chk =FALSE, # fill with chekcs, deafalut is FALSE, one and only of these may be true
            #
            loc.ids="locs.csv",    # ths is th csv file with loc id's
            trial.ids="trial.csv") # this is the csv file with the trail ids  

# makes the map 
maps<-vt.out$map.file # map files
files<-vt.out$data.book # data sheet files


# make a rep by rep line up

#repbyrep<-reshape(files[, c("line_name", "plot", "replication")], idvar = "line_name", timevar = "replication", direction = "wide")


#write output files
write.csv(maps, paste(files$trial[1],"map.csv", sep = "_"), row.names=F)
write.csv(files, paste(files$trial[1],"data_book.csv", sep = "_"), row.names=F)
#write.csv(repbyrep, paste(files$trial[1],"repbyrep.csv", sep = "_"), row.names=F)
#########################################################################################
#########################################################################################


## IYT's / MIT 
#########################################################################################
#########################################################################################
# DESIGN NOTES#
## RCBD (Randomized Complete Block Design) with checks
# each entry apperes onece in the block each check twice in each block
# three blocks per location
# fill with chekcs
# designed with about 50 entries in mind
# num.reps.chk should be sqrt(entry)/number.blocks so sqrt(50)/3 =  ~ 2 so this is why 2 is the default

?make.iyt() #help files
iyt.out<-make.iyt(
            # MUST CHANGE THESE THINGS BY LOCATION
            loc.to.use="crk", # this is the location use the codes must match locs.csv
            experiment="IYT", # name of the expeiremnt must match trial.csv
            num.beds=8,       # number of beds, i.e. number of field rows
            #
            # MUST CHANGE THESE TINGS BY TRIAL i.e. MIT/MAT/IYT...
            entries="iyt_entry.csv", # csv file with the entry list
            checks ="iyt_chk.csv", # csv file with the chekc list
            #
            # THIS IS THE DEFAULTS,DONT NEED TO CHANGE THEM BUT CHECK THEM FIRST TIME
            plot.start=1, # plot stat number
            number.blocks=3, # number of blocks 
            year=2018,       # plating year
            zurn.seed=5,    #zurn.seed nubmer 5 for oat 3 for barley
            num.reps.chk=2, # number of reps of the checks within a block
            #
            # THIS IS THE LOCATION AND LOC ID's FOR ZURN CODES
            loc.ids="locs.csv", # ths is th csv file with loc id's
            trial.ids="trial.csv")  # this is the csv file with the trail ids) 

maps2<-iyt.out$map.file # map files
files2<-iyt.out$data.book # data sheet files
#repbyrep2<-reshape(files2[, c("line_name", "plot", "replication")], idvar = "line_name", timevar = "replication", direction = "wide")
#library(reshape2)



#write output files, remove the "#" to run
write.csv(maps2, paste(files2$trial[1],"map.csv", sep = "_"), row.names=F)
write.csv(files2, paste(files2$trial[1],"data_book.csv", sep = "_"), row.names=F)

#########################################################################################
#########################################################################################

## PYT's /SPY
#########################################################################################
#########################################################################################
# DESIGN NOTES#
# AIBD (Augmented Incomplete Block Design) with checks 
# where checks apperear witin bloks but no entry replicatoin witin a block
#
# number of blocks should be sqrt of number of entries if 300, so sqrt(300) =18
#
# number of blocks should be sqrt of number of entries sqrt(300) =18
#
# there are primary and secondary checks
# there is one primary check and it will be in each block
# the secondary checks may appere in some or all blocks at random
# where secondary checks are replicated 3 times in the whole field default
# fill with chekcs

?make.pyt()

pyt.out<-make.pyt( 
             # MUST CHANGE THESE THINGS BY LOCATION
             loc.to.use="lam",
             experiment="FOUNDER",
             num.beds=8,
             #
             # MUST CHANGE THESE TINGS BY TRIAL i.e. MIT/MAT/IYT...
             entries= "founder_entry.csv",
             checks ="founder_chk_2.csv",
             #
             # THIS IS THE DEFAULTS, DONT NEED TO CHANGE THEM BUT CHECK THEM FIRST TIME
             plot.start=9500,
             nBlk = 16,
             year=2018,
             zurn.seed=5,
             chk2rep = 3,
             #
             # THIS IS THE LOCATION AND LOC ID's FOR ZURN CODES
             loc.ids="locs.csv",
             trial.ids="trial.csv")

maps3<-pyt.out$map.file
files3<-pyt.out$data.book

dim(files3)

write.csv(maps3, paste(files3$trial[1],"map.csv", sep = "_"), row.names=F)
write.csv(files3, paste(files3$trial[1],"data_book.csv", sep = "_"), row.names=F)

#########################################################################################
#########################################################################################