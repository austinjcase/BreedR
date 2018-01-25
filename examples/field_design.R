
# required pacakges

#install BreedR
devtools::install_github("austinjcase/BreedR")
library(BreedR)

# install plyr
install.packages(plyr)
library(plyr)

# install stringr
install.packages(stringr)
library(stringr)


# here set your workding direcotry
# must be modifed
# all files must be in working directory
# setwd("/Volumes/CFANS/AGRO/Oat_Lab/R CODES for planting/2018/") # set working directory 
setwd("/Users/case0197/Desktop") # Or desktop

# download the working example data to the working directory
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/example_IYT_chk.csv"), "iyt_chk.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/example_IYT_entry.csv"),"iyt_entry.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/example_VT_entry.csv"),"vt_entry.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/oat_loc_ids.csv"),"locs.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/oat_trial_ids.csv"),"trial.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/example_pyt_chk.csv"), "pyt_chk.csv", row.names =F)
write.csv(read.csv("https://raw.githubusercontent.com/austinjcase/BreedR/master/exmple%20data/example_pyt_entry.csv"),"pyt_entry.csv", row.names =F)


#VTs /MAT or COPS
##############################
##############################
# DESIGN NOTES#
## RCBD (Randomized Complete Block Design)
# no checks
# each entry apperes onece in the block
# three blocks per location
# fills with entry is true meaning may be duplicate entery in a block

?make.vt() # help function

x<- make.vt(loc.to.use="mos", # this is the location use the codes
                      loc.ids="locs.csv", # ths is th csv file with loc id's
                     trial.ids="trial.csv", # this is the csv file with the trail ids
                      experiment="VT", # name of the expeiremnt must match trail names
                     entries="vt_entry.csv", # csv file with the entry list
                     plot.start=1, # number to start teh first plot on 
                     number.blocks=3, # number of blocks 
                     year=2018, # the planting year
                zurn.seed=5) # the zurn crop number for oat this is 5, barley is 3

# makes the map 
maps<-x$map.file # map files
maps

files<-x$data.book # data sheet files
files
head(files)
#write output files, remove the "#" to run
write.csv(maps, paste(files$trial[1],"map.csv", sep = "_"), row.names=F)
write.csv(files, paste(files$trial[1],"data_book.csv", sep = "_"), row.names=F)

## IYT's / MIT 
##############################
##############################
# DESIGN NOTES#
## RCBD (Randomized Complete Block Design) with checks
# each entry apperes onece in the block each check twice in each block
# three blocks per location
# fill with chekcs


?make.iyt() #help files

y<-make.iyt(loc.to.use="crk", # this is the location use the codes # must match locs.csv
            experiment="EDS", # name of the expeiremnt  # must match trial.csv
            #
            entries="iyt_entry.csv", # csv file with the entry list
            checks ="iyt_chk.csv", # 
            #
            plot.start=100, # plot 1 stat number
            number.blocks=1, # number of blocks 
            year=2018,       # plating year
            zurn.seed=3,    #zurn.seed nubmer 5 for oat 3 for barley
            #
            # not needed to change below but can change if not using example file names
            loc.ids="locs.csv", # ths is th csv file with loc id's
            trial.ids="trial.csv")  # this is the csv file with the trail ids) 

maps2<-y$map.file # map files
maps2

files2<-y$data.book # data sheet files
files2

#write output files, remove the "#" to run
write.csv(maps2, paste(files2$trial[1],"map.csv", sep = "_"), row.names=F)
write.csv(files2, paste(files2$trial[1],"data_book.csv", sep = "_"), row.names=F)


## PYT's /SPY
##############################
##############################
# DESIGN NOTES#
# AIBD (Augmented Incomplete Block Design) with checks 
# where checks apperear witin bloks but no entry replicatoin witin a block
# six bolock default
# there are primary and secondary checks
# there is one primary check and it will be in each block
# the secondary checks may appere in some or all blocks at random
# where secondary checks are replicated 3 times in the whole field default
# fill with chekcs
##############

?make.pyt()

z<-make.pyt( loc.to.use="mos",
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

maps3<-z$map.file
maps3

files3<-z$data.book
files3

write.csv(maps3, paste(files3$trial[1],"map.csv", sep = "_"), row.names=F)
write.csv(files3, paste(files3$trial[1],"data_book.csv", sep = "_"), row.names=F)

