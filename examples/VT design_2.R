######################################################
##############################
# DESIGN NOTES#
## RCBD (Randomized Complete Block Design)
# no checks
# each entry apperes onece in the block
# three blocks per location

# required pacakges
library(devtools) 
#install_github("austinjcase/BreedR")
library(BreedR)
library(plyr)
library(stringr)
?make.vt()
?make.iyt()

# here set your workding direcotry
setwd("/Volumes/CFANS/AGRO/Oat_Lab/R CODES for planting/2018/")# set working directory 

#VTs
######################################################
##############################
# DESIGN NOTES#
## RCBD (Randomized Complete Block Design)
# no checks
# each entry apperes onece in the block
# three blocks per location

# help function
?make.vt

x<- make.vt(loc.to.use="mos", # this is the location use the codes
                      loc.ids="loc ids.csv", # ths is th csv file with loc id's
                     trial.ids="trial ids.csv", # this is the csv file with the trail ids
                      experiment="VT", # name of the expeiremnt 
                     entries="18 VT 9jan18.csv", # csv file with the entry list
                     plot.start=1, # plot 1 stat number
                     number.blocks=3, # number of blocks 
                     year=2018,
            zurn.seed=5) # the field year

maps<-x$map.file # map files
maps

files<-x$data.book # data sheet files
files

#write.csv()

## IYT's
######################################################
##############################
# DESIGN NOTES#
## RCBD (Randomized Complete Block Design)
# checks checks
# each entry apperes onece in the block each check twice in each block
# three blocks per location
library(BreedR)

?make.iyt
?make.iyt()

y<-make.iyt(loc.to.use="mos", # this is the location use the codes
          loc.ids="loc ids.csv", # ths is th csv file with loc id's
          trial.ids="trial ids.csv", # this is the csv file with the trail ids
          experiment="IYT", # name of the expeiremnt 
          entries="18 IYT 9jan18.csv", # csv file with the entry list
          plot.start=1000, # plot 1 stat number
          number.blocks=3, # number of blocks 
          year=2018,
          zurn.seed=5, 
          checks ="18 IYT chk 9jan18.csv")

maps2<-y$map.file # map files
maps2

#write.csv(maps2, "junk.maps.csv")

files2<-y$data.book # data sheet files
files2

