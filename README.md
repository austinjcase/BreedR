# BreedR
Package for Oat/Barley Breeding at Smith Lab at the UMN.

# Introduction
This is a package that I made to to three main tasks 1.) analyize field trails, 2.) pull data from our internal database, 3.) assist with desiging filed trails for planting. 

1.) analyize field trails
see pyt_anlaysis_fun, vt_analysis_fun

2.) pull data from the oat databse
see data.pull

3.) design field trails
A large Acknowledgement to Tyler Tiede for doing all the hard work with FldTrial package. See make.vt, and make.iyt. Which are wrapers for design.rcbd from FldTrial. But add functionality for use in the ZURN system and our internal databse. 

# Installation
    devtools::install_github("austinjcase/BreedR")

# Author
Austin Case 
case0197@umn.edu

# Acknowledgements
Tyler Tiede for FldTrial and Ian McNish for SQL assistance. 
