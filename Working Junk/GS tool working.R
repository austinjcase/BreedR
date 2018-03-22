library(data.table)
library(rrBLUP)
library(BreedR)

# uses
# do imputtaion 
# make a PCA
# do GS
# go cross validataion
# do progeny prediction
# peigree based prediction

setwd("/Users/case0197/Documents/git_BreedR/exmple data")

#done outsied the function 

## laod the pheno data
pheno<-fread("example_pheno.csv", head=T, stringsAsFactors = F)

## load the geno data 
geno<-fread("example.geno.csv",  head=T, stringsAsFactors = F)

## user specs
pheno<-pheno # data frame of phnotypes
geno<-geno # data frame of genotypes
pca<-TRUE # should their be a PCA made, T/F
impute<-TRUE # should the genotypes be imputed, T/F, must be true if doing GS
cross<-TRUE # will this be gs with cross validatio of the tp, T/F called here as train
cv.tp<- 80 # percent of TP to be in the trainig set 
nrep<- 10 # number of reps of the cv run
progney.vp<-TRUE # will there be progeny to use as validation population , called here a progney
progney.pred<- FALSE # will thre be unknown progney to predict T/F called here as candidate
pedigree<-TRUE # will predictions of pehnotypes based on pedigrees be made T/F

## 

## imputation of genotypes
# can be skiped by user if selected

## PCA 
# can be skipped by user if selected

## match genotypes to phenotypes
# required 

## cross validation 
# can be skiped by user if selected 

## progney validation 
# can be skipped by user if selected 

## pedigree based prediction 
# can be skipped by user if selected