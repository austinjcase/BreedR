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


#Function for cross vlaidataion 
#done outsied the function 

## laod the pheno data
pheno<-fread("example_pheno.csv", head=T, stringsAsFactors = F)

## load the geno data 
geno<-fread("example.geno.csv",  head=T, stringsAsFactors = F)

## user specs
pheno<-pheno # data frame of phnotypes
geno<-geno # data frame of genotypes
impute<-TRUE # should the genotypes be imputed, T/F, must be true if doing GS
cv.tp<- 80 # percent of TP to be in the trainig set 
nrep<- 10 # number of reps of the cv run
## 

## imputation of genotypes
# can be skiped by user if selected
names<-geno$line
geno$line<-NULL
geno<-as.matrix(geno)
row.names(geno)<-names
# done if user needs to impute geno data
if(impute == T){
geno2<-A.mat(geno,min.MAF=NULL,max.missing=NULL,
             impute.method="EM",tol=0.02,shrink=FALSE, return.imputed=TRUE) # compture the a.matrix and a imputed data set 
geno<-geno2$imputed
}
geno<-t(geno)
geno[1:5,1:5]

## match genotypes to phenotypes
head(pheno)

# get list of traits
traits<-unique(pheno$variable)
traits



# required 

## cross validation 

