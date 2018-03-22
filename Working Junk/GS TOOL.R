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
# 

setwd("/Users/case0197/Documents/git_BreedR/exmple data")

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
geno<-geno
pheno<-pheno
## GS tool for cross validation 

## imputation of genotypes
# can be skiped by user if selected
print("starting step 1 imputation of genotypes")


## match genotypes to phenotypes
# required 
print("starting step 2 matching genos to phenos")


## cross validation 
# can be skiped by user if selected 
print("starting step 3 gs cross validataion")





geno.imputed<-as.data.frame(geno.imputed, stringsAsFactors =F)
str(geno.imputed[,1:10])

#geno.imputed<-imputed.genos
dim(geno.imputed) #imputed genotye data
row.names(geno.imputed)
row.names(geno.imputed)<-geno.imputed$line
geno.imputed[1:5,1:5]
dim(geno.imputed)
geno.imputed[1:5,(ncol(geno.imputed)-5):ncol(geno.imputed)]
names.geno<-row.names(geno.imputed)
length(names.geno) #257
names.geno
str(geno.imputed[,1:5])

names.pheno<-unique(pheno$line)
length(names.pheno) # 278
names.pheno

### drop lines with no gneoytpe 
geno.clean <- geno.imputed[  rownames(geno.imputed)%in% names.pheno, ]
dim(geno.clean)
geno.clean[1:5,1:5]  #  252 93136
geno.clean[1:5,(ncol(geno.clean)-5):ncol(geno.clean)]  #  252 93136
row.names(geno.clean)

class(geno.clean)
str(geno.clean[,1:5])

#geno.clean<-as.matrix(geno.clean)
#

### drop lines with  gneoytpe but  phenotype 
head(pheno)
pheno<-as.data.frame(pheno)
pheno.clean <- pheno[  pheno$line %in% names.geno,  ]
dim(pheno.clean)
head(pheno.clean)
str(pheno.clean)
length(unique(pheno.clean$line)) # 252
#






### list of trait to do. 
trait.list<-unique(pheno.clean$variable)
#[1] "height_BLUE"        "log_sev_BLUE"       "yield_BLUE"         "test_weight_BLUE"   "grain_protein_BLUE"
#[6] "grain_oil_BLUE"     "color_BLUE"         "headDAP"            "cr_sev"             "smut"              
# [11] "groat_fat"          "groat_glucan"      
# [13] "groat_protein"      "graot_percent"     

#

length(trait.list)
boot<-NULL
n<-10 # nubmer of boot straps per trait
tp<-177 # number in traint pop 
vp<-75 #number in validataion pop

#trait.list<-trait.list[1:2]

#for (j in 1:length(trait.list)){
j<-14
time1<-Sys.time()
trait<-trait.list[j] # trait to work with 
for (i in 1:n) {
  ##############
  # cross valid setup 
  train<-as.matrix(sample(1:(tp+vp), tp))
  test<-setdiff(1:(tp+vp), train)
  train<-pheno.clean[train,]
  train<-unique(train$line)
  test<-pheno.clean[ test, ]
  test<-unique(test$line)
  train.data<-droplevels(subset(pheno.clean, variable == trait))
  pheno_train<- train.data[ train.data$line %in% train, ] 
  geno_train<-geno.clean[row.names(geno.clean) %in% train,]
  pheno_test<-train.data[ train.data$line %in% test, ] 
  geno_test<-geno.clean[row.names(geno.clean) %in%test,  ]
  values<-pheno_train[,2]
  row.names(pheno_train)<-pheno_train$line
  geno_train$line<-row.names(geno_train)
  all.data<-join(pheno_train,geno_train, by='line' )  ####### THIS IS KEY WAS SORTING WRONG!!!!!!!!!!!!!!!!
  ans<-NULL
  ans<-mixed.solve(y=all.data[,2], Z=all.data[,4:ncol(all.data)], K=NULL, SE=F,return.Hinv=F)
  
  e<-as.matrix(ans$u)
  dat<-as.matrix(sapply(geno_test, as.numeric))  
  dat<-dat[,1:(ncol(dat)-1)]
  valid<-dat %*% e
  valid_pred<-as.matrix(valid[,1] + ans$beta)
  valid_pred<-as.data.frame(valid_pred)
  valid_pred$line<-row.names(geno_test)
  names(valid_pred)[1]<-'effect'
  finished<-join(valid_pred, pheno_test, by ="line")
  out<-cor(finished$effect, finished$value, use="complete")
  out<-c(out,finished[1,4] )
  boot<-rbind(boot, out)
  print("")
  print("completed boot")
  print(i)
  print ("of")
  print(n)
  print("for trait")
  print(trait.list[j] )
  print("")
  rm(out)
  # }
  print("")
  print("finished with")
  print(trait.list[j])
  time2<-Sys.time()
  print(time2-time1)
  print("")
}
