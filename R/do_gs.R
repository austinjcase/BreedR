#' do.gs function
#'
#' This function allows you to pull data from the oat data base or similar
#' @param geno REQUIRED \code{matrix} #matrix with geno x markers, marker names are first row and lines are first col. cell[1,1] is empty
#' @param pheno REQUIRED \code{data.frame} #data frame with first col as line name, next are the data. line name must be "line_name"
#' @param trait REQUIRED \code{character string} #name of trait in pheno file, i.e. "GrainYield" 
#' 
#' @param cross.valid OPTIONAL \code{T/F} # T/F to do cross validation, either prog.valid and/or cross.valid must be set to T
#' @param tp.size OPTIONAL \code{number} # must be specified if cross.valid=T; fraction of whole pop to use as the training pop (0-1)
#' @param nrep OPTIONAL \code{number} # must be specified if cross.valid=T; number of bootrap itterations of of cross validation (1-inf)
#' 
#' @param prog.valid OPTIONAL \code{T/F} # T/F to do prog validation, either prog.valid and/or cross.valid must be set to T 
#' @param id OPTIONAL \code{data.frame} #data frame with fist col as line name, next is class where "prog" is the progney validation and "train" is the TP
#' 
#' @keywords do.gs
#' @export
#' 
#' @examples
#' 
#'### example data
#'geno<-read.csv("geno.example.csv", head=T, stringsAsFactors = F)
#'pheno<-read.csv("pheno.example.csv", head=T, stringsAsFactors = F )
#'id<-read.csv("ids.csv", head=T, stringsAsFactors = F )
#'### done by user

# cross and prog valid
#'do.gs(geno=geno, id = id, pheno = pheno, cross.valid =T, prog.valid = T, 
#'      tp.size= 0.7, nrep = 5, trait= "GrainYield")
# just cross valid
#'do.gs(geno=geno, pheno = pheno, cross.valid =T, 
#'      tp.size= 0.7, nrep = 5, trait= "GrainYield")
#'# just prog valid
#'do.gs(geno=geno, id = id, pheno = pheno, prog.valid = T, trait= "GrainYield")
#'


do.gs<-function(geno=NULL, pheno=NULL, id=NULL, cross.valid=F, prog.valid = F, tp.size= NULL ,nrep=NULL, trait=NULL) {
  
  ## input specs
  #########################
  # processing data files
  library(rrBLUP)
  row.names(geno)<-geno[,"line_name"] # set row names for genotypes 
  geno<-geno[,-c(1)] # drop genotypes names
  geno<-as.matrix(geno)
  ##########################
  
  ##########################
  # doing cross validation
  if (cross.valid == T ){
    out2<-NULL
    for (i in 1: nrep){
      print("DOING CROSS VALIDATION") # confirm cross valid
      print(paste("REP", i, "of", nrep))
      vp.size<-(1-tp.size) # get vp size
      whole.pop<-Reduce(intersect, list(row.names(geno),pheno$line_name)) # getting names of all pop
      tp.cross<-sample(whole.pop, ceiling(length(whole.pop)* tp.size)) # sampleing the names in TP
      vp.cross<-setdiff(whole.pop, tp.cross) # sampleing names in the VP 
      geno_tp <- geno[tp.cross, , drop = FALSE] # subset geno for tp
      geno_vp <- geno[vp.cross, , drop = FALSE] # subset geno for vp
      pheno_tp <- subset(pheno, line_name %in% tp.cross) # subset pheno for tp
      pheno_vp <- subset(pheno, line_name %in% vp.cross) # subset pheno for vp 
      f <- as.formula(paste(trait, "~ line_name")) # creates the model with the selected trait 
      mf <- model.frame( f,data = pheno_tp) # makes a model frame for the trait of interest
      y <- model.response(mf) # Extract the response vector
      # Rename the genotype matrix .. random effects
      Z_tp <- geno_tp
      Z_vp <- geno_vp
      #X <- model.matrix(~ 1, data = mf) # Create the X matrix, usefull for mutiple enviroments. 
      #K <- diag(ncol(Z_tp)) # Create the K matrix, the dimension is the number of markers , covariance between marekrs.
      # Fit the model
      #fit <- mixed.solve(y = y, Z = Z_tp, K = K, X = X)
      fit <- mixed.solve(y = y, Z = Z_tp)
      u_hat <- setNames(fit$u, colnames(Z_tp))
      g_hat2 <- (Z_vp %*% u_hat) +as.vector(fit$beta) # Predict
      g_hat2<-as.data.frame(g_hat2)
      g_hat2$line_name<-row.names(g_hat2)
      x<-merge(g_hat2,pheno_vp[,c("line_name", trait)], by ="line_name", all.x =T )
      out<-cbind(cor(x[, 2], x[, 3]),trait)
      out2<-rbind(out2, out)
      rm(out)
    }
    
    out2<-as.data.frame(out2)
    colnames(out2)[1]<-"cor"
    cross.out<-out2
    rm(out2)
    cross.out$cor<-as.numeric(as.character(cross.out$cor))
  }
  ##########################
  #
  
  ##########################
  # doing prog validation
  if (prog.valid == T ){
    out<-NULL
    print("DOING PROG VALIDATION") # confirm cross valid
    whole.pop<-Reduce(intersect, list(row.names(geno),pheno$line_name)) # getting names of all pop
    vp<-subset(id, class == "prog")
    vp<-vp$line_name
    tp<-subset(id, class == "train")
    tp<-tp$line_name
    geno_tp <- geno[tp, , drop = FALSE] # subset geno for tp
    geno_vp <- geno[vp, , drop = FALSE] # subset geno for vp
    pheno_tp <- subset(pheno, line_name %in% tp) # subset pheno for tp
    pheno_vp <- subset(pheno, line_name %in% vp) # subset pheno for vp 
    f <- as.formula(paste(trait, "~ line_name")) # creates the model with the selected trait 
    mf <- model.frame( f,data = pheno_tp) # makes a model frame for the trait of interest
    y <- model.response(mf) # Extract the response vector
    # Rename the genotype matrix .. random effects
    Z_tp <- geno_tp
    Z_vp <- geno_vp
    X <- model.matrix(~ 1, data = mf) # Create the X matrix, usefull for mutiple enviroments. 
    K <- diag(ncol(Z_tp)) # Create the K matrix, the dimension is the number of markers , covariance between marekrs.
    # Fit the model
    fit <- mixed.solve(y = y, Z = Z_tp, K = K, X = X)
    u_hat <- setNames(fit$u, colnames(Z_tp))
    g_hat2 <- (Z_vp %*% u_hat) +as.vector(fit$beta) # Predict
    g_hat2<-as.data.frame(g_hat2)
    g_hat2$line_name<-row.names(g_hat2)
    x<-merge(g_hat2,pheno_vp[,c("line_name", trait)], by ="line_name", all.x =T )
    out<-cbind(cor(x[, 2], x[, 3]),trait)
    colnames(out)[1]<-"cor"
    out<-as.data.frame(out)
    out$cor<-as.numeric(as.character(out$cor))
    prog.out<-out
    rm(out)
  }
  
  ##########################
  #
  
  ##
  #list to return
  if (prog.valid == T & cross.valid == T){return(list(prog=prog.out, cross=cross.out ))}
  if (prog.valid == T & cross.valid == F){return(prog.out)}
  if (prog.valid == F & cross.valid == T){return(cross.out)}
  #  
  
  
}

