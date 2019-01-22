#' Pull data from a database
#' 
#' @description 
#' This function is used to pull data from one of the breeding databases
#'
#' @param expt \code{data.frame} of experient descriptions.
#' @param field \code{data.frame} of field data.
#' @param qual \code{data.frame} of post harvest data.
#' @param trial \code{character} trial to select (optional).
#' @param years \code{character} years to select (optional).
#' @param lines \code{character} lines to be select (optional).
#' 
#' @importFrom sqldf sqldf
#' @importFrom data.table setDT
#' 
#' @export
#' 
data.pull <- function(expt, field, qual, trial = NULL, years = NULL, lines = NULL) {
  
  ## Create an expression to select
  pheno <- sqldf("select * from field left join expt on field.trial_name = expt.trial_name")
  pheno <- pheno[, !duplicated(colnames(pheno))]
  
  pheno_all<-sqldf("select * from pheno left join qual on pheno.database_code = qual.database_code")
  pheno_all <- pheno_all[, !duplicated(colnames(pheno_all))]
  
  if(is.null(trial)) {select<-pheno_all} else {select<-subset(pheno_all, experiment == trial)}
  if(is.null(years)){select<-select} else {select<-subset(select, year %in% years)}
  if(is.null(lines)){select<-select} else {select<-subset(select, line %in% lines)}
  
  select<-setDT(select)
  return(select)
}

#' #' @describeIn data_pull
#' #' @export
#' data.pull <- function(expt, field, qual, trial = NULL, years = NULL, lines = NULL) {
#'   stop("This function is deprecated. Please use 'data_pull' instead.")
#' }
