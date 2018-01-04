#' data.pull fnction
#'
#' This function allows you to pull data from the oat data base or similar
#' @param expt REQUIRED \code{data.frame} of experient descirptions in the OAT DATA BASE
#' @param field REQUIRED \code{data.frame} of field data in the OAT DATA BASE
#' @param post REQUIRED \code{data.frame} of post harvest data in the OAT DATA BASE
#' @param trail OPTIONAL\code{character string} a type of trial as described in the OAT DATA BASE
#' @param years OPTIONAL\code{character string} years to select data from
#' @param lines OPTIONAL\code{character string} lines to be selected
#' @keywords data.pull
#' @export
#' @examples
#' data.pull


data.pull <- function(expt=NULL,field=NULL, qual=NULL,trial=NULL, years=NULL, lines=NULL)
{
  library(sqldf)
  library(data.table)
  pheno<-sqldf("select * from field left join expt on field.trial_name = expt.trial_name")
  pheno <- pheno[, !duplicated(colnames(pheno))]
  pheno_all<-sqldf("select * from pheno left join qual on pheno.database_code = qual.database_code")
  rm(pheno)
  pheno_all <- pheno_all[, !duplicated(colnames(pheno_all))]
  if(is.null(trial)) {select<-pheno_all} else {select<-subset(pheno_all, experiment == trial)}
  if(is.null(years)){select<-select} else {select<-subset(select, year %in% years)}
  if(is.null(lines)){select<-select} else {select<-subset(select, line %in% lines)}
  select<-setDT(select)
  return(select)
}
