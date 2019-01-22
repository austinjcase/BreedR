#' Compute line means
#'
#' @param expt \code{data.frame} of experient descriptions.
#' @param field \code{data.frame} of field data.
#' @param qual \code{data.frame} of post harvest data.
#' @param trial \code{character} trial to select (optional).
#' @param years \code{character} years to select (optional).
#' @param lines \code{character} lines to be select (optional).
#' 
#' 
#' This function allows line means to be gathered accross year and trials, gives simple means
#' @param expt.impt REQUIRED \code{data.frame} experiemtn description file from OAT DATA BASE
#' @param field.impt REQUIRED \code{data.frame}  field phenotype file from OAT DATA BASE
#' @param qual.impt REQUIRED \code{data.frame} quality phenotype file from OAT DATA BASE
#' @param traits.impt REQUIRED \code{list} a list of traits to use
#' @param year.impt OPTIONAL \code{list} list of years to use
#' @param lines.impt OPTIONAL \code{list} list of lines to use
#' @param trial.impt OPTIONAL \code{list} list of trials to  see OAT DATA BASE FOR CODE
#' @param trial.drop.impt OPTIONAL \code{list} list of trials to drop see OAT DATA BASE FOR CODE
#' 
#' @keywords line_means
#' @export
#' @examples
#' 
#' line_means


line_means<-function(expt.impt=NULL,
                     field.impt=NULL,
                     qual.impt=NULL,
                     traits.impt=NULL,
                     year.impt=NULL,
                     lines.impt=NULL,
                     trial.impt=NULL, 
                     trial.drop.impt=NULL){
  ### function START
  expt<-expt.impt
  field<-field.impt
  qual<-qual.impt
  traits<-traits.impt
  year<-year.impt
  lines<-lines.impt
  trial<-trial.impt
  trial.drop<-trial.drop.impt
  ###
  
  out1<-data.pull(expt =expt, 
                  field= field, 
                  qual = qual, 
                  year=year, 
                  lines =lines ,
                  trial = trial )
  out1<-as.data.frame(out1)
  if(is.null(trial.drop) == FALSE){ out1<-out1[!(out1$trial_name %in% trial.drop ),]}
  traits<-c('line','trial_name', traits)
  q<-out1[,traits]
  trl_mean<-aggregate(. ~ line +trial_name, data=q, mean, na.rm=TRUE, na.action= NULL)
  state_mean<-aggregate(. ~ line , data=subset(q, select=-c(trial_name)), mean, na.rm=TRUE, na.action= NULL)
  return(list(trl_mean=trl_mean,state_mean=state_mean))
}


###

