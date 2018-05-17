#' trl_means function
#'
#' This function allows line means to be gathered accross year and trials, gives simple means
#' @param expt.impt REQUIRED \code{data.frame} experiemtn description file from OAT DATA BASE
#' @param field.impt REQUIRED \code{data.frame}  field phenotype file from OAT DATA BASE
#' @param qual.impt REQUIRED \code{data.frame} quality phenotype file from OAT DATA BASE
#' @param traits.impt REQUIRED \code{list} a list of traits to use
#' @param year.impt OPTIONAL \code{list} list of years to use
#' @param lines.impt OPTIONAL \code{list} list of lines to use
#' @param trial.impt OPTIONAL \code{list} list of trials to use see OAT DATA BASE FOR CODE
#' @param trial.drop.impt OPTIONAL \code{list} list of trials to drop see OAT DATA BASE FOR CODE
#' 
#' @keywords trl_means
#' @export
#' @examples
#' 
#' 
#' trl_means


trl_means<-function(expt.impt=NULL,
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
  #trl_mean<-aggregate(. ~ line +trial_name, data=q, mean, na.rm=TRUE, na.action= NULL)
  trl_mean<-aggregate(. ~ trial_name , data=subset(q, select=-c(line)), mean, na.rm=TRUE, na.action= NULL)
  return(trl_mean)
}


###

