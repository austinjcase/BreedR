#' line_means function
#'
#' This function allows line means to be gathered accross year and trials, gives simple means
#' @param expt.impt REQUIRED \code{data.frame} experiemtn description file from OAT DATA BASE
#' @param field.impt REQUIRED \code{data.frame}  field phenotype file from OAT DATA BASE
#' @param qual.impt REQUIRED \code{data.frame} quality phenotype file from OAT DATA BASE
#' @param traits.impt REQUIRED \code{list} a list of traits to use
#' @param year.impt OPTIONAL \code{list} list of years to use
#' @param lines.impt OPTIONAL \code{list} list of lines to use
#' @param trial.impt OPTIONAL \code{list} list of trials to use see OAT DATA BASE FOR CODE
#' 
#' @keywords line_means
#' @export
#' @examples
#' 
#' 
#' 
#' out2<-line_means(expt=exp_odb,
#'                field=fld_odb,
#'               qual=qual_odb,
#'               traits=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'),
#'               year=c(2017, 2016),
#'               lines=c('DEON','SABER'),
#'               trial=c('vt','pyt'))
#' 
#' out2
#'
#'
#'out5<-line_means(expt=exp_odb,
#'                 field=fld_odb,
#'                 qual=qual_odb,
#'                 traits=c('yield', 'test_weight', 'groat_glucan', 'cr_sev'))
#'
#'out5
#' 
#' line_means


line_means<-function(expt.impt=NULL,
                     field.impt=NULL,
                     qual.impt=NULL,
                     traits.impt=NULL,
                     year.impt=NULL,
                     lines.impt=NULL,
                     trial.impt=NULL){
  ### function START
 expt<-expt.impt
  field<-field.impt
  qual<-qual.impt
  #traits<-traits.impt
  year<-year.impt
  lines<-lines.impt
  trial<-trial.impt
  ###
  
  out1<-data.pull(expt =expt, 
                  field= field, 
                  qual = qual, 
                  year=year, 
                  lines =lines ,
                  trial = trial )
  traits<-c('line', traits.impt)
  out1<-as.data.frame(out1)
  q<-out1[,traits]
  out<-aggregate(. ~ line, data=q, mean, na.rm=TRUE, na.action= NULL)
  return(out)
}


###

