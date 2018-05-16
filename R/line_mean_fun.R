#' line_means function
#'
#' This function allows line means to be gathered accross year and trials, gives simple means
#' @param expt REQUIRED \code{data.frame} experiemtn description file from OAT DATA BASE
#' @param field REQUIRED \code{data.frame}  field phenotype file from OAT DATA BASE
#' @param qual REQUIRED \code{data.frame} quality phenotype file from OAT DATA BASE
#' @param traits REQUIRED \code{list} a list of traits to use
#' @param year OPTIONAL \code{list} list of years to use
#' @param lines OPTIONAL \code{list} list of lines to use
#' @param trial OPTIONAL \code{list} list of trials to use see OAT DATA BASE FOR CODE
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


line_means<-function(expt=NULL,
                     field=NULL,
                     qual=NULL,
                     traits=NULL,
                     year=NULL,
                     lines=NULL,
                     trial=NULL){
  ### function START
  expt<-expt
  field<-field
  qual<-qual
  traits<-traits
  year<-year
  lines<-lines
  trial<-trial
  ###
  out1<-data.pull(expt =expt, 
                  field= field, 
                  qual = qual, 
                  year=year, 
                  lines =lines ,
                  trial = trial )
  traits<-c('line', traits)
  out<-aggregate(. ~ line, data=out1[,..traits], mean, na.rm=TRUE, na.action= NULL)
  return(out)
}


###

