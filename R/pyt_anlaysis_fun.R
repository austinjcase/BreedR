#' pyt_analysis function
#'
#' This function allows to do pyt anlysis of aibd type designs within single years
#' output is a tall file with the trait values from each location given as adjusted BLUES based on the checks.
#' values of t0.05 are an LSD value for if they test line gives a "HIGHER" trait value than the selected line to beat
#' where 1= higher than check at alpha 0.05. Where State is the overall average of all locations
#'
#' @param exp_data REQUIRED \code{data.frame} experiemtn description file from OAT DATA BASE
#' @param fld_data REQUIRED \code{data.frame}  field phenotype file from OAT DATA BASE
#' @param qual_data REQUIRED \code{data.frame} quality phenotype file from OAT DATA BASE
#' @param trls REQUIRED \code{vector} type of trial as described by the OAT DATA BASE  example "pyt"
#' @param trait REQUIRED \code{vector} trait to be used example "yield"
#' @param drop.locs OPTIONAL \code{vector} vector of trial_names to drop eample "c("pyt_2017_stp" , "pyt_2017_btn")"
#' @param check.to.top REQUIRED \code{vector} check to test if signficantly better than example "DEON"
#'
#' @keywords pyt_analysis
#' @export
#' @examples
#'pyt_analysis

pyt_analysis <- function (exp_data=NULL,fld_data=NULL, qual_data=NULL,
                         trls= NULL,yrs=NULL,
                         trait=NULL,drop.locs=NULL,
                         check.to.top=NULL)
{

  exp_data=exp_data
  fld_data=fld_data
  qual_data=qual_data
  trls= trls
  yrs=yrs
  trait=trait
  drop.locs=drop.locs
  check.to.top=check.to.top

  ### required pacakges
  library(BreedR)
  library(dplyr)
  library(plyr)
  library(lme4)
  ###

  #pull out data by the selection
  #####
  raw.data<-data.pull(expt = exp_data, field = fld_data, qual = qual_data,
                      trial = trls, years = yrs)
  data<-raw.data
  data<-as.data.frame(data)
  data<-subset(data, ! trial_name  %in% drop.locs) # filter by locs in drop.locs param
  # just keep line, block, trait, line_code, and trail_name
  data$trait<-data[, trait] # select trait of interest
  data<-data[c('line', 'block', 'trait', 'line_code', 'trial_name')]
  #make data correct types
  str(data)
  data$trait<-as.numeric(data$trait)
  data$block<-as.factor(data$block)
  data$line_code<-as.factor(data$line_code)
  data$line<-as.factor(data$line)
  data$trial_name<-as.factor(data$trial_name)

  # drop locations with no trait data
  qx<-NULL
  qx<-aggregate(trait ~ trial_name , data=data, FUN=mean, na.rm=F, na.omit =F) # calc mean by loc
  ins<-qx$trial_name # get locs with data
  qxy<-subset(data, trial_name %in% ins) #sub set the locatiosn tih data means
  ins<-unique(qxy$trial_name) # list of locs with trait data
  qxys<-subset(data, ! trial_name %in% ins) # locs with no trait data
  outs<-unique(qxys$trial_name) # list of locs with no trait dat
  outs<-as.data.frame((outs)) # bind
  names(outs) [1]<-"trial_name" # processing
  means.table<-rbind.fill(outs,qx) # gives means table by trait
  names(means.table)[2]<-trait
  data<-subset(data, trial_name %in% ins) # filter by locs with trait data
  data<-droplevels(data)

  #####


  # now by location
  locs<-unique(data$trial_name) # selected locs

  #####
  boot<-NULL
  for (i in 1:length(locs)) {
    loc<-locs[i] # location
    data2<-subset(data,trial_name %in% loc ) # data for just that loc
    data2<-droplevels(data2)
    unique(data2$trial_name)
    data2<-as.data.frame(data2)
    checks<-subset(data2, line_code != 0)
    checks<-droplevels(checks)
    #
    # use mixed model to get block effect with both ran line and block
    out2<-lmer(trait ~ (1|line) + (1|block), data = checks) # treat both lines and blocks as random
    rans<-ranef(out2) # ran effects solution for block for adjustment
    rans<-rans$block # block effects
    rans$block<-row.names(rans)
    names(rans)[1]<-'effect'
    #
    # adjust values by the ran effect solution
    data3 <-data2 # data3 is for adjustment
    data4<- join(data3, rans, by = 'block') # intermindate step
    data3<-data4
    rm(data4)
    data3$trait.adj<-data3$trait-data3$effect # subtract effect form trait values
    #
    # doing to get a lsd value for check comparision
    checks2<-subset(data3, line_code != 0)
    checks2<-droplevels(checks2)
    out<-lm(trait.adj ~ line + block, data=checks2) #aov
    anova<-anova(out)
    anova$`Mean Sq`[3] # LSI val
    anova$Df[1]*anova$Df[2]
    crit_val<-qt(c(.05, .95), df=anova$Df[1]*anova$Df[2] ) [2] # t crit vale
    LSI<-crit_val*
      sqrt(
        (((anova$Df[2]+2) *(anova$Df[1]+2) *(anova$`Mean Sq`[3]))
         /((anova$Df[2]+1) *(anova$Df[1]+1)))
      )
    x<-aggregate(trait ~  line, data =checks2, FUN=mean)
    comp<-x[x$line == check.to.top,] [2] + LSI  # compair to user check.to.top param
    entries<-subset(data3, line_code == 0)
    entries$crit<-rep(comp, nrow(entries))
    entries$t0.05<-ifelse(entries$trait.adj > entries$crit, 1, 0 )  # if it beats the check give a 1 if not 0
    #
    # processing to get final values
    loc.done<-entries[, c('line','trait.adj', 'line_code', 'trial_name', 't0.05')]
    checks.final<-aggregate(trait.adj ~  line, data =checks2, FUN=mean)
    checks.final<-join(checks.final, data3, by ='line', match = 'first')
    checks.final<-checks.final[, c('line','trait.adj', 'line_code', 'trial_name')]
    final<-rbind.fill(loc.done,checks.final)
    #colnames(final)[colnames(final)=='trait.adj'] <- trait # rename check
    boot<-rbind(boot,final) # bind to boot
    rm(data2) # drop not unneeded
    rm(data3) # drop not unneeded
    rm(final)
  }
  #####


  # now across all locations
  #####
  data2<-as.data.frame(data)
  checks<-subset(data2, line_code != 0)
  checks<-droplevels(checks)
  # use mixed model to get block effect with both ran line and block
  out2<-lmer(trait ~ (1|line) * (1|trial_name)  , data = checks) # treat lines and tralis as rand get trail effects
  rans<-ranef(out2) # ran effects solution for trail adjustment
  rans<-rans$trial_name # trial effects
  rans$trial_name<-row.names(rans)
  names(rans)[1]<-'effect'
  #
  # adjust values by the ran effect solution
  data3<- join(boot, rans, by = 'trial_name') # intermindate step to join with boot from singl loc data
  data3$trait.adj.overall<-data3$trait.adj-data3$effect # subtract effect form trait values
  #
  # doing to get a lsd value for check comparision
  checks2<-subset(data3, line_code != 0)
  checks2<-droplevels(checks2)
  out<-lm(trait.adj ~ line *trial_name , data=checks2) #aov
  anova<-anova(out)
  crit_val<-qt(c(.05, .95), df=anova$Df[1]*anova$Df[2] ) [2] # t crit vale
  LSI<-crit_val*
    sqrt(
      (((anova$Df[2]+2) *(anova$Df[1]+2) *(anova$`Mean Sq`[3]))
       /((anova$Df[2]+1) *(anova$Df[1]+1)))
    )
  x<-aggregate(trait.adj ~  line, data =checks2, FUN=mean)
  comp<-x[x$line == check.to.top,] [2] + LSI  # compair to user check.to.top param
  rm(x)
  entries<-subset(data3, line_code == 0)
  entries.overall<-aggregate(trait.adj.overall ~  line, data =entries, FUN=mean)
  rm(entries)
  entries.overall$crit<-rep(comp, nrow(entries.overall))
  entries.overall$t0.05.overall<-ifelse(entries.overall$trait.adj > entries.overall$crit, 1, 0 )  # if it beats the check give a 1 if not 0
  #
  #####

  # data processing to get final results
  checks.overall<-aggregate(trait.adj.overall ~  line, data =checks2, FUN=mean)
  final.overall<-rbind.fill(entries.overall, checks.overall)
  final.overall<-merge(x = final.overall, y = data2[ , c("line","line_code")], by = "line", all.x=F)
  final.overall <- final.overall[!duplicated(final.overall$line),]
  colnames(final.overall)[colnames(final.overall)=='trait.adj.overall'] <- trait # rename check
  colnames(final.overall)[colnames(final.overall)=='t0.05.overall'] <- "t0.05" # rename check
  final.overall$crit<-NULL
  final.overall$trial_name<-rep("state", nrow(final.overall))
  colnames(boot)[colnames(boot)=='trait.adj'] <- trait # rename check
  #
  #makes final data vector
  final_final<-rbind(boot,final.overall)
  head(final_final)
  #

  if (is.null(qx)) {
    final_final<-NULL
  }

  if (is.null(qx)) {
    warning ( "trait has no data")
  }
  return (final_final)

}






