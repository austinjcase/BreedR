#' vt_analysis function
#'
#' This function allows 1 2 and 3 yr data reprots like PR24
#' @param exp_data REQUIRED \code{data.frame} experiemtn description file from OAT DATA BASE
#' @param fld_data REQUIRED \code{data.frame}  field phenotype file from OAT DATA BASE
#' @param qual_data REQUIRED \code{data.frame} quality phenotype file from OAT DATA BASE
#' @param tyial_type REQUIRED \code{vector} type of trial as described by the OAT DATA BASE  example "vt"
#' @param yr1 OPTIONAL \code{vector} first year of data to use example (2017) at least one value is required for yr1 or yr2
#' @param yr2 OPTIONAL \code{vector} first year of data to use example (2016) at least one value is required for yr1 or yr2
#' @param yr3 OPTIONAL \code{vector} first year of data to use example (2015)
#' @param trait_sel REQUIRED \code{vector} exact name of trait to use in anlysis example "yield"
#' @param exclude OPTIONAL \code{list} list of trial_names to exclude from the OAT DATA BASE example (c("vt_2017_stp", "vt_2016_stp"))
#' @param expt_obj REQUIRED \code{list} list of experimnetal objectives as defined from the OAT DATA BASE example (""agro"")
#' @param cut_offs OPTIONAL \code{list} list of critical values to drop of from trait data, where the first is the low and the second is the high example (c(50, 300) )
#'
#' @keywords vt_analysis
#' @export
#' @examples
#' vt_analysis_2

vt_analysis <- function (exp_data=NULL,fld_data=NUL, qual_data=NULL,
                         trial_type= NULL,yr1=NULL, yr2=NULL, yr3=NULL,
                         trait_sel =NULL,exclude=NULL,expt_obj=NULL,cut_offs=NULL)
{



  # required pacakges
  #####
  library("plyr") # good fuctions
  library("sqldf") # does joins for gathering data
  library("data.table") # data table for working with data
  library("rowr") # gives the cbind.fill

  #####

  #user specifics
  #####
  drops<-exclude # droped trails
  trait<-trait_sel # selected traits
  expt_type<-expt_obj # eperimental objective fromt the expt desc in the ODB
  crit_vals<-cut_offs # droping bad valus if user specifices
  #####

  # data setup pulls data using the data.pull function in BreedR
  #####
  all_vt<-data.pull(expt = exp_data, field = fld_data, qual = qual_data,
                    trial = trial_type, years = c(yr1, yr2, yr3))

  vt<-all_vt # for processing make new object
  all_vt<-setDT(all_vt)
  vt<-setDT(all_vt)

  #####

  # get list of lines by year
  #####
  vt1_list<-unique(subset(vt, year == yr1, select =line))
  vt2_list<-unique(subset(vt, year == yr2, select =line))
  vt3_list<-unique(subset(vt, year == yr3, select =line))
  vt2b_list<-Reduce(intersect, list(vt1_list$line,vt3_list$line)) #where 2b is yr1 and yr3 data
  vt2b_list<-setDT(as.data.frame(vt2b_list))
  names(vt2b_list)[1] <-"line"
  vt2c_list<-Reduce(intersect, list(vt2_list$line,vt3_list$line)) #where 2c is yr2 and yr3 data
  vt2c_list<-setDT(as.data.frame(vt2c_list))
  names(vt2c_list)[1] <-"line"

  # merge them to get 1yr 2yr and 3yr lines
  vt_lists<-sqldf("select * from vt1_list
                  left join vt2_list on vt1_list.line = vt2_list.line
                  left join vt3_list on vt1_list.line = vt3_list.line")

  # gather liens by lists for years
  vt_lists<-cbind.fill(vt1_list, vt2_list, vt3_list, vt_lists, vt2b_list, vt2c_list) # combine them
  colnames(vt_lists)<-c("list_1","list_2","list_3","yr1","yr2", "yr3", "yr2b", "yr2c") # rename them
  vt_lists<-as.matrix(vt_lists)
  vt_lists<-as.data.frame(vt_lists)
  #####


  # data prep for factor types
  #####
  vt<-all_vt
  vt$rep<-as.factor(vt$rep)
  vt$line<-as.factor(vt$line)
  vt$location<-as.factor(vt$location)
  vt$year<-as.factor(vt$year)
  vt$trial_name<-as.factor(vt$trial_name)
  #names(vt)
  vt<-as.data.frame(vt) # must now be data.frame
  vt$trait<-vt[, trait]
  #names(vt)
  vt<-setDT(vt)
  #q<-which(colnames(vt)== trait)  ## selects the trait for use
  #names(vt)
  #trait_name<-names(vt)[(names(vt) %in% trait)]
  #trait_name<-c(trait_name, "line")
  #vt$trait<-vt[,..q]
  #vt$trait<-vt[,..trait]
  vt$trait<-as.numeric(as.character(vt$trait))
  str(vt)
  #####

  # experimement drops drop experiment whcih do not meeti the expt_type param
  #####
  vt2<- subset(vt, trial_obj == expt_type) #drop all no agro trial_obj == agro
  if(is.null(drops)) {vt2<-vt2} else {vt2<-subset(vt2, ! trial_name %in% drops )} # drop out trails to exclude
  vt2<-setDT(vt2)

  rm(vt) # house keeping drpo traits which were not selected
  head(vt2)
  class(vt2)
  #####

  # drop bad values by crit val param
  #####
  vt2<-as.data.frame(vt2)
  if(!is.null(crit_vals)){
  test <- within(vt2, trait[trait < crit_vals[1]] <- NA)
  test <- within(test, trait[trait > crit_vals[2]] <- NA)
  vt2<-test
  rm(test)
  }

  vt2<-setDT(vt2)
  min(vt2$trait, na.rm=T)
  max(vt2$trait, na.rm=T)

#test<-subset(vt2, line == "STREAKER" & trial_name =="vt_2017_fef")

#  str(vt2)

   #####

  #need to drop locatiosn with no data for the trait
  #####
  qx<-aggregate(trait ~ trial_name , data=vt2, FUN=mean, na.rm=F, na.omit =F) # calc mean by loc
  ins<-qx$trial_name # get locs with data
  qxy<-subset(vt2, trial_name %in% ins) #sub set the locatiosn tih data means
  ins<-unique(qxy$trial_name) # list of locs with trait data
  qxys<-subset(vt2, ! trial_name %in% ins) # locs with no trait data
  outs<-unique(qxys$trial_name) # list of locs with no trait dat
  outs<-as.data.frame((outs)) # bind
  names(outs) [1]<-"trial_name" # processing
  means.table<-rbind.fill(outs,qx) # gives means table by trait
  names(means.table)[2]<-trait
  vt2<-subset(vt2, trial_name %in% ins) # filter by locs with trait data
  rm(qx, qxy, qxys) # house keeping
  #####

  # yr1 location averages
  #####
  sel<-yr1 # year to select first year
  data<-subset(vt2, year %in% sel) # filter by year
  lines1yr<- vt_lists$yr1 # list of the lines in both yr1
  data2<-subset(data, line %in% lines1yr )
  data2<-droplevels(data2)
  locs<-unique(data2$location) # list of locaions matching year criteria
  boot<-NULL # null boot vector to gather data


  for (i in 1:length(locs)) {
    loc<-locs[i] # selects location
    data3<-subset(data2, location %in% loc) # filter by location
    data4<-aggregate(trait ~ line +rep,FUN=mean, data=data3, na.rm=F, na.omit =F) #average by rep if more than one per rep
    data4<-droplevels(data4) # drop unusel lvl
    out<-lm(trait ~ line + rep   , data=data4) #anova procedure for single evnroment anova
    anova<-anova(out)
    number_reps<-length(unique(data4$rep)) # get number of reps
    lsd_05<-qnorm(p=0.975) * sqrt((anova$`Mean Sq`[3]*((1/ number_reps) + (1/number_reps))))  ## gives lsd 0.05
    lsd_10<-qnorm(p=0.95) * sqrt((anova$`Mean Sq`[3]*((1/ number_reps) + (1/number_reps))))  ## gives lsd 0.05
    means<-aggregate(trait ~ line ,FUN=mean, data=data4, na.rm=F, na.omit =F) # get line means
    trial_mean<-summary(data4$trait)[4] # get trail means
    final<-rbind(means, data.frame(line=c("lsd_05", "lsd_10", "mean"), trait=c(lsd_05, lsd_10, trial_mean) )) # bind results
    final$trial_name<-rep(unique(data3$trial_name), nrow(final)) # add trail names
    boot<-rbind(boot, final) # add to boot for loop
    rm(loc,data3,data4,out,anova,number_reps,lsd_05,lsd_10,means,trial_mean,final ) # house keeping
  }
  result_yr1<-boot # this is the final results for yr1's
  if (is.null (yr1)){ result_yr1<-NULL} # NULL IF yr1 is missing
 # return(result_yr1)
  rm(sel, data, lines1yr, data2, locs, boot) #house keeping
  #####

  # yr1 year state wide average's multiple locatios
  #####
  sel<-c(yr1) # select that year
  data<-subset(vt2, year %in% sel) # filter by year
  data<-droplevels(data) # drop unused
  data3<-aggregate(trait ~ line +rep+trial_name,FUN=mean, data=data, na.rm=F, na.omit =F) #average by rep if more than one per rep
  data3<-droplevels(data3) # drop unused
  out<-lm(trait ~ line *  rep%in%trial_name * trial_name  , data=data3) # anova with rep nested
  anova<-anova(out) # get anova but residuals are wrgon as nor witin blcok var, use EMS of Line:Trial_name
  number_enviro<-length(unique(data3$trial_name)) # get number of envrioment
  number_reps<-length(unique(data3$rep)) # get number of resp
  lsd_05<-qnorm(p=0.975) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.05)valeus
  lsd_10<-qnorm(p=0.95) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.10)valeus
  means<-aggregate(trait ~ line ,FUN=mean, data=data3, na.rm=F, na.omit =F) # get means of liens across locs
  trial_mean<-summary(data3$trait)[4] # overall mean
  final<-rbind(means, data.frame(line=c("lsd_05", "lsd_10", "mean"), trait=c(lsd_05, lsd_10, trial_mean) )) # bind up
  final$trial_name<- rep(paste(c("state_wide",sel), collapse = '_'), nrow(final)) # add trait name which is state wide
  result_yr1_state<-final
  #return(result_yr1_state)

  if (is.null (yr1)){ result_yr1_state<-NULL} # NULL IF yr1 is missing
  rm(sel, data, data3, out, anova, number_enviro, number_reps, lsd_05, lsd_10, means, trial_mean, final) # house keeping
  #####

  # two yr average
  # 2yr means could be yr1 and yr2 which is regular
  # so yr2  = yr1 and yr2 so "2017-2016"
  # or yr2b = yr1 and yr3 so "2017-2015"
  # or yr2c = yr2 and yr3 so "2016-2015"

  # yr2 regular single loc
  #####

  years<-c(yr1, yr2) # years we are looking at
  vt3<-subset(vt2, year %in% years) # subset by year
  sel<-aggregate(trait ~line +rep+trial_name , data=vt3, FUN=mean, na.rm=F) # get averages witin a locatsion
  vt_lists<-as.data.frame(vt_lists)
  lines2yr<- vt_lists$yr2 # list of the lines in both yr1 and yr2 = yr2
  data2<-subset(sel, line %in% lines2yr ) # only lines in both yr1 and yr2
  data2<-droplevels(data2) # drup unused
  data2$location<-gsub("vt_.*_", "",data2$trial_name) # add to get just locations data
  locs<-unique(data2$location)  # gets list of locatiosn with boty yr1 and yr2 data

    # loop to get 2yrs avers for locatiosn
  boot<-NULL

  for (i in 1:length(locs)) {
    loc<-locs[i]
    data2b<-subset(data2, location %in% loc) # filter by location
    number_locs<-length(unique(data2b$trial_name)) # get number of locs must be more than 1
    # can only do anova if greater than 1
    final<-NULL
    if(number_locs > 1){
    data3<-aggregate(trait ~ line +rep+trial_name,FUN=mean, data=data2b, na.rm=F, na.omit =F) #average by rep if more than one per rep
    data3<-droplevels(data3) # drop unused
    out<-lm(trait ~ line *  rep%in%trial_name * trial_name  , data=data3) # anova with rep nested
    anova<-anova(out) # get anova but residuals are wrgon as nor witin blcok var, use EMS of Line:Trial_name
    number_enviro<-length(unique(data3$trial_name)) # get number of envrioment
    number_reps<-length(unique(data3$rep)) # get number of resp
    lsd_05<-qnorm(p=0.975) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.05)valeus
    lsd_10<-qnorm(p=0.95) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.10)valeus
    means<-aggregate(trait ~ line ,FUN=mean, data=data3, na.rm=F, na.omit =F) # get means of liens across locs
    trial_mean<-summary(data3$trait)[4] # overall mean
    final<-rbind(means, data.frame(line=c("lsd_05", "lsd_10", "mean"), trait=c(lsd_05, lsd_10, trial_mean) )) # bind up
    final$trial_name<- rep(paste(c(loc,years), collapse = '_'), nrow(final)) # add trait name which is state wide
    }
    boot<-rbind(boot, final)
    final<-NULL
  }
  result_yr2<-boot
  if (is.null (yr1) | is.null (yr2)){ result_yr2<-NULL} # NULL IF yr1 or yr2 is missing
  rm(vt3, years,sel,lines2yr,data2, locs ,loc,
     data2b, data3,out,anova,number_enviro, number_locs, number_reps,lsd_05, lsd_10, means,trial_mean, boot) # house keeping
 # return(result_yr2)
  print("end 2yr reg")

  #####

  # yr2 regular state wide average
  #####
  vt3<-subset(vt2, year %in% c(yr1, yr2)) # subset by year
  years<-c(yr1, yr2) # years we are looking at
  sel<-aggregate(trait ~line +rep+trial_name , data=vt3, FUN=mean, na.rm=F) # get averages witin a locatsion
  lines2yr<- vt_lists$yr2 # list of the lines in both yr1 and yr2 = yr2
  data2<-subset(sel, line %in% lines2yr ) # only lines in both yr1 and yr2
  data2<-droplevels(data2) # drup unused
  vt3<-subset(vt2, year %in% c(yr1, yr2)) # subset by year
  years<-c(yr1, yr2) # years we are looking at
  sel<-aggregate(trait ~line +rep+trial_name , data=vt3, FUN=mean, na.rm=F) # get averages witin a locatsion
  lines2yr<- vt_lists$yr2 # list of the lines in both yr1 and yr2 = yr2
  data2<-subset(sel, line %in% lines2yr ) # only lines in both yr1 and yr2
  data3<-droplevels(data2) # drop unused
  out<-lm(trait ~ line *  rep%in%trial_name * trial_name  , data=data3) # anova with rep nested
  anova<-anova(out) # get anova but residuals are wrgon as nor witin blcok var, use EMS of Line:Trial_name
  number_enviro<-length(unique(data3$trial_name)) # get number of envrioment
  number_reps<-length(unique(data3$rep)) # get number of resp
  lsd_05<-qnorm(p=0.975) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.05)valeus
  lsd_10<-qnorm(p=0.95) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.10)valeus
  means<-aggregate(trait ~ line ,FUN=mean, data=data3, na.rm=F, na.omit =F) # get means of liens across locs
  trial_mean<-summary(data3$trait)[4] # overall mean
  final<-rbind(means, data.frame(line=c("lsd_05", "lsd_10", "mean"), trait=c(lsd_05, lsd_10, trial_mean) )) # bind up
  final$trial_name<- rep(paste(c("state_wide",years), collapse = '_'), nrow(final)) # add trait name which is state wide
  result_yr2_state<-final
  #return(result_yr2_state)
  if (is.null (yr1) | is.null (yr2)){ result_yr2_state<-NULL} # NULL IF yr1 is missing
  rm(sel, data3, out, anova, number_enviro, number_reps,years, lsd_05, lsd_10, means, trial_mean, final) # house keeping
  print("line 283")

  #####

  # yr2b  single loc
  #  yr2b = yr1 and yr3 so "2017-2015"
  #####
  years<-c(yr1, yr3) # years we are looking at
  vt3<-subset(vt2, year %in% years) # subset by year
  sel<-aggregate(trait ~line +rep+trial_name , data=vt3, FUN=mean, na.rm=F) # get averages witin a locatsion
  lines2yr<- vt_lists$yr2b # list of the lines in both yr1 and yr3
  data2<-subset(sel, line %in% lines2yr ) # only lines in both yr1 and yr2
  data2<-droplevels(data2) # drup unused
  data2$location<-gsub("vt_.*_", "",data2$trial_name) # add to get just locations data
  locs<-unique(data2$location)  # gets list of locatiosn with boty yr1 and yr3 data
  # loop to get 2yrs avers for locatiosn
  boot<-NULL

  for (i in 1:length(locs)) {
    loc<-locs[i]
    data2b<-subset(data2, location %in% loc) # filter by location
    number_locs<-length(unique(data2b$trial_name)) # get number of locs must be more than 1
    number_locs
    # can only do anova if greater than 1
    final<-NULL
    if(number_locs > 1){
    data3<-aggregate(trait ~ line +rep+trial_name,FUN=mean, data=data2b, na.rm=F, na.omit =F) #average by rep if more than one per rep
    data3<-droplevels(data3) # drop unused
    out<-lm(trait ~ line *  rep%in%trial_name * trial_name  , data=data3) # anova with rep nested
    anova<-anova(out) # get anova but residuals are wrgon as nor witin blcok var, use EMS of Line:Trial_name
    number_enviro<-length(unique(data3$trial_name)) # get number of envrioment
    number_reps<-length(unique(data3$rep)) # get number of resp
    lsd_05<-qnorm(p=0.975) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.05)valeus
    lsd_10<-qnorm(p=0.95) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.10)valeus
    means<-aggregate(trait ~ line ,FUN=mean, data=data3, na.rm=F, na.omit =F) # get means of liens across locs
    trial_mean<-summary(data3$trait)[4] # overall mean
    final<-rbind(means, data.frame(line=c("lsd_05", "lsd_10", "mean"), trait=c(lsd_05, lsd_10, trial_mean) )) # bind up
    final$trial_name<- rep(paste(c(loc,years), collapse = '_'), nrow(final)) # add trait name which is state wide
    }
    boot<-rbind(boot, final)
    final<-NULL
    rm(final)
  }
  result_yr2b<-boot
  if (is.null (yr1)|is.null (yr3) ){ result_yr2b<-NULL} # NULL IF yr1  or yr3 is missing
  rm(vt3, years,sel,lines2yr,data2, locs ,loc,
     data2b, data3,out,anova,number_enviro,  number_reps,lsd_05, lsd_10, means,trial_mean, boot) # house keeping
  print("line 330")

   #return(result_yr2b)
  #####

  # yr2b  state wide average
  #  yr2b = yr1 and yr3 so "2017-2015"
  #####
  years<-c(yr1, yr3) # years we are looking at
  vt3<-subset(vt2, year %in% years) # subset by year
  sel<-aggregate(trait ~line +rep+trial_name , data=vt3, FUN=mean, na.rm=F) # get averages witin a locatsion
  lines2yr<- vt_lists$yr2b # list of the lines in both yr1 and yr3
  data2<-subset(sel, line %in% lines2yr ) # only lines in both yr1 and yr3
  data2<-droplevels(data2) # drup unused
  data3<-droplevels(data2) # drop unused
  out<-lm(trait ~ line *  rep%in%trial_name * trial_name  , data=data3) # anova with rep nested
  anova<-anova(out) # get anova but residuals are wrgon as nor witin blcok var, use EMS of Line:Trial_name
  number_enviro<-length(unique(data3$trial_name)) # get number of envrioment
  number_reps<-length(unique(data3$rep)) # get number of resp
  lsd_05<-qnorm(p=0.975) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.05)valeus
  lsd_10<-qnorm(p=0.95) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.10)valeus
  means<-aggregate(trait ~ line ,FUN=mean, data=data3, na.rm=F, na.omit =F) # get means of liens across locs
  trial_mean<-summary(data3$trait)[4] # overall mean
  final<-rbind(means, data.frame(line=c("lsd_05", "lsd_10", "mean"), trait=c(lsd_05, lsd_10, trial_mean) )) # bind up
  final$trial_name<- rep(paste(c("state_wide",years), collapse = '_'), nrow(final)) # add trait name which is state wide
  result_yr2_b_state<-final
  if (is.null (yr1)|is.null (yr3) ){ result_yr2_b_state<-NULL} # NULL IF yr1 or yr3 is missing
  rm(sel, data3, out, anova, number_enviro, number_reps,years, lsd_05, lsd_10, means, trial_mean, final) # house keeping
  #####

  # yr2c single loc
  #  yr2c = yr2 and yr3 so "2016-2015"
  #####
  years<-c(yr2, yr3) # years we are looking at
  vt3<-subset(vt2, year %in% years) # subset by year
  sel<-aggregate(trait ~line +rep+trial_name , data=vt3, FUN=mean, na.rm=F) # get averages witin a locatsion
  lines2yr<- vt_lists$yr2c # list of the lines in both yr2 and yr3
  data2<-subset(sel, line %in% lines2yr ) # only lines in both yr1 and yr2
  data2<-droplevels(data2) # drup unused
  data2$location<-gsub("vt_.*_", "",data2$trial_name) # add to get just locations data
  locs<-unique(data2$location)  # gets list of locatiosn with boty yr1 and yr3 data
  # loop to get 2yrs avers for locatiosn
  boot<-NULL

  for (i in 1:length(locs)) {
    loc<-locs[i]
    data2b<-subset(data2, location %in% loc) # filter by location
    number_locs<-length(unique(data2b$trial_name)) # get number of locs must be more than 1
    # can only do anova if greater than 1
    final<-NULL
    if(number_locs > 1){
    data3<-aggregate(trait ~ line +rep+trial_name,FUN=mean, data=data2b, na.rm=F, na.omit =F) #average by rep if more than one per rep
    data3<-droplevels(data3) # drop unused
    out<-lm(trait ~ line *  rep%in%trial_name * trial_name  , data=data3) # anova with rep nested
    anova<-anova(out) # get anova but residuals are wrgon as nor witin blcok var, use EMS of Line:Trial_name
    number_enviro<-length(unique(data3$trial_name)) # get number of envrioment
    number_reps<-length(unique(data3$rep)) # get number of resp
    lsd_05<-qnorm(p=0.975) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.05)valeus
    lsd_10<-qnorm(p=0.95) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.10)valeus
    means<-aggregate(trait ~ line ,FUN=mean, data=data3, na.rm=F, na.omit =F) # get means of liens across locs
    trial_mean<-summary(data3$trait)[4] # overall mean
    final<-rbind(means, data.frame(line=c("lsd_05", "lsd_10", "mean"), trait=c(lsd_05, lsd_10, trial_mean) )) # bind up
    final$trial_name<- rep(paste(c(loc,years), collapse = '_'), nrow(final)) # add trait name which is state wide
    }
    boot<-rbind(boot, final)
    final<-NULL
    rm(final)
  }
  result_yr2c<-boot
  if (is.null (yr2) | is.null(yr3)){ result_yr2c<-NULL} # NULL IF yr2 or yr 3is missing
  rm(vt3, years,sel,lines2yr,data2, locs ,loc,
     data2b, data3,out,anova,number_enviro,  number_reps,lsd_05, lsd_10, means,trial_mean, boot) # house keeping
  print("line 402")
   #####

  # yr2c  state wide average
  #  yr2c = yr2 and yr3 so "2016-2015"
  #####
  years<-c(yr2, yr3) # years we are looking at
  vt3<-subset(vt2, year %in% years) # subset by year
  sel<-aggregate(trait ~line +rep+trial_name , data=vt3, FUN=mean, na.rm=F) # get averages witin a locatsion
  lines2yr<- vt_lists$yr2c # list of the lines in both yr1 and yr3
  data2<-subset(sel, line %in% lines2yr ) # only lines in both yr1 and yr3
  data2<-droplevels(data2) # drup unused
  data3<-droplevels(data2) # drop unused
  out<-lm(trait ~ line *  rep%in%trial_name * trial_name  , data=data3) # anova with rep nested
  anova<-anova(out) # get anova but residuals are wrgon as nor witin blcok var, use EMS of Line:Trial_name
  number_enviro<-length(unique(data3$trial_name)) # get number of envrioment
  number_reps<-length(unique(data3$rep)) # get number of resp
  lsd_05<-qnorm(p=0.975) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.05)valeus
  lsd_10<-qnorm(p=0.95) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.10)valeus
  means<-aggregate(trait ~ line ,FUN=mean, data=data3, na.rm=F, na.omit =F) # get means of liens across locs
  trial_mean<-summary(data3$trait)[4] # overall mean
  final<-rbind(means, data.frame(line=c("lsd_05", "lsd_10", "mean"), trait=c(lsd_05, lsd_10, trial_mean) )) # bind up
  final$trial_name<- rep(paste(c("state_wide",years), collapse = '_'), nrow(final)) # add trait name which is state wide
  result_yr2_c_state<-final
  if (is.null (yr2) | is.null(yr3)){ result_yr2_c_state<-NULL} # NULL IF yr2 or yr3 is missing
  rm(sel, data3, out, anova, number_enviro, number_reps,years, lsd_05, lsd_10, means, trial_mean, final) # house keeping
  #####

  #  3yr average
  # yr1 yr2 yr3

  # yr3 regular single loc
  #####
  years<-c(yr1, yr2, yr3) # years we are looking at
  vt3<-subset(vt2, year %in% years) # subset by year
  sel<-aggregate(trait ~line +rep+trial_name , data=vt3, FUN=mean, na.rm=F) # get averages witin a locatsion
  lines3yr<- vt_lists$yr3 # list of the lines in both yr1 and yr2 = yr2
  data2<-subset(sel, line %in% lines3yr ) # only lines in both yr1 and yr2 and yr3
  data2<-droplevels(data2) # drup unused
  data2$location<-gsub("vt_.*_", "",data2$trial_name) # add to get just locations data
  locs<-unique(data2$location)  # gets list of locatiosn with boty yr1 and yr2 data
  boot<-NULL

  for (i in 1:length(locs)) {
    loc<-locs[i]
    data2b<-subset(data2, location %in% loc) # filter by location

    number_locs<-length(unique(data2b$trial_name)) # get number of locs must be more than 1
    # can only do anova if greater than 1
    final<-NULL
    if(number_locs > 1){
    data3<-aggregate(trait ~ line +rep+trial_name,FUN=mean, data=data2b, na.rm=F, na.omit =F) #average by rep if more than one per rep
    data3<-droplevels(data3) # drop unused
    out<-lm(trait ~ line *  rep%in%trial_name * trial_name  , data=data3) # anova with rep nested
    anova<-anova(out) # get anova but residuals are wrgon as nor witin blcok var, use EMS of Line:Trial_name
    number_enviro<-length(unique(data3$trial_name)) # get number of envrioment
    number_reps<-length(unique(data3$rep)) # get number of resp
    lsd_05<-qnorm(p=0.975) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.05)valeus
    lsd_10<-qnorm(p=0.95) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.10)valeus
    means<-aggregate(trait ~ line ,FUN=mean, data=data3, na.rm=F, na.omit =F) # get means of liens across locs
    trial_mean<-summary(data3$trait)[4] # overall mean
    final<-rbind(means, data.frame(line=c("lsd_05", "lsd_10", "mean"), trait=c(lsd_05, lsd_10, trial_mean) )) # bind up
    final$trial_name<- rep(paste(c(loc,years), collapse = '_'), nrow(final)) # add trait name which is state wide
    }
    boot<-rbind(boot, final)
    final<-NULL
    rm(final)
  }
  result_yr3<-boot
  if (is.null (yr1) | is.null (yr2) | is.null(yr3)){ result_yr3<-NULL} # NULL IF yr1 yr2 or y3 is missing
  rm(vt3, years,sel,lines3yr,data2, locs ,loc,
     data2b, data3,out,anova,number_enviro,  number_reps,lsd_05, lsd_10, means,trial_mean, boot) # house keeping
  print("line 472")
   #####

  # yr3  state wide average
  #####
  years<-c(yr1, yr2, yr3) # years we are looking at
  vt3<-subset(vt2, year %in% years) # subset by year
  sel<-aggregate(trait ~line +rep+trial_name , data=vt3, FUN=mean, na.rm=F) # get averages witin a locatsion
  lines3yr<- vt_lists$yr3 # list of the lines in both yr1 and yr2 and yr 3
  data2<-subset(sel, line %in% lines3yr ) # only lines in both yr1 and yr2 and yr3
  sel<-aggregate(trait ~line +rep+trial_name , data=vt3, FUN=mean, na.rm=F) # get averages witin a locatsion
  data3<-droplevels(data2) # drop unused
  out<-lm(trait ~ line *  rep%in%trial_name * trial_name  , data=data3) # anova with rep nested
  anova<-anova(out) # get anova but residuals are wrgon as nor witin blcok var, use EMS of Line:Trial_name
  number_enviro<-length(unique(data3$trial_name)) # get number of envrioment
  number_reps<-length(unique(data3$rep)) # get number of resp
  lsd_05<-qnorm(p=0.975) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.05)valeus
  lsd_10<-qnorm(p=0.95) * sqrt((anova$`Mean Sq`[4]*(((1/(number_enviro* number_reps))+(1/(number_enviro* number_reps))))))  # gives the lsd (0.10)valeus
  means<-aggregate(trait ~ line ,FUN=mean, data=data3, na.rm=F, na.omit =F) # get means of liens across locs
  trial_mean<-summary(data3$trait)[4] # overall mean
  final<-rbind(means, data.frame(line=c("lsd_05", "lsd_10", "mean"), trait=c(lsd_05, lsd_10, trial_mean) )) # bind up
  final$trial_name<- rep(paste(c("state_wide",years), collapse = '_'), nrow(final)) # add trait name which is state wide
  result_yr3_state<-final
  if (is.null (yr1) | is.null (yr2) | is.null(yr3)){ result_yr3_state<-NULL} # NULL IF yr1 or yr2 or yr3 is missing
  rm(sel, lines3yr,data3, out, anova, number_enviro, number_reps,years, lsd_05, lsd_10, means, trial_mean, final) # house keeping
  #####

  # bind up final product
  #####
  final_final<-rbind(result_yr1,result_yr1_state,
                     result_yr2, result_yr2_state,
                     result_yr2b, result_yr2_b_state,
                     result_yr2c, result_yr2_c_state,
                     result_yr3, result_yr3_state )

  names(final_final)[2]<-trait # rename trait to user spec for trait
  rm(list=setdiff(ls(), "final_final"))
  print("it is done")

  return(final_final) # return the final_final product

  #####

  }

