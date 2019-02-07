#' Generate a randomized complete block design
#'
#' @description Randomized complete block designs (RCBD) are exceedingly common experimental designs in many field, including agricultural reserach. Details on RCBDs can easily be found in basic experimental design textbooks or an internet search; briefly, a set of experimental entries along with check lines, if desired, comprise a single block. This block is then replicated in its entirety at least once (i.e. two blocks) and up to any number of times.
#' @param enviro Optional \code{character string} describing the environment which the field trial will be grown in. Default is the current date and time.
#' @param exp.name Required \code{character string} identifying name for the experiment.
#' @param nBlks Optional \code{Integer} indicating the number of complete blocks desired. Default is \code{2}. If a single replication is desired to grow across multiple environments (aka locations, see \code{Details} for further information) then set nBlks to \code{1}.
#' @param entries A \code{character vector} of experimental (i.e. not check) entries. User must either provide this \strong{OR} \code{nEntries} (see next).
#' @param nEntries \code{Integer} argument indicating how many experimental entries are to be included in the trial. If \code{nEntries} = \emph{m}, then \emph{m} generic entry names will be generated and included in the output. User must either provide this \strong{OR} \code{entries}.. \code{nEntries} will be superceded if \code{entries} is provided.
#' @param chks A \code{character vector} including the set of check lines to be included. User must either provide this \strong{OR} \code{nChks} (see next).
#' @param nChks \code{Integer} argument indicating how many check lines (see \code{Details}) are to be included in the trial. DUser must either provide this \strong{OR} \code{chks}.
#' @param nChkReps Optional \code{Integer} argument indicating how many times each check line should be replicated \strong{per block}. Default is \code{1} meaning that each check line will appear in each block twice (i.e. replicated once).
#' @param nFieldRows Optional, but must be provided if nFieldCols is not. \code{Integer} argument indicating the number of field rows (dimension 'a' in Figure 1).This value can be calculated internally by passing only \code{nFieldCols}.
#' @param nFieldCols Optional, but must be provided if nFieldRows is not \code{integer} argument indicating the number of field columns (dimension 'b',Figure 1). This value can be calculated internally by passing only \code{nFieldRows}.
#' @param plot.start Optional \code{integer} indicating the identification number of the first plot. \code{design.aibd} places the first plot in the "bottom-left" corner of the field and numbering then serpentines, starting to the right (Figure 1). Default is \code{1001}.
#' @param plot_id.template Optional \code{character vector} of length two. The first character is a string that will preceed the plot number in the plot_id field of the output, and the second character is the character to delimit the provided string and the plot number. For example, c("EnvA_TrialA", "_") will result in "EnvA_TrialA_1001", etc.
#' @param fillWithEntry Optional \code{Logical}. Default is \code{TRUE}, meaning fill plots (i.e. plots remaining after the defined sets of experimental entries and check lines are assigned) will be replaced by randomly-selected experimental entries. A "D" will appear in the output next to these replicated entries.
#' @param fillWithChk Optional \code{Logical}. If \code{TRUE} this will supercede \code{fillWithEntry} and any fill plots will be replaced by a balanced set of check lines. Default is \code{FALSE}.
#' @param dup.list Optional \code{character vector} containing the candidates that could be replicated if \code{fillWithEntry = TRUE}. This is useful if there is a low amount of source seed for some experimental entries; those, for example, would be excluded from \code{dup.list}. Default is \code{NULL}, meaning that all entries are candidates for duplication.
#' @details Figure 1 presents a simplified version of an RCBD with two replications within an environment where each block is a rectangle and there are no fill plots. In reality a block may end and the next start in the middle of a row; this is done to minimize the size of the experiment while meeting all requirements.
#'
#'
#'          \if{html}{\figure{rcbd_Fig1.jpg}}
#'  RCBD may not be well-suited for all circumstances; in these cases consider using other design functions included in \link{FldTrial}: \link{design.aibd} or \link{design.mad}.
#'
#'
#'  @return A \code{list} containing:
#'          \itemize{
#'            \item \code{rcbd.dgn} A \code{dataframe} of the resulting design. A line code of 0 denotes an experimental entry, codes of 1, 2, ..., (nChks) denote the first, second, up to the (nChks)-th check, respectively. The order of the check lines and thus line code is arbitrary and has no bearing on placement or randomization.
#'                                  If \code{fillWithEntry} is \code{TRUE} then a  "D" will be placed in the "Duplicates" column for all duplicated entries.
#'                                  Note that the column name "replication" is used in \code{rcbd.dgn} rather than "block" or "blk" for reasons related to compatibility with a preferred data entry software.
#'            \item \code{plot.layout} A \code{matrix} of plot numbers reflecting the field layout. The first column contains field row numberis (i.e. 1:\code{nFieldRows}) and the last row contains field column numbers (i.e. 1:\code{nFieldCols}). See Figure 1.
#'            \item \code{check.layout} A \code{matrix} of line codes reflecting the field layout. See \code{plot.layout} for further details.
#'            \item \code{rep.layout} A \code{matirx} of block assignments reflecting the field layout. See \code{plot.layout} for further details.
#'            \item \code{field.dim} The dimensions of the resulting field design; nFieldRows x nFieldCols, or a x b (Figure 1).
#'            \item \code{nDuplicates} The number of duplicated experimental entries. Will only be non-zero if \code{fillWithEntry} is \code{TRUE}.
#'            \item \code{rlzPerChks} The final percentage of the designed represented by check lines. This can be affected by nChkReps and fillWithCheck
#'          }
#' @references
#'        Bernardo, Rex. 2010. Breeding for Quantitative Traits in Plants. Stemma Press. Woodbury, MN.
#'
#' @examples
#' \dontrun{
#' ## Example 1 - Generic layout with 2 reps
#' rcbd.ex1 <- design.rcbd(exp.name = "ex1", nBlks = 2,
#'                nEntries = 80, nChks = 3, nChkReps = 3,
#'                nFieldRows = 8)
#'
#' ## Example 2 - Layout with defined lines and checks
#' rcbd.ex2 <- design.rcbd(exp.name = "ex2", nBlks = 2,
#'                entries = paste("Line", 1:75, sep="."),
#'                chks = c("Larry", "Curly", "Moe"),
#'                nChkReps = 4, nFieldCols = 20)
#' }
#' @export
#'


design.rcbd <- function(enviro=format(Sys.Date(), "%x"), exp.name=NULL, nBlks=2, entries= NULL, nEntries= NULL, chks=NULL, nChks=0, nChkReps=1, nFieldRows=NULL, 
                        nFieldCols=NULL, plot.start=1001, fillWithEntry=T, fillWithChk=F, dup.list=NULL, plot_id.template=NULL){

  ## QC
  if(is.null(entries) & is.null(nEntries)) stop("Must provide an entry list (entries=) OR the number of entries desired (nEntries=).")

  if(is.null(chks) & nChks == 0) cat("\nProceeding without checks\n")

  if(all(is.null(nFieldRows), is.null(nFieldCols))) stop("Provide the number of rows (sometimes called ranges or beds) (nFieldRows=)")

  ## Develop other non-input functions parameters
  if(is.null(exp.name)) exp.name <- "You_should_have_named_your_experiment"; trial <- paste(exp.name, enviro, sep="_")
  if(!is.null(entries)){entries <- as.character(as.matrix(entries)); nEntries <- length(entries)} else entries <- paste("entry", 1:nEntries, sep="_")
  if(!is.null(chks)) chks <- as.character(as.matrix(chks)) else if(nChks > 0) chks <- paste("chk", 1:nChks, sep="_")
  if(!is.null(dup.list)) dup.list <- as.character(as.matrix(dup.list)) else{dup.list <- entries}

  nChks <- length(chks) ## Will stay 0 if chks = NULL and nChks = 0

  entry.mat <- cbind(line_name=c(unique(entries), chks, "FILLER"), entry=c(1:(length(unique(entries)) + nChks), NA)) ## This is for field book

  if(fillWithChk) fillWithEntry <- FALSE
  if(fillWithEntry) fillWithChk <- FALSE
  if(all(fillWithEntry, fillWithChk)) stop("Select either fillWithEntry or fillWithCheck")

  nTotal <- nBlks*(nEntries + nChks*nChkReps)

  if(is.null(nFieldCols)) nFieldCols <- ceiling(nTotal / nFieldRows) ## One of these must be provided, checked above
  if(is.null(nFieldRows)) nFieldRows <- ceiling(nTotal / nFieldCols)

  exp.size <- nFieldCols * nFieldRows ## This includes all blks (i.e. reps)
  if(exp.size < nTotal) stop("The field dimensions provided are not large enough to accomodate all of the defined entries and checks.")

  nFill.exp <- exp.size - nTotal
  nFill.blk <- floor(nFill.exp/nBlks)
  nFill.remainder <- nFill.exp - (nFill.blk*nBlks)

  ## Build field design
  fld.dgn <- data.frame(environment=enviro, trial=trial, plot_id=NA, plot=plot.start:(plot.start+(exp.size-1)), replication=c(rep(1:nBlks, each=(exp.size/nBlks)), rep("FILLER", times=nFill.remainder)), row=rep(1:nFieldRows, each=nFieldCols), column=rep(c(1:nFieldCols, nFieldCols:1), length.out=exp.size), line_code=NA, line_name=NA)
  fld.dgn$plot_id <- if(is.null(plot_id.template)) paste(trial, plot.start:(plot.start+(exp.size-1)), sep="_") else paste(plot_id.template[1], fld.dgn$plot, sep = plot_id.template[2])

  ## Fills in FILL plots, and randomizes chks, entries, etc..
  if(fillWithChk) entries.col <- unlist(lapply(1:nBlks, function(b){tmp <- c(entries, rep(chks, times=nChkReps), rep(chks[sample(1:nChks, size = nChks, replace = FALSE)], times=nFill.blk/nChks, length.out=nFill.blk)); tmp[sample(1:length(tmp), replace = FALSE)]}))
  if(fillWithEntry) entries.col <- unlist(lapply(1:nBlks, function(b){tmp <- c(entries, rep(chks, times=nChkReps), rep(dup.list[sample(1:length(dup.list), size = length(dup.list), replace = FALSE)], times= ceiling(nFill.blk/length(dup.list)), length.out=nFill.blk)); tmp[sample(1:length(tmp), replace = FALSE)]}))
  if(!fillWithEntry & !fillWithChk) entries.col <- unlist(lapply(1:nBlks, function(b){tmp <- c(entries, rep(chks, times=nChkReps)); tmp <- c(tmp[sample(1:length(tmp), replace = FALSE)], rep("FILLER", times=nFill.blk))}))

  fld.dgn$line_name <- c(entries.col, rep("FILLER", times=nFill.remainder))
  fld.dgn$line_code <- sapply(fld.dgn$line_name, function(X){if(X %in% entries) return(0) ; if(X %in% chks) return(which(chks == X)); if(X == "FILLER") return(NA)})

  fld.dgn <- merge(fld.dgn, entry.mat, by = "line_name", all.x = TRUE); fld.dgn <- fld.dgn[order(fld.dgn$plot), ]

  if(fillWithEntry){
    fld.dgn$duplicates  <- ""
    for(b in 1:nBlks){
      blk.tmp <- subset(fld.dgn, replication == b)
      dup.entries <- blk.tmp[which(duplicated(blk.tmp$entry) & blk.tmp$line_code %in% 0), "entry"]
      fld.dgn[which(fld.dgn$replication == b & fld.dgn$entry %in% dup.entries), "duplicates"]  <- "D"
    }
  }
  nDups <- length(which(fld.dgn$duplicates == "D"))/2

  per.chks <- round(length(which(fld.dgn[,"line_code"] %in% (1:nChks))) / length(!is.na(fld.dgn[,"line_code"])), digits = 3)

  ## Make field.layout with plot numbers
  for.layout.mats <- fld.dgn[order(fld.dgn$row, fld.dgn$column), ]

  fld.plot <- apply(t(matrix(for.layout.mats$plot, nrow = nFieldRows, ncol=nFieldCols, byrow=T)), 1, rev)
  fld.plot <- cbind(nFieldRows:1, fld.plot)
  fld.plot <- rbind(fld.plot, c("", 1:nFieldCols))

  ## Make field.layout with line codes
  line.code <- apply(t(matrix(for.layout.mats$line_code, nrow = nFieldRows, ncol=nFieldCols, byrow=T)), 1, rev)
  line.code <- cbind(nFieldRows:1, line.code)
  line.code <- rbind(line.code, c("", 1:nFieldCols))

  ## Make field.layout with blk codes
  blk.code <- apply(t(matrix(for.layout.mats$replication, nrow = nFieldRows, ncol=nFieldCols, byrow=T)), 1, rev)
  blk.code <- cbind(nFieldRows:1, blk.code)
  blk.code <- rbind(blk.code, c("", 1:nFieldCols))

  ##
  ## make fld.dgn have planting order

  plot<-fld.dgn ## assign output to a vecor called plot
  #get even cols, where cols are the planter driection of travel
  colNum<-unique(plot$column) # get cols
  evenCol<-colNum[!c(T,F)] # get even cols
  evens<-subset(plot, column %in%evenCol ) # subset by even cols
  oddCol<-colNum[c(T,F)] # get odd cols
  odds<-subset(plot, column %in%oddCol ) # subset by odd col
  rm(plot) # remove plot
  ## if odd column 1)sort by by column 2) then by row in assending order
  plot.odds<-odds[order(odds$column,odds$row),] # sort by col then row all in assedning order
  plot.odds$plant_order<-1:nrow(plot.odds) # make a plant row vec

  ## if even column  1)sort by  column  in assending, 2) then by row in decending order
  plot.evens<-evens[order(evens$column,-evens$row),] # sorting by column then row in decending
  plot.evens$plant_order<-1:nrow(plot.evens) # make a planting order vector

  ## merge odd and even cols
  plot_final<-rbind(plot.evens, plot.odds) # merge odd and even col vectors
  rm(plot.evens) # drop
  rm(plot.odds) # drop
  plot_final<-plot_final[order(plot_final$column, plot_final$plant_order),] #sort by col then plant order
  plot_final$plant_order<-1:nrow(plot_final) # make final planting order
  fld.dgn <-plot_final

  ## end add plant order
  ##

  return(list(rcbd.dgn=fld.dgn, plot.layout=fld.plot, check.layout=line.code, rep.layout=blk.code, field.dim=paste(nFieldRows, "x", nFieldCols, sep=" "), nDuplicates=nDups, rlzPerChks=per.chks))
}



