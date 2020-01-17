#' Generate a flexible incomplete block design augmented with a set of replicated checks
#'
#' @description \code{design.aibd} assists in generating the layout and randomization of an augmented incomplete block design (AIBD) that is first described herein. This design is proposed as a less-restrictive version of the traditional modified augmented design (type 2) (\cite{Lin and Poushinsky, 1985}) and its more recent revision by the Barley CAP (\url{http://www.barleycap.org}); see \link{design.mad} for more information. Additionally, it is amendable to a braoder array of spatially-related error reduction procedures (see \link{adjust.fld} for more details on such procedures).
#'              'Flexible' refers to the fact that the incomplete blocks within an AIBD are not restricted to a certain size, as in th 3 row x 5 column restriction of the Barley CAP's MAD design (see \link{design.mad}, Figure 2). The placement of checks is also less restricted and more random when compared to the prior MAD layouts.
#' @param enviro Optional \code{character string} describing the environment which the field trial will be grown in. Default is the current date and time.
#' @param exp.name Required \code{character string} identifying name for the experiment.
#' @param entries A \code{character vector} of experimental (i.e. not check) entries. User must either provide this \strong{OR} \code{nEntries} (see next).
#' @param nEntries \code{Integer} argument indicating how many experimental entries are to be included in the trial. If \code{nEntries} = \emph{m}, then \emph{m} generic entry names will be generated and included in the output. User must either provide this \strong{OR} \code{entries}.. \code{nEntries} will be superceded if \code{entries} is provided.
#' @param chks A \code{character vector} including the set of check lines to be included. The first check included in the list will be assigned as the \code{primary} check and the remaining will be assigned as \code{secondary} checks (see \code{Details}). User must either provide this \strong{OR} \code{nChk2} (see next).
#' @param nChk2 \code{Integer} argument indicating how many \strong{secondary} check lines (see \code{Details}) are to be included in the trial. A primary check is automatically assigned in addition the the \code{nChk2} secondary checks, brining the total number of unique check lines to \eqn{nChk2 + 1}. User must either provide this \strong{OR} \code{chks}.
#' @param nFieldRows Optional, but must be provided if nFieldCols is not. \code{Integer} argument indicating the number of field rows (dimension 'a' in Figure 1).This value can be calculated internally by passing only \code{nFieldCols}.
#' @param nRowsPerBlk Optional \code{integer} argument indicating the number of rows per row_blk (dimension 'b', Figure 1). If an argument is provided it must be a divisor if \code{nFieldRows} to allow for blocking. If an 'optimized' dimension (see \code{Details}) is desired, leave empty.
#' @param nFieldCols Optional, but must be provided if nFieldRows is not \code{integer} argument indicating the number of field columns (dimension 'b',Figure 1). This value can be calculated internally by passing only \code{nFieldRows}.
#' @param nColsPerBlk Optional \code{integer} argument indicating the number of columns per col_blk (dimension 'd', Figure 1). If an argument is provided it must be a divisor if \code{nFieldCols} to allow for blocking. If an 'optimized' dimension (see \code{Details}) is desired, leave empty.
#' @param nBlks.min Optional \code{integer} argument indicating the minimum number of incomplete blocks acceptable in the final design. This also controls the minimum number of primary checks since there is one primary check per incomplete block. Default is \code{3}.
#' @param minDims Optional \code{numeric vector} argument of length 2 indicating the minimum row_blk dimension ('b', Figure 1) and minimum col_blk dimension ('d', Figure 1). Since the experimenter (i.e. user) ultimately knows what is ideal for their field, this is useful for fine-tuning 'optimized' designs. Default is \code{c(2,4)}, denoting minimum dimension of 2 rows per row_blk and 4 columns per col_blk, respectively.
#' @param maxDims Optional \code{numeric vector} argument of length 2 indicating the maximum row_blk dimension ('b', Figure 1) and maximum col_blk dimension ('d', Figure 1). Default is \code{c(Inf,Inf)}, denoting that there are no restrictions on block dimensions.
#' @param nChk2.min Optional \code{integer} indicating the number of times \strong{each} secondary check should appear in the design, i.e. the number of replicates per secondary check. Default is \code{3}.
#' @param plot.start Optional \code{integer} indicating the identification number of the first plot. \code{design.aibd} places the first plot in the "bottom-left" corner of the field and numbering then serpentines, starting to the right (Figure 1). Default is \code{1001}.
#' @param plot_id.template Optional \code{character vector} of length two. The first character is a string that will preceed the plot number in the plot_id field of the output, and the second character is the character to delimit the provided string and the plot number. For example, c("EnvA_TrialA", "_") will result in "EnvA_TrialA_1001", etc.
#' @param minPerChks Optional \code{numeric} argument in the range [0,1) indicating the minimum acceptable value of percent checks, i.e. what percent of the experiment is represented by a check line? Traditionally, the standard has been 10\%. Default is \code{0.09}.
#' @param maxPerChks Optional \code{numeric} argument in the range (0,1] indicating the maximum acceptable value of percent checks. If this value is exceeded during the creation of the field design the function will return a \code{warning} indicatin such. Default is \code{0.25}.
#' @param fillWithEntry Optional \code{logical}. Default is \code{TRUE}, meaning \code{FILL} plots remaining after \code{minPerCheck} has been reached will be replaced by randomly selected experimental entries. A "D" will appear in the output next to these replicated entries.
#' @param dup.list Optional \code{character vector} containing the candidates that could be replicated if \code{fillWithEntry = TRUE}. This is useful if there is a low amount of source seed for some experimental entries; those, for example, would be excluded from \code{dup.list}. Default is \code{NULL}, meaning that all entries are candidates for duplication.
#' @param fillWithChk Optional \code{logical}. If \code{TRUE} this will supercede \code{fillWithEntry} and any \code{FILL} plots remaining after \code{minPerCheck} has been reached will be replaced by secondary check lines. Default is \code{FALSE}.
#' @details The main highlights of AIBD and \code{design.aibd}:
#'            \enumerate{
#'              \item Adjustments based on blocking factors, e.g. block (b*d, Figure 1), row blocks (b, Figure 1), column blocks (d, Figure 1), can be implemented since:
#'                \itemize{
#'                  \item A primary check appears once, at random, in each incomplete block. This facilitates the use of downstream adjustment procedures that address spatially-related environmental variance.
#'                  \item A set of secondary checks are distributed throughout the experiment in a nearly-random process. This refers to the fact that as the sampling process has been tailored to ensure fairly even distribution of checks across blocks, hoewever placement within blocks is completely at random.
#'                  All adjustment procedures are carried out by the function \link{adjust.fld}.
#'                }
#'              \item Since field researchers typically know the depth of the experimental site and their plot dimensions users are required to provide, at a minimum, the number of field rows (Figure 1, dimension a), i.e. \code{nFieldRows}. Additional field dimensions can also be provided if desired as long as they allow all design requirements to be fulfilled. The following outline the various scenarios of user field dimension (refer to Figure 1) input:
#'                  \itemize{
#'                      \item Scenario 1: All field dimensions (a, b, c, d, respectively) are provided
#'                      \item Scenario 2: nFieldRows and nRowsPerBlk (a and b), \strong{OR} nFieldCols and nColsPerBlk (c and d) are provided
#'                      \item Scenario 3: nFieldRows and nFieldCols (a and c) are provided
#'                      \item Scenario 4: Most naive scenario - only nFieldRows (a) \strong{OR} nFieldCols is provided
#'                  }
#'                    In each of the above scenarios, the field dimension not provided will be determined by \code{design.aibd}; \code{minPerChks} is used as the starting point to determine the remainder of the field dimensions. Ultimately, the goal of \code{design.aibd} is to provide an 'optimized' design that meets the users requirements while remaining as close to the \code{minPerChk} argument as possible.
#'              \item The nature of the 'optimization' procedure mentioned just above can result in field designs that meet all requirements but posses some undesireable characteristic(s) to the user. Some common issues and their solution are listed below:
#'                  \itemize{
#'                      \item Block dimensions or number of blocks is not desireable: Define \code{minBlkDims} and/or \code{maxBlkDims} to modulate \code{nRowsPerBlk} (b), \code{nColsPerBlk} (d), and the number of blocks. Since each block contains a primary check, this can also be used to control the number of primary checks replicates.
#'                      \item More checks are desired than what are initially assigned, but the user does not want to increase the overall experiment size: Set \code{fillWithCheck} to \code{TRUE}, which will result in fill plots to be replaced with secondary check lines rather than replicated experimental entries.
#'                  }
#'              }
#'
#'          \if{html}{\figure{aibd_Fig1.jpg}}
#'
#'          AIBD may not be well-suited for all circumstances; in these cases consider using other design functions included in \link{FldTrial}: \link{design.rcbd} or \link{design.mad}.
#'
#' @return A \code{list} containing:
#'          \itemize{
#'            \item \code{aibd.dgn} A \code{dataframe} of the resulting design. A line code of 0 denotes an experimental entry, codes of 1, 2, ..., (\code{nChk2} + 1) denote the primary, first secondary, up to the (\code{nChk2} + 1)-th secondary check, respectively.
#'                                  If \code{fillWithEntry} is \code{TRUE} then a  "D" will be placed in the "Duplicates" column for all duplicated entries.
#'                                  Note that the column name "replication" is used to denote whole block replication, whereas "blk" denotes plot assignments to incomplete blocks.
#'            \item \code{plot.layout} A \code{matrix} of plot numbers reflecting the field layout. The first column contains field row numberis (i.e. 1:\code{nFieldRows}) and the last row contains field column numbers (i.e. 1:\code{nFieldCols}).
#'            \item \code{check.layout} A \code{matrix} of line codes reflecting the field layout. See \code{plot.layout} for further details.
#'            \item \code{blk.layout} A \code{matirx} of block assignments reflecting the field layout. See \code{plot.layout} for further details.
#'            \item \code{field.dims} The dimensions of the resulting field design; nFieldRows x nFieldCols, or a x c (Figure 1).
#'            \item \code{nDuplicates} The number of duplicated experimental entries. Will only be non-zero if \code{fillWithEntry} is \code{TRUE}.
#'            \item \code{rlzPerChks} The final percentage of the designed represented by check lines. This may be different than \code{minPerChks}, especially when \code{fillWithCheck} is \code{TRUE}.
#'          }
#' @references
#'    Lin, C.-S. and G. Poushinsky. 1985. A modified augmented design (type 2) for rectangular plots. Can. J. Plant Sci. 65:743-749.
#' @examples
#' \dontrun{
#' ## Example 1 - Provide only nFieldRows
#' aibd.ex1 <- design.aibd(exp.name = "ex1", nEntries = 350,
#'              nChk2 = 3, nFieldRows = 10)
#'
#' ## Example 2 - Provide nRowsPerBlk as well
#' aibd.ex2 <- design.aibd(exp.name = "ex2", nEntries = 350,
#'              nChk2 = 3, nFieldRows = 10, nRowsPerBlk = 5)
#'
#' ## Example 3 - Use minDims to define min nColsPerBlk
#' aibd.ex3 <- design.aibd(exp.name = "ex3", nEntries = 280,
#'              nChk2 = 3, nFieldRows = 9, minDims = c(Inf, 10))
#'
#' }
#'
#' @export


design.aibd <- function(enviro=format(Sys.Date(), "%x"), exp.name=NULL, entries=NULL, nEntries= NULL, chks= NULL, nChk2= NULL, nFieldRows= NULL,  nRowsPerBlk=NULL, nFieldCols= NULL, nColsPerBlk=NULL, nBlks.min=1, minDims=c(1,1), maxDims=c(Inf, Inf), nChk2.min=1, plot.start=1001, plot_id.template = NULL, minPerChks=0.00, maxPerChks=0.99, fillWithEntry=T, dup.list=NULL, fillWithChk=F){
  
  ## Set some parameters to NULL
  nBlkRows <- nBlkCols <- NULL
  
  ## Define min and max block dimensions
  minRowBlkDim <- minDims[1]; maxRowBlkDim <- maxDims[1]
  minColBlkDim <- minDims[2]; maxColBlkDim <- maxDims[2]
  
  ## Whole bunch of QC
  if(is.null(exp.name)) stop("Must provide a name for the experiment (exp.name=)"); trial <- paste(exp.name, enviro, sep="_")
  if(all(is.null(entries), is.null(nEntries))) stop("Must provide an entry list (entries=) OR the number of entries desired (nEntries=).")
  if(all(is.null(chks), is.null(nChk2))) stop("Must provide a list of check names (chks=) with the primary check listed first\n OR the number of SECONDARY checks desired (nChk2=).")
  if(all(is.null(nFieldRows), is.null(nFieldCols))) stop("Provide the number of rows (sometimes called ranges or beds) (nFieldRows=).")
  
  if(!is.null(nFieldRows)){
    if(prime(nFieldRows)) warning("nFieldRows is a prime number - rows will not be blocked.")
    if(nFieldRows > maxRowBlkDim) stop("The row dimension entered cannot be blocked and lies outside the defined min or max.")
  }
  if(!is.null(nFieldCols)){
    if(prime(nFieldCols)) warning("nFieldCols is a prime number - the columns will not be blocked.")
    if(nFieldCols > maxColBlkDim) stop("The column dimension entered cannot be blocked and lies outside the defined min or max.")
  }
  
  if(all(fillWithEntry, fillWithChk)) stop("Select either fillWithEntry or fillWithCheck")
  if(fillWithChk) fillWithEntry <- F
  if(fillWithEntry) fillWithChk <- F
  
  
  ## Calculate other non-input parameters
  entries <- if(!is.null(entries)) as.character(as.matrix(entries)) else paste("entry", 1:nEntries, sep="_"); nEntries <- length(entries)
  chks <- if(!is.null(chks)) as.character(as.matrix(chks)) else paste("chk", 1:(nChk2+1), sep="_"); nChks <- length(chks); nChk2 <- nChks-1
  dup.list <- if(!is.null(dup.list)) as.character(as.matrix(dup.list)) else entries
  
  entry.mat <- cbind(line_name=c(unique(entries), chks), entry=1:(length(unique(entries)) + nChks)) ## This is for field book
  
  if(!is.null(nRowsPerBlk)){
    if(nRowsPerBlk < minRowBlkDim | nRowsPerBlk > maxRowBlkDim) stop("The row dimension entered lies outside the defined min or max.")
    if(nFieldRows %% nRowsPerBlk != 0) stop("nFieldRows is not evenly divisible by the defined number of nRowsPerBlk")
    nBlkRows <- nFieldRows/nRowsPerBlk
  }
  if(!is.null(nColsPerBlk)){
    if(nColsPerBlk < minColBlkDim | nColsPerBlk > maxColBlkDim) stop("The column dimension entered lies outside the defined min or max.")
    if(nFieldCols %% nColsPerBlk != 0) stop("nFieldCols is not evenly divisible by the defined number of nColsPerBlk")
    nBlkCols <- nFieldCols/nColsPerBlk
  }
  if(!is.null(nBlkRows) & !is.null(nBlkCols)){
    nBlks <- nBlkRows*nBlkCols
    nTotal <- nEntries + nBlks + (nChk2*nChk2.min)
  }
  if(!is.null(nFieldRows) & !is.null(nFieldCols)) exp.size <- nFieldRows*nFieldCols
  
  ## Calculate remaining field paramaters -- this is based on min and max blk.dims and % checks
  ########################################################################################################
  ########################################################################################################
  
  ### Scenario 1 - User perovides all field dimensions
  if(all(!is.null(nFieldRows), !is.null(nRowsPerBlk), !is.null(nFieldCols), !is.null(nColsPerBlk))){
    nChk.plots <- nBlks + nChk2*nChk2.min
    min.plots <- nEntries + nChk.plots
    nFill <- nFieldRows*nFieldCols - min.plots
    if(nFill < 0) stop("Field dimension defined cannot accomodate the number of entries and checks specified.")
    print("Design via Scenario 1")
  }
  
  ## Scenario 2.1 - User did not provide nFieldCols & nColsPerBlk
  if(all(c(!is.null(nFieldRows), !is.null(nRowsPerBlk), is.null(nFieldCols), is.null(nColsPerBlk)))){
    environment(scenario2.1) <- environment()
    print("Design via Scenario 2")
    
    while(1){
      perChk.list <- seq(minPerChks, maxPerChks, by = .0025)
      perChk.out <- sapply(perChk.list, scenario2.1, return.params=FALSE) ## Calls scenario2.1 function
      
      if(min(perChk.out) < 1){
        sel.PerChk <- is.min(perChk.out)[1]
        end.params <- scenario2.1(perChk.start = perChk.list[sel.PerChk], return.params = TRUE)
        perChk <- end.params$perChk
        nFill <- end.params$nFill
        nFieldCols <- end.params$nFieldCols
        nBlkCols <- end.params$nBlkCols
        nColsPerBlk <- nFieldCols / nBlkCols
        nBlks<- end.params$nBlks
        nChk.plots <- nBlks + nChk2*nChk2.min
        break
      } else nChk2.min <- nChk2.min + 1
      if(nChk2.min > 100 & !fillWithChk) stop("AIBD not available for the defined parameters. Suggest changing PerChk limits or set fillWithChk = TRUE.")
      if(nChk2.min > 100) stop("AIBD not available for the defined parameters. Suggest changing PerChk limits.")
    }
  } ## End scenario 2.1
  
  ## Scenario 2.2 - User did not provide nFieldRows & nRowsPerBlk
  if(all(c(!is.null(nFieldCols), !is.null(nColsPerBlk), is.null(nFieldRows), is.null(nRowsPerBlk)))){
    environment(scenario2.2) <- environment()
    print("Design via Scenario 2")
    
    while(1){
      perChk.list <- seq(minPerChks, maxPerChks, by = .0025)
      perChk.out <- sapply(perChk.list, scenario2.2, return.params=FALSE) ## Calls scenario2 function
      
      if(min(perChk.out) < 1){
        satisfied.s2 <- T
        sel.PerChk <- is.min(perChk.out)[1]
        end.params <- scenario2.2(perChk.start = perChk.list[sel.PerChk], return.params = TRUE)
        perChk <- end.params$perChk
        nFill <- end.params$nFill
        nFieldRows <- end.params$nFieldRows
        nBlkRows <- end.params$nBlkRows
        nRowsPerBlk <- nFieldRows/ nBlkRows
        nBlks <- end.params$nBlks
        nChk.plots <- nBlks + nChk2*nChk2.min
        break
      } else nChk2.min <- nChk2.min + 1
      if(nChk2.min > 100 & !fillWithChk) stop("AIBD not available for the defined parameters. Suggest changing PerChk limits or set fillWithChk = TRUE.")
      if(nChk2.min > 100) stop("AIBD not available for the defined parameters. Suggest changing PerChk limits.")
    }
  } ## End scenario 2.2
  
  
  ## Scenario 3 - User provides only nFieldRows and nFieldCols
  if(all(c(!is.null(nFieldRows), !is.null(nFieldCols), is.null(nRowsPerBlk), is.null(nColsPerBlk)))){
    environment(scenario3) <- environment()
    print("Design via Scenario 3")
    
    while(1){
      perChk.list <- seq(minPerChks, maxPerChks, by = .0025)
      perChk.out <- sapply(perChk.list, scenario3, return.params=FALSE)
      
      if(min(perChk.out) < 1){
        sel.PerChk <- is.min(perChk.out)[1]
        end.params <- scenario3(perChk.start = perChk.list[sel.PerChk], return.params = TRUE)
        perChk <- end.params$perChk
        nFill <- end.params$nFill
        nBlkRows <- end.params$nBlkRows
        nBlkCols <- end.params$nBlkCols
        nRowsPerBlk <- nFieldRows / nBlkRows
        nColsPerBlk <- nFieldCols / nBlkCols
        nBlks <- end.params$nBlks
        nChk.plots <- nBlks + nChk2*nChk2.min
        break
      } else nChk2.min <- nChk2.min + 1
      if(nChk2.min > 100 & !fillWithChk) stop("AIBD not available for the defined parameters. Suggest changing PerChk limits or set fillWithChk to TRUE.")
      if(nChk2.min > 100) stop("AIBD not available for the defined parameters. Suggest changing PerChk limits.")
    }
  } ## End of scenario 3
  
  
  ## Scenario 4.1 - User only provided nFieldRows - calcualte rest
  if(all(c(!is.null(nFieldRows), is.null(nRowsPerBlk), is.null(nFieldCols), is.null(nColsPerBlk)))){
    environment(scenario4.1) <- environment()
    print("Design via Scenario 4")
    
    while(1){
      perChk.list <- seq(minPerChks, maxPerChks, by = .0025)
      perChk.out <- sapply(perChk.list, scenario4.1, return.params=FALSE)
      
      minPerChk.out <- min(perChk.out)
      if(minPerChk.out < 1){
        sel.PerChk <- is.min(perChk.out)[1]
        end.params <- scenario4.1(perChk.start = perChk.list[sel.PerChk], return.params=TRUE)
        perChk <- end.params$perChk
        nFill <- end.params$nFill
        nFieldCols <- end.params$nFieldCols
        nBlkRows <- end.params$nBlkRows
        nBlkCols <- end.params$nBlkCols
        nRowsPerBlk <- nFieldRows / nBlkRows
        nColsPerBlk <- nFieldCols / nBlkCols
        nBlks<- end.params$nBlks
        nChk.plots <- nBlks + nChk2*nChk2.min
        break
      } else nChk2.min <- nChk2.min + 1
      if(nChk2.min > 100 & !fillWithChk) stop("AIBD not available for the defined parameters. Suggest changing PerChk limits or set fillWithChk to TRUE.")
      if(nChk2.min > 100) stop("AIBD not available for the defined parameters. Suggest changing PerChk limits.")
    }
  } ## End scenario 4.1
  
  ## Scenario 4.2 - User only provided nFieldCols - calcualte rest
  if(all(c(!is.null(nFieldCols), is.null(nColsPerBlk), is.null(nFieldRows), is.null(nRowsPerBlk)))){
    environment(scenario4.2) <- environment()
    print("Design via Scenario 4")
    
    while(1){
      perChk.list <- seq(minPerChks, maxPerChks, by = .0025)
      perChk.out <- sapply(perChk.list, eval(scenario4.2, envir = parent.frame()), return.params=FALSE)
      
      minPerChk.out <- min(perChk.out)
      if(minPerChk.out < 1){
        sel.PerChk <- is.min(perChk.out)[1]
        end.params <- eval(scenario4.2(perChk.start = perChk.list[sel.PerChk], return.params=TRUE), envir = parent.frame())
        perChk <- end.params$perChk
        nFill <- end.params$nFill
        nFieldRows <- end.params$nFieldRows
        nBlkRows <- end.params$nBlkRows
        nBlkCols <- end.params$nBlkCols
        nRowsPerBlk <- nFieldRows / nBlkRows
        nColsPerBlk <- nFieldCols / nBlkCols
        nBlks<- end.params$nBlks
        nChk.plots <- nBlks + nChk2*nChk2.min
        break
      } else nChk2.min <- nChk2.min + 1
      if(nChk2.min > 100 & !fillWithChk) stop("AIBD not available for the defined parameters. Suggest changing PerChk limits or set fillWithChk to TRUE.")
      if(nChk2.min > 100) stop("AIBD not available for the defined parameters. Suggest changing PerChk limits.")
    }
  } ## End scenario 4.2
  
  exp.size <- nFieldRows * nFieldCols
  #################################
  #################################
  ## Build field design file
  for(r in 1:nBlkRows){
    if(r == 1){start <- 1; end <- nBlkCols; blkCol <- c()}
    if(r %% 2 == 1) step1 <- rep(c(rep(start:end, each=nColsPerBlk), rep(end:start, each=nColsPerBlk)), times=nRowsPerBlk)
    if(r %% 2 == 0) step1 <- rep(c(rep(end:start, each=nColsPerBlk), rep(start:end, each=nColsPerBlk)), times=nRowsPerBlk)
    step2 <- step1[1:(length(step1)/2)]
    blkCol <- c(blkCol, step2)
    start <- max(unique(step2))+1
    end <- start+(nBlkCols-1)
  }
  
  fld.dgn <- data.frame(environment= enviro, trial= trial, plot_id= NA,  plot= plot.start:(plot.start+(exp.size-1)), replication=1, row= rep(1:nFieldRows, each=nFieldCols), column= rep(c(1:nFieldCols, nFieldCols:1), length.out=exp.size), blk= blkCol, row_blk= rep(1:nBlkRows, each=exp.size/nBlkRows), col_blk= rep(c(rep(1:nBlkCols, each=nColsPerBlk), rev(rep(1:nBlkCols, each=nColsPerBlk))), times = nFieldRows)[1:exp.size], line_code=rep(NA, times=exp.size), line_name=rep(NA, times=exp.size))
  fld.dgn$plot_id <- if(is.null(plot_id.template)) paste(trial, plot.start:(plot.start+(exp.size-1)), sep="_") else paste(plot_id.template[1], fld.dgn$plot, sep = plot_id.template[2])
  
  if(!fillWithChk){ ## !fillWithChk so if it is fiilWithEntry or no fill replacement at all, there will still be enough checks
    nFill.to.chk2 <- 0
    while(!(nChk.plots+nFill.to.chk2)/(nEntries+nChk.plots+nFill.to.chk2+nFill) >= minPerChks){
      nFill.to.chk2 <- nFill.to.chk2+1
      nFill <- nFill-1
    }
  }
  
  to.subtract <- 0
  if(!fillWithChk & !fillWithEntry){
    fld.dgn[(nrow(fld.dgn)-nFill+1):nrow(fld.dgn), "line_code"] <- -1 ## Do this first so that a check is not placed in a fill plot
    to.subtract <- nFill
  }
  
  
  ## Place chk1's -- if decide to include > 1 chk1/blk then will need to change size
  chk1.sample.mat <- matrix((1:exp.size)[order(blkCol)], nrow = nBlks, byrow = T)
  if(to.subtract > 0) chk1.sample.mat[nrow(chk1.sample.mat), (ncol(chk1.sample.mat)-to.subtract+1):ncol(chk1.sample.mat)] <- NA
  chk1.sample <- apply(chk1.sample.mat, 1, function(X) sample(X[which(!is.na(X))], size = 1))
  
  fld.dgn[chk1.sample, "line_code"] <- 1
  
  ## Sample chk2's from around the field and place them -- This is updated code in response to J-L's comment to increase equitability of chk2 distribution
  if(fillWithChk) nChk2.plots <- nChk2*nChk2.min + nFill else nChk2.plots <- nChk2*nChk2.min + nFill.to.chk2
  sel.chk2.blks <- unlist(lapply(1:ceiling(nChk2.plots/nBlks), function(b) sample(1:nBlks, nBlks, F)))
  sel.chk2.blks <- table(sel.chk2.blks[1:nChk2.plots])
  
  chk2.list <- rep(2:(1+nChk2), times=ceiling(nChk2.plots/nChk2))[1:nChk2.plots] ## Generates an orderd list of chk2's to be sampled from and randomply placed into blocks in a fashion that allows a specific chk2 to appear in the most blocks possible
  for(b in 1:length(sel.chk2.blks)){
    blk <- as.numeric(names(sel.chk2.blks[b]))
    nPlots.in.blk <- sel.chk2.blks[[b]]
    plots <- sample(fld.dgn[which(fld.dgn$blk == blk & is.na(fld.dgn$line_code)), "plot"], size = nPlots.in.blk, replace = F)
    sel.chk2s <- chk2.list[1:nPlots.in.blk]; chk2.list <- chk2.list[-c(1:nPlots.in.blk)]
    fld.dgn[which(fld.dgn$plot %in% plots), "line_code"] <- sel.chk2s
  }
  
  ## Make lists of entries + chk2's and RANDOMIZE
  if(fillWithEntry){
    rand.entries <- c(entries, sample(dup.list, nFill, F))
    rand.entries <- rand.entries[order(sample(1:length(rand.entries), replace=F))]
  } else{ ## This will work with fill or fillWithChek
    rand.entries <- entries[order(sample(1:nEntries, size = nEntries, replace = F))]
  }
  
  fld.dgn[which(is.na(fld.dgn$line_code)), "line_code"] <- 0
  
  fld.dgn[,"line_name"] <- sapply(c(fld.dgn[,"line_code"]), function(X){if(X == 0) return(NA) ; if(X %in% 1:(nChk2+1)) return(chks[X]); if(X == -1) return("FILLER")}) ## could use merge, but whatever
  
  fld.dgn[which(is.na(fld.dgn$line_name)), "line_name"] <- rand.entries
  fld.dgn[which(fld.dgn$line_code == -1), "line_code"] <- NA
  
  ## Add "entry" and "replication" columns that are needed for field.book
  fld.dgn<- merge(fld.dgn, entry.mat,all.x = T, by = "line_name", sort = FALSE); fld.dgn <- fld.dgn[order(fld.dgn$plot), ]
  
  if(fillWithEntry){
    extra.col <- matrix(" ", nrow = nrow(fld.dgn), ncol = 1)
    dup.entries <- fld.dgn[which(duplicated(fld.dgn$entry) & fld.dgn$line_code %in% 0), "entry"]
    nDups <- length(dup.entries)
    extra.col[which(fld.dgn$entry %in% dup.entries)] <- "D"
    fld.dgn <- cbind(fld.dgn, duplicates=extra.col)
  } else nDups <- 0
  
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
  blk.code <- apply(t(matrix(for.layout.mats$blk, nrow = nFieldRows, ncol=nFieldCols, byrow=T)), 1, rev)
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
  
  return(list(aibd.dgn=fld.dgn, plot.layout=fld.plot, check.layout=line.code, blk.layout=blk.code, field.dims=paste(nFieldRows, "x", nFieldCols, sep=" "), nDuplicates=nDups, rlzPerChks=per.chks))
  
  
  
} ## End of design.aibd




#' Generate a modified augmented design layout
#'
#' @description \code{design.mad} generates a field layout whose various attributed originate from Lin and Poushinsky's modified augmented desing (type 2) (\cite{Lin and Poushinsky, 1985}), with modifications made by the Barley CAP (\url{http://www.barleycap.org}) to accomodate standard row crop field plots (Figure 2). See \code{Details} for more information.
#' @param enviro Optional \code{character string} describing the environment which the field trial will be grown in. Default is the current date and time.
#' @param exp.name Required \code{character string} identifying name for the experiment.
#' @param entries A \code{character vector} of experimental (i.e. not check) entries. User must either provide this \strong{OR} \code{nEntries} (see next).
#' @param nEntries \code{Integer} argument indicating how many experimental entries are to be included in the trial. If \code{nEntries} = \emph{m}, then \emph{m} generic entry names will be generated and included in the output. User must either provide this \strong{OR} \code{entries}.. \code{nEntries} will be superceded if \code{entries} is provided.
#' @param chks A \code{character vector} including the set of check lines to be included. The first check included in the list will be assigned as the \code{primary} check and the remaining will be assigned as \code{secondary} checks (see \code{Details}). User must either provide this \strong{OR} \code{nChk2} (see next).
#' @param nChk2 \code{Integer} argument indicating how many \strong{secondary} check lines (see \code{Details}) are to be included in the trial. A primary check is automatically assigned in addition the the \code{nChk2} secondary checks, brining the total number of unique check lines to \eqn{nChk2 + 1}. User must either provide this \strong{OR} \code{chks}.
#' @param nFieldRows Optional, but must be provided if nFieldCols is not. \code{Integer} argument indicating the number of field rows (dimension 'a' in Figure 1).This value can be calculated internally by passing only \code{nFieldCols}.
#' @param nFieldCols Optional, but must be provided if nFieldRows is not \code{integer} argument indicating the number of field columns (dimension 'b',Figure 1). This value can be calculated internally by passing only \code{nFieldRows}.
#' @param nChk2.min Optional \code{integer} indicating the number of times \strong{each} secondary check should appear in the design, i.e. the number of replicates per secondary check. Default is \code{3}.
#' @param plot.start Optional \code{integer} indicating the identification number of the first plot. \code{design.aibd} places the first plot in the "bottom-left" corner of the field and numbering then serpentines, starting to the right (Figure 1). Default is \code{1001}.
#' @param plot_id.template Optional \code{character vector} of length two. The first character is a string that will preceed the plot number in the plot_id field of the output, and the second character is the character to delimit the provided string and the plot number. For example, c("EnvA_TrialA", "_") will result in "EnvA_TrialA_1001", etc.
#' @param minPerChks Optional \code{numeric} argument in the range [0,1) indicating the minimum acceptable value of percent checks, i.e. what percent of the experiment is represented by a check line? Traditionally, the standard has been 10\%. Default is \code{0.09}.
#' @param maxPerChks Optional \code{numeric} argument in the range (0,1] indicating the maximum acceptable value of percent checks. If this value is exceeded during the creation of the field design the function will return a \code{warning} indicatin such. Default is \code{0.25}.
#' @param fillWithEntry Optional \code{logical}. Default is \code{TRUE}, meaning \code{FILL} plots remaining after \code{minPerCheck} has been reached will be replaced by randomly selected experimental entries. A "D" will appear in the output next to these replicated entries.
#' @param dup.list Optional \code{character vector} containing the candidates that could be replicated if \code{fillWithEntry = TRUE}. This is useful if there is a low amount of source seed for some experimental entries; those, for example, would be excluded from \code{dup.list}. Default is \code{NULL}, meaning that all entries are candidates for duplication.
#' @param fillWithChk Optional \code{Logical}. If \code{TRUE} this will supercede \code{fillWithEntry} and any \code{FILL} plots remaining after \code{minPerCheck} has been reached will be replaced by secondary check lines. Default is \code{FALSE}.
#' @details The modified augmented design (type 2) was originally proposed by \cite{Lin and Poushinsky (1985)}. The design originally arranged experimental entries in a grid pattern consisting of "whole plots" (WPs), with the dimension of each whole plot being 1 row x 5 columns. At the center of each WP was a "control" or check plot denoted as the primary check. Furthermore, a subset of the WPs also contained secondary check lines.
#'          See \code{Figure 1} from Lin and Poushinsky (1985) for details on the original (type 2) modified augmented desging. This type of design allowed for multiple adjustment procedures to correct for spatial heterogeneity (i.e. spatially-related error variance) (\cite{Lin and Poushinsky, 1985}).
#'          Because of its heavy use of check plots (20 percent guaranteed from primary checks alone!) the traditional type 2 MAD was modified by the Barley CAP (\url{http://www.barleycap.org}). The dimesion of the WPs have been changed to 3 rows x 5 columns (Figure 2), and the center plot is still retained as the "control" or check plot. Secondary checks (typically 2-3) are still assigned to a subset of the blocks.
#'          The advantage of this modification is that fewer check plots are required, however it places a stronger restriction on overall field size, specifically that the field dimensions must be reducible by the 3 row x 5 column WP sizes. The adjustment procedures for correcting spatially-related error from Lin and Poushinsky (1985) are retained and described in more depth in the documentation for the function \link{adjust.fld}.
#'
#'
#'          Figure 1:\if{html}{\figure{Lin.Pulinsky_mad_Fig1.jpg}}
#'
#'
#'
#'
#'          Figure 2:\if{html}{\figure{mad_Fig2.jpg}}
#'
#'          MAD may not be well-suited for all circumstances; in these cases consider using other design functions included in \link{FldTrial}: \link{design.rcbd} or \link{design.aibd}
#'
#' @return \itemize{
#'            \item mad.dgn A \code{dataframe} of the resulting design. A line code of 0 denotes an experimental entry, codes of 1, 2, ..., (\code{nChk2} + 1) denote the primary, first secondary, up to the (\code{nChk2} + 1)-th secondary check, respectively.
#'                           If \code{fillWithEntry} is \code{TRUE} then a  "D" will be placed in the "Duplicates" column for all duplicated entries.
#'                           Note that the column name "replication" is used to denote whole block replication, whereas "blk" denotes plot assignments to incomplete blocks.
#'            \item plot.layout A \code{matrix} of plot numbers reflecting the field layout. The first column contains field row numberis (i.e. 1:\code{nFieldRows}) and the last row contains field column numbers (i.e. 1:\code{nFieldCols}).
#'            \item check.layout A \code{matrix} of line codes reflecting the field layout. See \code{plot.layout} for further details.
#'            \item blk.layout A \code{matirx} of block assignments reflecting the field layout. See \code{plot.layout} for further details.
#'            \item field.dims The dimensions of the resulting field design; nFieldRows x nFieldCols, or a x c (Figure 1).
#'            \item nDuplicates The number of duplicated experimental entries. Will only be non-zero if \code{fillWithEntry} is \code{TRUE}.
#'            \item rlzPerChks The final percentage of the designed represented by check lines. This may be different than \code{minPerChks}, especially when \code{fillWithCheck} is \code{TRUE}.
#'          }
#'
#' @references
#'    Lin, C.-S. and G. Poushinsky. 1985. A modified augmented design (type 2) for rectangular plots. Can. J. Plant Sci. 65:743-749.
#'
#' @examples
#' \dontrun{
#' ## Example 1 - Provide only nFieldRows
#' mad.ex1 <- design.mad(exp.name = "ex1", nEntries = 250,
#'              nChk2 = 2, nFieldRows = 9)
#'
#' ## Example 2 - Provide entries and checks
#'                Provide both nFieldRows and nFieldCols
#' mad.ex2 <- design.mad(exp.name = "ex2",
#'              entries = paste("Line", 1:300, sep = "."),
#'              chks = c("Larry", "Curly", "Moe"),
#'              nFieldRows = 12, nFieldCols = 30)
#' }
#' @export


design.mad <- function(enviro=format(Sys.Date(), "%x"), exp.name=NULL, entries= NULL, nEntries= NULL, chks= NULL, nChk2= NULL, nFieldRows= NULL, nFieldCols= NULL, nChk2.min=3, plot.start=1001, plot_id.template = NULL, minPerChks=0.10, maxPerChks=0.5, fillWithEntry=T, dup.list=NULL, fillWithChk=F){
  ## MADII designs are specifically 3x5 designs with a primarcy check in the center of each incomplete block and a set of secondary checks randomly appearing in a number of incomplete blocks
  
  ## QC
  if(is.null(exp.name)) stop("Must provide a name for the experiment (exp.name=)"); trial <- paste(exp.name, enviro, sep="_")
  if(is.null(entries) & is.null(nEntries)) stop("Must provide an entry list (entries=) OR the number of entries desired (nEntries=).")
  if(is.null(chks) & is.null(nChk2)) stop("Must provide a list of check names (chks=) with the primary check listed first\n OR the number of SECONDARY checks desired (nChk2=).")
  if(all(is.null(nFieldRows), is.null(nFieldCols))) stop("Must provide the number of rows (nFieldRows=) OR the number of columns (nFieldCols=.")
  if(all(!is.null(nFieldRows), nFieldRows %% 3 !=0)) stop("For MADII designs the number of field rows (nFieldRows) must be a multiple of 3.\n Either modify the input or switch to a more flexiblie design (i.e. design.aibd).")
  if(all(!is.null(nFieldCols), nFieldCols %% 5 !=0)) stop("For MADII designs the number of field columns (nFieldCols) must be a multiple of 5.\n Either modify the input or switch to a more flexiblie design (i.e. design.aibd).")
  
  entries <- if(!is.null(entries)) as.character(as.matrix(entries)) else paste("entry", 1:nEntries, sep="_"); nEntries <- length(entries)
  
  chks <- if(!is.null(chks)) as.character(as.matrix(chks)) else paste("chk", 1:(nChk2+1), sep="_"); nChks <- length(chks); nChk2 <- nChks-1
  dup.list <- if(!is.null(dup.list)) as.character(as.matrix(dup.list)) else entries
  
  entry.mat <- cbind(line_name=c(unique(entries), chks, "FILLER"), entry=c(1:(length(unique(entries)) + nChks), NA))
  
  if(fillWithChk) fillWithEntry <- FALSE ## Put this first b/c fillWithEntry is the default
  if(fillWithEntry) fillWithChk <- FALSE
  if(all(fillWithEntry, fillWithChk)) stop("Select either fillWithEntry or fillWithCheck")
  
  
  ## Calculate all missing field parameters -- if nFielCols is provided:
  if(!is.null(nFieldRows)) nBlkRows <- nFieldRows / 3
  if(!is.null(nFieldCols)) nBlkCols <- nFieldCols / 5
  
  
  if(all(!is.null(nFieldCols), !is.null(nFieldRows))){
    startPerChks <- minPerChks
    
    while(1){
      nBlks <- nBlkRows*nBlkCols
      if(nBlks < nChk2.min) stop("The input parameters does not allow for enough blocks to accomodate nChk2.min.")
      if(nFieldCols %% 5 != 0) stop("For MADII designs the number of field columns (nFieldCols) must be a multiple of 5. Either modify the input or switch to a more flexiblie design (i.e. design.aibd).")
      
      if(nFieldRows*nFieldCols < (nEntries + nBlks + nChk2*nChk2.min)) stop("The defined field dimensions are not large enough to accomodate the number of entries.")
      
      exp.size <- nFieldRows*nFieldCols
      
      if(fillWithEntry) nChk2.blks <- ceiling((startPerChks*exp.size - nBlks) / nChk2) ## If user denotes that fill plots should contain replicated entries, then base the nChk2.blks on minPerChk
      if(fillWithChk) nChk2.blks <- floor((exp.size - (nEntries + nBlks)) / nChk2)
      
      nFill <- exp.size - (nEntries + nBlks + nChk2.blks*nChk2)
      if(nFill >= 0 & nChk2.blks >= nChk2.min) break else startPerChks <- startPerChks + 0.005
      if(nFill < 0 & nChk2.blks >= nChk2.min) startPerChks <- startPerChks - 0.001
      if((startPerChks > maxPerChks) | ((startPerChks + .01) < minPerChks)) stop("Cannot satisfy secondary check requirements given the specified field dimensions : Consider reducing nChk2, nChk2.min, or use fillWithChk.")
    }
    
    if(startPerChks < minPerChks) warning("DESIGN STILL GENERATED: minPerChks was slightly reduced to accomodate the defined secondary check requirements.\nSee the rlzPerChk output for final result.")
  }
  
  
  ## If nFieldCols isn't provided, will be calculated here:
  if(is.null(nFieldCols)){
    min.nCol <- ceiling((nEntries + nBlkRows + nChk2*nChk2.min) / nFieldRows) ## use nBlkRows b/c that defines the minimum number of blocks
    
    satisfied <- F
    while(!satisfied){
      perChk.tmp <- minPerChks
      while(min.nCol %% 5 != 0) min.nCol <- min.nCol+1
      
      nBlkCols <- min.nCol / 5
      nBlks <- nBlkRows*nBlkCols
      
      exp.size <- nFieldRows*min.nCol
      
      if(fillWithEntry){
        while(!satisfied){
          nChk2.blks <- ceiling((perChk.tmp*exp.size - nBlks) / nChk2)
          if(nChk2.blks >= nChk2.min) satisfied <- T else perChk.tmp <- perChk.tmp + .001
          if((((nBlks + nChk2.blks*nChk2) / exp.size) > maxPerChks) | ((nEntries + nBlks + nChk2.blks*nChk2) > exp.size)){min.nCol <- min.nCol + 1; satisfied <- F; break}
        }
      }
      if(fillWithChk){
        nChk2.blks <- floor((exp.size - (nEntries + nBlks)) / nChk2)
        if(nChk2.blks >= nChk2.min) satisfied <- T else min.nCol <- min.nCol + 1
      }
    }
    
    nFill <- exp.size - (nEntries + nBlks + nChk2.blks*nChk2)
    nFieldCols <- min.nCol
  }
  
  ## If nFieldRows isn't provided, will be calculated here:
  if(is.null(nFieldRows)){
    min.nRow <- ceiling((nEntries + nBlkCols + nChk2*nChk2.min) / nFieldCols) ## use nBlkRows b/c that defines the minimum number of blocks
    
    satisfied <- F
    while(!satisfied){
      perChk.tmp <- minPerChks
      while(min.nRow %% 3 != 0) min.nRow <- min.nRow+1
      
      nBlkRows <- min.nRow / 3
      nBlks <- nBlkRows*nBlkCols
      
      exp.size <- nFieldCols*min.nRow
      
      if(fillWithEntry){
        while(!satisfied){
          nChk2.blks <- ceiling((perChk.tmp*exp.size - nBlks) / nChk2)
          if(nChk2.blks >= nChk2.min) satisfied <- T else perChk.tmp <- perChk.tmp + .001
          if((((nBlks + nChk2.blks*nChk2) / exp.size) > maxPerChks) | ((nEntries + nBlks + nChk2.blks*nChk2) > exp.size)){min.nCol <- min.nCol + 1; satisfied <- F; break}
        }
      }
      if(fillWithChk){
        nChk2.blks <- floor((exp.size - (nEntries + nBlks)) / nChk2)
        if(nChk2.blks >= nChk2.min) satisfied <- T else min.nCol <- min.nCol + 1
      }
    }
    
    nFill <- exp.size - (nEntries + nBlks + nChk2.blks*nChk2)
    nFieldRows <- min.nRow
  }
  
  
  ## Build field design file ## Must be a better way to do this, but it works
  for(r in 1:nBlkRows){
    if(r == 1){start <- 1; end <- nBlkCols; blkCol <- c()}
    if(r %% 2 == 1) step1 <- rep(c(rep(start:end, each=5), rep(end:start, each=5)), times=3)
    if(r %% 2 == 0) step1 <- rep(c(rep(end:start, each=5), rep(start:end, each=5)), times=3)
    step2 <- step1[1:(length(step1)/2)]
    blkCol <- c(blkCol, step2)
    start <- max(unique(step2))+1
    end <- start+(nBlkCols-1)
  }
  
  fld.dgn <- data.frame(environment= enviro, trial=trial, plot_id= NA, plot= plot.start:(plot.start+(exp.size-1)), replication=1, row= rep(1:nFieldRows, each=nFieldCols), column= rep(c(1:nFieldCols, nFieldCols:1), length.out=exp.size), blk= blkCol, row_blk= rep(1:nBlkRows, each=exp.size/nBlkRows),
                        col.blk= rep(c(rep(1:nBlkCols, each=5), rev(rep(1:nBlkCols, each=5))), times = nFieldRows)[1:exp.size], line_code=rep(c(rep(NA, times=nFieldCols), rep(c(NA, NA, 1, NA, NA), times=nBlkCols), rep(NA, times=nFieldCols)), times=nBlkRows),
                        line_name=rep(NA, times=exp.size))
  fld.dgn$plot_id <- if(is.null(plot_id.template)) paste(trial, plot.start:(plot.start+(exp.size-1)), sep="_") else paste(plot_id.template[1], fld.dgn$plot, sep = plot_id.template[2])
  
  ## Line codes for chk1 are filled in already, but need entry col. filled in as well as the rest of the line codes
  coords.sel <- cbind(sample(rep((1:nBlkRows), times=ceiling(nChk2.blks/nBlkRows)), size = nChk2.blks, replace = F), sample(rep((1:nBlkCols), times=ceiling(nChk2.blks/nBlkRows)), size=nChk2.blks, replace = F))
  blk.coords <- unique(fld.dgn[,8:10])
  sel.chk2.blks <- apply(coords.sel, 1, function(X) blk.coords[which(blk.coords[,2] %in% X[1] & blk.coords[,3] %in% X[2]), 1])
  
  for(b in sel.chk2.blks){
    plots <- sample(fld.dgn[which(fld.dgn$blk == b & is.na(fld.dgn$line_code)), "plot"], size = nChk2, replace = F)
    order <- sample(1:nChk2, size = nChk2, replace = F)
    fld.dgn[which(fld.dgn$plot %in% plots), "line_code"] <- (2:(1+nChk2))[order]
  }
  
  if(fillWithEntry) entries.tmp <- c(entries, sample(rep(dup.list, times=ceiling(nFill/length(dup.list))), size = nFill, replace = F))
  if(fillWithChk) entries.tmp <- c(entries, sample(rep(chks[-1], times=ceiling(nFill/nChk2)), size = nFill, replace = F))
  rand.entries <- entries.tmp[sample(1:length(entries.tmp), size = length(entries.tmp), replace = F)]
  if(!fillWithEntry & !fillWithChk) rand.entries <- c(entries[sample(1:nEntries, replace = F)], rep("FILLER", times=nFill))
  
  rand.entries <- cbind(sapply(rand.entries, function(X) {if(X == "FILLER") NA else{if(X %in% chks) which(chks == X) else 0}}), rand.entries)
  
  fld.dgn[, "line_name"] <- sapply(fld.dgn[,"line_code"], function(X) chks[X])
  fld.dgn[which(is.na(fld.dgn$line_code)), 11:12] <- rand.entries
  
  ## Add entry column for field.book
  fld.dgn <- merge(fld.dgn, entry.mat, all.x = T, by = "line_name", sort = FALSE); fld.dgn <- fld.dgn[order(fld.dgn$plot), ]
  #fld.book <- cbind(fld.dgn[,c(3,4,2,12,6,7,13,5)], notes=" ")
  
  ## Identify duplicates if fillWithEntry
  if(fillWithEntry){
    extra.col <- matrix(" ", nrow = nrow(fld.dgn), ncol = 1)
    dup.entries <- fld.dgn[which(duplicated(fld.dgn$line_name) & fld.dgn$line_code %in% 0), "line_name"]
    nDups <- length(dup.entries)
    extra.col[which(fld.dgn$line_name %in% dup.entries)] <- "D"
    fld.dgn <- cbind(fld.dgn, duplicates=extra.col)
  } else nDups <- 0
  
  per.chks <- round(length(which(fld.dgn[,"line_code"] %in% (1:(nChk2+1)))) / length(!is.na(fld.dgn[,"line_code"])), digits = 3)
  
  for.layout.mats <- fld.dgn[order(fld.dgn$row, fld.dgn$column), ]
  ## Make field.layout with plot numbers
  fld.plot <- apply(t(matrix(for.layout.mats$plot, nrow = nFieldRows, ncol=nFieldCols, byrow=T)), 1, rev)
  fld.plot <- cbind(nFieldRows:1, fld.plot)
  fld.plot <- rbind(fld.plot, c("", 1:nFieldCols))
  
  ## Make field.layout with line codes
  line.code <- apply(t(matrix(for.layout.mats$line_code, nrow = nFieldRows, ncol=nFieldCols, byrow=T)), 1, rev)
  line.code <- cbind(nFieldRows:1, line.code)
  line.code <- rbind(line.code, c("", 1:nFieldCols))
  
  ## Make field.layout with blk codes
  blk.code <- apply(t(matrix(for.layout.mats$blk, nrow = nFieldRows, ncol=nFieldCols, byrow=T)), 1, rev)
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
  
  return(list(mad.dgn=fld.dgn, plot.layout=fld.plot, check.layout=line.code, blk.layout=blk.code, field.dim=paste(nFieldRows, "x", nFieldCols, sep=" "), nDuplicates=nDups, rlzPerChks=per.chks))
}



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






## Supporting functions ##

#' Support functions not available to user, just other functions
#' 

trace <- function(x) mean(diag(x))
reduce <- function(x) which(x %% 1:x == 0)
prime <- function(x) !any(x %% 2:(x-1) == 0)


rm.NA.cols <- function(X) length(na.omit(X)) ## ## Function that removes columns of all NA from individual environments (not all traits are evaluated across all envs)

is.max <- function(list.in){
  return(which(list.in == max(list.in, na.rm = T)))
}

is.min <- function(list.in){
  return(which(list.in == min(list.in, na.rm = T)))
}

round.up <- function(x,to=10){
  to*(x%/%to + as.logical(x%%to))
}

### finds rho for row and column
find.rhos <- function(rhos, fr.in){
  for(i in 1:length(fr.in)){x <- fr.in[i]; assign(names(x), x[[1]])}
  print(rhos)
  
  Z.fr[["AR1"]] <- model.matrix(~ -1+ row:column, data = data.fr)
  
  K.fr[["AR1"]] <- rhos[1]^colDist.fr %x% rhos[2]^rowDist.fr
  
  return(-emmremlMultiKernel(y=data.fr[,trait.fr], X=X.fr, Zlist=Z.fr, Klist=K.fr)$loglik)
}


optim.2 <- function(fr.in.2=fr.list){ ## Have to do this becasue EMMREML uses optim... was leading to errors
  fr.out <- optim(par = c(.5,.5), fn = find.rhos, control = list(reltol = 1e-6), fr.in=fr.in.2)
  return(fr.out)
}


## Merge multiple dfs stored in a list
merge.all <- function(df.list, by.m=NULL){
  i<-1
  while(i <= length(df.list)){
    if(i == 1) merged.df <- get(df.list[i]) else merged.df <- merge(merged.df, get(df.list[i]), by=by.m, all.x=T)
    i <- i+1
  }
  return(merged.df)
}


## Harmonic means
harm.mean <- function(x){ ## Calculates harmonic mean of a vector
  if(0 %in% x) x <- subset(x, x > 0)
  harmean <- 1/mean(1/x)
  return(harmean)
}


### lmm.fun
#lmm.list.in <- blup.list
lmm.fun <- function(lmm.list.in){
  for(i in 1:length(lmm.list.in)){x <- lmm.list.in[i]; assign(names(x), x[[1]])}
  
  if(is.null(Vformula.lmm)) new.Vformula.lmm <- ~ lmm.R else new.Vformula.lmm <- as.formula(paste("~", paste(c(attr(terms.formula(Vformula.lmm), "term.labels"), "lmm.R"), collapse = " + "), sep = " "))
  
  return(regress(formula.lmm, new.Vformula.lmm, pos = rep(T, 100), identity=F, data = data.lmm, verbose = as.numeric(lmm.verbose)))
}

### Forms R matrix from list of multiple trial-specific matrices
make.R <-function(mr.list.in=NULL){
  for(i in 1:length(mr.list.in)){x <- mr.list.in[i]; assign(names(x), x[[1]])}
  final.R <- matrix(0, nrow.mr, nrow.mr)
  
  for(r in 1:length(trial.mats)){
    final.R[dims.mr[[r]], dims.mr[[r]]] <- trial.mats[[r]]
  }
  return(final.R)
}

### Moving average function -- utilizes Technow's mvngGrAd() package
#mv.in = mvavg.input
mvng.avg <- function(mv.in=NULL){  
  ## Unpack input
  for(i in 1:length(mv.in)){x <- mv.in[i]; assign(names(x), x[[1]])}
  
  if(!exists("toMaximize")){
    toMaximize <- function(combs){
      mvAvg.results <- movingGrid(rows, cols, obsPhe = as.numeric(data.mv[,trait.mv]), shapeCross=list(1:combs[1], 1:combs[1], 1:combs[2], 1:combs[2]), layers=c(1:combs[3]), excludeCenter=T)
      if(any(is.na(mvAvg.results@movingMean))) -Inf else mvAvg.results@correlation
    }
    toMaximize.no_l <- function(combs){
      mvAvg.results <- movingGrid(rows, cols, obsPhe = as.numeric(data.mv[,trait.mv]), shapeCross=list(1:combs[1], 1:combs[1], 1:combs[2], 1:combs[2]), layers=NULL, excludeCenter=T)
      if(any(is.na(mvAvg.results@movingMean))) -Inf else mvAvg.results@correlation
    }
    toMaximize.no_lr <- function(combs){
      mvAvg.results <- movingGrid(rows, cols, obsPhe = as.numeric(data.mv[,trait.mv]), shapeCross=list(NULL, NULL, 1:combs, 1:combs), layers=NULL, excludeCenter=T)
      if(any(is.na(mvAvg.results@movingMean))) -Inf else mvAvg.results@correlation
    }
    toMaximize.no_lc <- function(combs){
      mvAvg.results <- movingGrid(rows, cols, obsPhe = as.numeric(data.mv[,trait.mv]), shapeCross=list(1:combs, 1:combs, NULL, NULL), layers=NULL, excludeCenter=T)
      if(any(is.na(mvAvg.results@movingMean))) -Inf else mvAvg.results@correlation
    } 
  }
  
  if(is.null(grid.mv)){ ## If a grid size isn't provided, mvng.avg will find the 'optimal'
    
    if(is.null(max.dims)){
      nth_sth <- 1:(fld.dims[1]-1) ## Doing -1 makes it possible for even plots in the corners to have the whole row/colun included (i.e. the whole field for corner plots)
      east_west <- 1:(fld.dims[2]-1)
    } else{
      nth_sth <- 1:max.dims[1] ## l is limited by the # of rows and columns
      east_west <- 1:max.dims[2]
    }
    
    cat("\nOptimizing moving grid size for", trait.mv, "in", trial.mv, "\n")
    combs.no_l <- cbind(rep(nth_sth, each= length(east_west)), rep(east_west, times= length(nth_sth)))
    combs <- cbind(combs.no_l, apply(combs.no_l, 1, min))
    
    max.list <- list()
    cat("\nStep 1 of 4\n")
    max.list[[1]] <- list("cor.list"=cor.list <- pbapply(combs, 1, toMaximize), "max"=max(cor.list, na.rm = T), "max.pos"=is.max(cor.list))
    cat("\nStep 2 of 4\n")
    max.list[[2]] <- list("cor.list"=cor.list <- pbapply(combs.no_l, 1, toMaximize.no_l), "max"=max(cor.list, na.rm = T), "max.pos"=is.max(cor.list))
    cat("\nStep 3 of 4\n")
    max.list[[3]] <- list("cor.list"=cor.list <- pbapply(as.matrix(east_west), 1, toMaximize.no_lr), "max"=max(cor.list, na.rm = T), "max.pos"=is.max(cor.list))
    cat("\nStep 4 of 4\n")
    max.list[[4]] <- list("cor.list"=cor.list <- pbapply(as.matrix(nth_sth), 1, toMaximize.no_lc), "max"=max(cor.list, na.rm = T), "max.pos"=is.max(cor.list))
    
    chosen.pos <- is.max(unlist(lapply(max.list, function(x){return(x$max)}))); chosen.pos <- chosen.pos[length(chosen.pos)]
    chosen <- max.list[[chosen.pos]]
    
    if(chosen.pos == 1) {
      r <- combs[chosen$max.pos, 1]; c <- combs[chosen$max.pos, 2]; l <- combs[chosen$max.pos, 3]
      mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(1:r, 1:r, 1:c, 1:c), layers=c(1:l), excludeCenter=T)
    }
    if(chosen.pos == 2) {
      r <- combs.no_l[chosen$max.pos, 1]; c <- combs.no_l[chosen$max.pos, 2]; l <- 0
      mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(1:r, 1:r, 1:c, 1:c), layers=NULL, excludeCenter=T)
    }
    if(chosen.pos == 3) {
      r <- 0; c <- east_west[chosen$max.pos]; l <- 0
      mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(NULL, NULL, 1:c, 1:c), layers=NULL, excludeCenter=T)  
    }
    if(chosen.pos == 4) {
      r <- nth_sth[chosen$max.pos]; c <- 0; l <- 0
      mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(1:r, 1:r, NULL, NULL), layers=NULL, excludeCenter=T)  
    }
    
  } else{ ### If user chooses to define grid size then it jumps to here
    
    if(length(grid.mv) != 3) stop("A numeric vector of length 3 (row, column, diagonal) is required for each trial in grid.sizes")
    r <- grid.mv[1]
    c <- grid.mv[2]
    l <- grid.mv[3]
    if(!any(c(r,c,l) == 0)) mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(1:r, 1:r, 1:c, 1:c), layers=c(1:l), excludeCenter=T)
    if(r == 0) mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(NULL, NULL, 1:c, 1:c), layers=c(1:l), excludeCenter=T)
    if(c == 0) mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(1:r, 1:r, NULL, NULL), layers=c(1:l), excludeCenter=T)
    if(l == 0) mv.correction <- movingGrid(rows, cols, obsPhe=as.numeric(data.mv[,trait.mv]), shapeCross=list(1:r, 1:r, 1:c, 1:c), layers=NULL, excludeCenter=T)
    
    if(any(is.na(mv.correction@movingMean))) stop("The defined mvavg grid is resulting in NA covariates. Change one or more dimension.")
  }
  
  mv.means <- mv.correction@movingMean
  return(list(mvavg.adj=mvngGrAd::fitted(mv.correction), mvavg.covs= (mv.means - mean(mv.means, na.rm = T)), mvavg.grid.sz=c(r,c,l)))
} ## End of moving average


tt.update <- function(orig, response=T, obj, pos="end"){ ## pos can be "beginning" or "end"
  y <- NULL; if(response) y <- orig[[2]]
  if(pos == "beginning") return(as.formula(paste(y, "~", paste(c(obj, attr(terms.formula(orig), "term.labels")), collapse = " + "), sep = " ")))
  if(pos == "end") return(as.formula(paste(y, "~", paste(c(attr(terms.formula(orig), "term.labels"), obj), collapse = " + "), sep = " ")))
}

FldTrial.lm <- function(y, X){
  n <- nrow(X)
  k <- ncol(X)
  
  suppressWarnings(inv.tX.X <- chol2inv(chol(t(X) %*% X)))
  if(!exists("inv.tX.X")) inv.tX.X <- MASS::ginv(t(X) %*% X) ## If chol fails then use general inverse of MASS package
  
  beta <- matrix(inv.tX.X %*% t(X) %*% y, ncol = 1, dimnames = list(colnames(X), NULL))
  
  yhat <- X %*% beta
  e <- as.vector(y - yhat)
  
  sigma.samp <- (t(e) %*% e) / (n-k)
  varB <- sigma.samp %x% inv.tX.X
  se.B <- sqrt(sigma.samp) * sqrt(diag(inv.tX.X))
  rownames(varB) <- colnames(varB) <- rownames(beta)
  
  return(list(betahat=beta, resids=e, varbetahat= diag(varB), stderrbetahat=se.B, dfErr=(n-k)))
}


### Based on 'predictmeans' pacakge
calc.ASED <- function(K, vcov.m){
  nK <- nrow(K)
  
  kindx <- 1:nK
  CM <- matrix(0, nrow = nK * (nK - 1)/2, ncol = nK)
  t <- 1
  for (i in 2:nK) {
    for (j in 1:(i - 1)) {
      CM[t, ] <- (kindx == j) - (kindx == i)
      #varn1[t] <- rnK[i]
      #varn2[t] <- rnK[j]
      t <- t + 1
    }
  }
  rK <- CM %*% K
  
  dses <- as.numeric(apply(rK, 1, function(x) {
    y <- matrix(x, nrow = 1)
    sqrt(y %*% tcrossprod(vcov.m, y))
  }))
  
  return(mean(dses))  
}


#### design.aibd design functions
## Scenario 2.1 function 
scenario2.1 <- function(perChk.start, return.params){
  perChk <- NA
  minPlots <- ceiling((1+perChk.start) * nEntries)
  min.nFieldCols <- ceiling(minPlots / nFieldRows)
  
  while(prime(min.nFieldCols)) min.nFieldCols <- min.nFieldCols + 1
  
  cand.nBlkCols <- reduce(min.nFieldCols) ## leave in this order to test the largest blocks possible first
  cand.nColsPerBlk <- min.nFieldCols / cand.nBlkCols
  cands2remove <- unique(c(which(cand.nColsPerBlk < minColBlkDim | cand.nColsPerBlk > maxColBlkDim | cand.nColsPerBlk == min.nFieldCols), which(cand.nBlkCols*nBlkRows < nBlks.min)))
  if(length(cands2remove) > 0){
    cand.nBlkCols <- cand.nBlkCols[-cands2remove]
    cand.nColsPerBlk <- cand.nColsPerBlk[-cands2remove]
    cand.nBlks <- cand.nBlkCols * nBlkRows      
  }
  
  if(length(cand.nBlkCols) == 0){
    return(1)
  } else{
    for(c in 1:length(cand.nBlks)){
      nTotal.tmp <- nEntries + cand.nBlks[c] + nChk2*nChk2.min
      exp.size.tmp <- min.nFieldCols*nFieldRows
      nFill.tmp <- exp.size.tmp - nTotal.tmp
      
      if(nFill.tmp >= 0){perChk.tmp <- (exp.size.tmp - nEntries) / exp.size.tmp} else{perChk.tmp <- 1}
      
      if(perChk.tmp >= minPerChks & perChk.tmp <= maxPerChks){
        perChk <- perChk.tmp
        break
      }
    }
  }
  if(!return.params & !is.na(perChk)) return(perChk)
  if(!return.params & is.na(perChk)) return(1)
  if(return.params) return(list(perChk=perChk, nFill=nFill.tmp, nFieldCols=min.nFieldCols, nBlkCols=cand.nBlkCols[c], nBlks=cand.nBlks[c])) 
}

## Scenario 2.2 function -- finds nFieldRows and nRowsPerBlk
scenario2.2 <- function(perChk.start, return.params){
  perChk <- NA
  minPlots <- ceiling((1+perChk.start) * nEntries)
  min.nFieldRows <- ceiling(minPlots / nFieldCols)
  
  while(prime(min.nFieldRows)) min.nFieldRows <- min.nFieldRows + 1
  
  cand.nBlkRows <- reduce(min.nFieldRows) ## leave in this order to test the largest blocks possible first
  cand.nRowsPerBlk <- min.nFieldRows / cand.nBlkRows
  cands2remove <- unique(c(which(cand.nRowsPerBlk < minRowBlkDim | cand.nRowsPerBlk > maxRowBlkDim | cand.nRowsPerBlk == min.nFieldRows), which(cand.nBlkRows*nBlkCols < nBlks.min)))
  if(length(cands2remove) > 0){
    cand.nBlkRows <- cand.nBlkRows[-cands2remove]
    cand.nRowsPerBlk <- cand.nRowsPerBlk[-cands2remove]
    cand.nBlks <- cand.nBlkRows * nBlkCols    
  }
  
  if(length(cand.nBlkRows) == 0) return(1) else{
    for(c in 1:length(cand.nBlks)){
      nTotal.tmp <- nEntries + cand.nBlks[c] + nChk2*nChk2.min
      exp.size.tmp <- min.nFieldRows*nFieldCols
      nFill.tmp <- exp.size.tmp - nTotal.tmp
      
      perChk.tmp <-  if(nFill.tmp >= 0) (exp.size.tmp - nEntries) / exp.size.tmp else perChk.tmp <- 1
      
      if(perChk.tmp >= minPerChks & perChk.tmp <= maxPerChks){
        perChk <- perChk.tmp
        break
      }
    }
  }
  
  if(!return.params & !is.na(perChk)) return(perChk)
  if(!return.params & is.na(perChk)) return(1)
  if(return.params) return(list(perChk=perChk, nFill=nFill.tmp, nFieldRows=min.nFieldRows, nBlkRows=cand.nBlkRows[c], nBlks=cand.nBlks[c]))
}

## Scenario 3 Function - User does not provide block dimensions
scenario3 <- function(perChk.start, return.params){
  perChk <- NA
  
  ## Set up candidate row dimensions
  cand.nBlkRows <- reduce(nFieldRows)
  cand.nRowsPerBlk <- nFieldRows / cand.nBlkRows
  cand.rows2remove <- which(cand.nRowsPerBlk < minRowBlkDim | cand.nRowsPerBlk > maxRowBlkDim)
  if(length(cand.rows2remove > 0)){
    cand.nBlkRows <- cand.nBlkRows[-cand.rows2remove]
    cand.nRowsPerBlk <- cand.nRowsPerBlk[-cand.rows2remove]      
  }
  
  ## Set up candidate column dimensions
  cand.nBlkCols <- reduce(nFieldCols) ## leave in this order to test the largest blocks possible first
  cand.nColsPerBlk <- nFieldCols / cand.nBlkCols
  cand.cols2remove <- which(cand.nColsPerBlk < minColBlkDim | cand.nColsPerBlk > maxColBlkDim | cand.nColsPerBlk == nFieldCols)
  if(length(cand.cols2remove) > 0){
    cand.nBlkCols <- cand.nBlkCols[-cand.cols2remove]
    cand.nColsPerBlk <- cand.nColsPerBlk[-cand.cols2remove]
  }
  
  ## List of candidate block sizes
  cand.nBlks.mat <- rbind(rep(cand.nBlkRows, times=length(cand.nBlkCols)), rep(cand.nBlkCols, each=length(cand.nBlkRows)))
  cand.nBlks <- apply(cand.nBlks.mat, 2, function(X){return(X[1]*X[2])})
  cand.blks2remove <- which(cand.nBlks < nBlks.min)
  if(length(cand.blks2remove) > 0){
    cand.nBlks.mat <- as.matrix(cand.nBlks.mat[,-cand.blks2remove])
    cand.nBlks <- cand.nBlks[-cand.blks2remove] 
  }
  
  if(length(cand.nBlks) == 0){
    return(1)
  } else{
    for(c in 1:length(cand.nBlks)){
      nTotal.tmp <- nEntries + cand.nBlks[c] + nChk2*nChk2.min
      exp.size.tmp <- nFieldCols*nFieldRows
      nFill.tmp <- exp.size.tmp - nTotal.tmp
      
      if(nFill.tmp >= 0){perChk.tmp <- (exp.size.tmp - nEntries) / exp.size.tmp} else{perChk.tmp <- 1}
      
      if(minPerChks <= perChk.tmp & perChk.tmp <= maxPerChks){
        perChk <- perChk.tmp
        break
      }
    }
  }
  if(!return.params & !is.na(perChk)) return(perChk)
  if(!return.params & is.na(perChk)) return(1)
  if(return.params) return(list(perChk=perChk, nFill=nFill.tmp, nBlkRows=cand.nBlks.mat[1,c], nBlkCols=cand.nBlks.mat[2,c], nBlks=cand.nBlks[c])) 
}


### Scenario 4.1 function -- user provides only nFieldRows
scenario4.1 <- function(perChk.start, return.params){
  perChk <- NA
  minPlots <- ceiling((1+perChk.start) * nEntries)
  min.nFieldCols <- ceiling(minPlots / nFieldRows)
  
  while(prime(min.nFieldCols)) min.nFieldCols <- min.nFieldCols + 1
  
  ## Set up candidate row dimensions
  cand.nBlkRows <- reduce(nFieldRows)
  cand.nRowsPerBlk <- nFieldRows / cand.nBlkRows
  cands2remove <- (cand.nRowsPerBlk < minRowBlkDim | cand.nRowsPerBlk > maxRowBlkDim)
  cand.nBlkRows <- cand.nBlkRows[!cands2remove]
  cand.nRowsPerBlk <- cand.nRowsPerBlk[!cands2remove]  
  
  ## Set up candidate column dimensions
  cand.nBlkCols <- reduce(min.nFieldCols) ## leave in this order to test the largest blocks possible first
  cand.nColsPerBlk <- min.nFieldCols / cand.nBlkCols
  cands2remove <- (cand.nColsPerBlk < minColBlkDim | cand.nColsPerBlk > maxColBlkDim | cand.nColsPerBlk == min.nFieldCols)
  cand.nBlkCols <- cand.nBlkCols[!cands2remove]
  cand.nColsPerBlk <- cand.nColsPerBlk[!cands2remove]
  
  ## List of candidate block sizes
  cand.nBlks.mat <- rbind(rep(cand.nBlkRows, times=length(cand.nBlkCols)), rep(cand.nBlkCols, each=length(cand.nBlkRows)))
  cand.nBlks <- apply(cand.nBlks.mat, 2, function(X){return(X[1]*X[2])})
  cands2remove <- cand.nBlks < nBlks.min
  cand.nBlks.mat <- as.matrix(cand.nBlks.mat[,!cands2remove])
  cand.nBlks <- cand.nBlks[!cands2remove]  
  
  if(length(cand.nBlks) == 0) return(1) else{
    for(c in 1:length(cand.nBlks)){
      nTotal.tmp <- nEntries + cand.nBlks[c] + nChk2*nChk2.min
      exp.size.tmp <- min.nFieldCols*nFieldRows
      nFill.tmp <- exp.size.tmp - nTotal.tmp ## This whole loop is to calculate the nFill.tmp parameter to make sure that the field is big enough to accomodate all entries and required checks (as defined), plus enough "extra" to satisfy minPerChk requirement
      
      perChk.tmp <- if(nFill.tmp >= 0) (exp.size.tmp - nEntries) / exp.size.tmp else 1
      
      if(minPerChks <= perChk.tmp & perChk.tmp <= maxPerChks){
        perChk <- perChk.tmp
        break
      }
    }
  }
  if(!return.params & !is.na(perChk)) return(perChk)
  if(!return.params & is.na(perChk)) return(1)
  if(return.params) return(list(perChk=perChk, nFill=nFill.tmp, nFieldCols=min.nFieldCols, nBlkRows=cand.nBlks.mat[1,c], nBlkCols=cand.nBlks.mat[2,c], nBlks=cand.nBlks[c])) 
}

### Scenario 4.2 function -- user provides only nFieldCols, find rest
#perChk.start <- perChk.list[1]
scenario4.2 <- function(perChk.start, return.params){
  perChk <- NA
  minPlots <- ceiling((1+perChk.start) * nEntries)
  min.nFieldRows <- ceiling(minPlots / nFieldCols)
  
  while(prime(min.nFieldRows)) min.nFieldRows <- min.nFieldRows + 1
  
  ## Set up candidate column dimensions
  cand.nBlkCols <- reduce(nFieldCols)
  cand.nColsPerBlk <- nFieldCols / cand.nBlkCols
  cands2remove <- cand.nColsPerBlk < minColBlkDim | cand.nColsPerBlk > maxColBlkDim
  cand.nBlkCols <- cand.nBlkCols[!cands2remove]
  cand.nColsPerBlk <- cand.nColsPerBlk[!cands2remove]  
  
  ## Set up candidate column dimensions
  cand.nBlkRows <- reduce(min.nFieldRows) ## leave in this order to test the largest blocks possible first
  cand.nRowsPerBlk <- min.nFieldRows / cand.nBlkRows
  cands2remove <- (cand.nRowsPerBlk < minRowBlkDim | cand.nRowsPerBlk > maxRowBlkDim | cand.nRowsPerBlk == min.nFieldRows)
  cand.nBlkRows <- cand.nBlkRows[!cands2remove]
  cand.nRowsPerBlk <- cand.nRowsPerBlk[!cands2remove]
  
  ## List of candidate block sizes
  cand.nBlks.mat <- rbind(rep(cand.nBlkRows, times=length(cand.nBlkCols)), rep(cand.nBlkCols, each=length(cand.nBlkRows)))
  cand.nBlks <- apply(cand.nBlks.mat, 2, function(X) return(X[1]*X[2]))
  cands2remove <- cand.nBlks < nBlks.min
  cand.nBlks.mat <- as.matrix(cand.nBlks.mat[,!cands2remove])
  cand.nBlks <- cand.nBlks[!cands2remove]  
  
  if(length(cand.nBlks) == 0) return(1) else{
    for(c in 1:length(cand.nBlks)){
      nTotal.tmp <- nEntries + cand.nBlks[c] + nChk2*nChk2.min
      exp.size.tmp <- min.nFieldRows*nFieldCols
      nFill.tmp <- exp.size.tmp - nTotal.tmp ## This whole loop is to calculate the nFill.tmp parameter to make sure that the field is big enough to accomodate all entries and required checks (as defined), plus enough "extra" to satisfy minPerChk requirement
      
      perChk.tmp <- if(nFill.tmp >= 0) (exp.size.tmp - nEntries) / exp.size.tmp else 1
      
      if(minPerChks <= perChk.tmp & perChk.tmp <= maxPerChks){
        perChk <- perChk.tmp
        break
      }
    }
  }
  if(!return.params & !is.na(perChk)) return(perChk)
  if(!return.params & is.na(perChk)) return(1)
  if(return.params) return(list(perChk=perChk, nFill=nFill.tmp, nFieldRows=min.nFieldRows, nBlkRows=cand.nBlks.mat[1,c], nBlkCols=cand.nBlks.mat[2,c], nBlks=cand.nBlks[c])) 
}





