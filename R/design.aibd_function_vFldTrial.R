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

