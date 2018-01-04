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
