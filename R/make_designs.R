#' Create experimental design randomizations
#' 
#' @description 
#' These functions create randomized complete block designed (RCBD) or augmented incomplete block
#' designed (AIBD) experiments.
#' 
#' @param location The name of the location, writting as a three-letter (or more) code.
#' @param location.id The two-digit zurn number for the location.
#' @param trial The name of the trial.
#' @param trial.id The two-digit zurn number for trial.
#' @param number.rows The number of beds / field rows.
#' @param plot.start The number of the first plot i.e. 0 or 1 or 1000 which ever, Default is 1001
#' @param number.blocks The number of blocks to use i.e. 0 or 1 or 2 or 3. Defalut is 3. For an RCBD, this is also the number of replications.
#' @param year The year of the planting.
#' @param crop.id The zurn code for the crop i.e. 5 for oat or 3 for barley 
#' @param entries A csv file i.e.("18 VT 9jan18.csv") with one column "line" which has the list of the entries for the VT
#' @param checks A csv file i.e.("18 VT 9jan18.csv") with one column "check" which has the list of the checks.
#' @param number.reps.chk The number of check replications. Default is 1.
#' @param fill.with A character determining what should fill empty plots. If "check," plots will be filled with checks; if "entry", plots will
#' be filled with entries; if "filler", plots will be filled with "FILLER"; alternatively, a custom string can be supplied (i.e. "RASMUSSEN").
#' @param write.excel Should the design and map files be exported to an .xlsx file? The name of the file will be "trial_year_loc_rand.xlsx"
#' 
#' @examples 
#' 
#' @import stringr
#' @import writexl
#' 
#' @export
#' 
#' 
make.rcbd <- function(location, location.id, trial, trial.id, entries, plot.start = 1001, number.blocks = 3, number.rows, year, crop.id, 
                      checks, number.reps.chk = 1, fill.with = c("check", "entry", "filler"), write.excel = FALSE) {
  
  ## Check use inputs
  location <- as.character(location)
  location.id <- formatC(x = as.integer(location.id), width = 2, flag = "0")
  trial <- as.character(trial)
  trial.id <- formatC(x = as.integer(trial.id), width = 2, flag = "0")
  plot.start <- as.integer(plot.start)
  if (!nchar(plot.start) == 4) stop("'plot.start' must be 4 digits.")
  number.blocks <- as.integer(number.blocks)
  year <- as.integer(year)
  crop.id <- as.integer(crop.id)
  number.rows <- as.integer(number.rows)
  number.reps.chk <- as.integer(number.reps.chk)
  
  ## Fill.with must be length 1
  if (length(fill.with) > 1) stop("'fill.with' must be a character vector of length 1.")
  
  
  ## Files to read in
  # Check extensions
  if (!str_detect(entries, ".csv")) stop ("The 'entries' file does not have the .csv extension.")
  if (!str_detect(checks, ".csv")) stop ("The 'checks' file does not have the .csv extension.")
  
  entries_df <- read.csv(entries, stringsAsFactors = FALSE)
  checks_df <- read.csv(checks, stringsAsFactors = FALSE)

  
  # Check if any checks appear in the entry list
  conflict.chk <- intersect(checks_df[,1], entries_df[,1])
  
  ## Entries are the individuals in the entries file, minus any overlap with the checks
  entries <- setdiff(x = entries_df[,1], y = checks_df[,1])
  # Checks are lines in the check file
  checks <- checks_df[,1]
  
  
    
  ###
  # Fill with options
  fillWithEntry <- fill.with == "entry"
  fillWithChk <- fill.with == "check"
  
  ###
  rcbd <- design.rcbd(enviro = location, exp.name = trial, chks = checks, nChkReps = number.reps.chk,
                      nBlks = number.blocks, entries = entries, nChks = 0, nFieldRows = number.rows, 
                      plot.start = plot.start, fillWithEntry = fillWithEntry, fillWithChk = fillWithChk)
  
  
  # Make the map file
  plot.lay <- as.data.frame(rcbd$plot.layout, stringsAsFactors = FALSE)
  rep.lay <- as.data.frame(rcbd$rep.layout, stringsAsFactors = FALSE)
  check.lay <- as.data.frame(rcbd$check.layout, stringsAsFactors = FALSE)

  # Make the data file
  design <- rcbd$rcbd.dgn
  # Sort on plot
  design <- design[order(design$plot), , drop = FALSE]
  
  design$environment <- as.character(design$environment)
  # Add the location code
  design$loc_code <- location.id
  # Add the experiment code and experiment
  design$trial <- trial
  design$trial_code <- trial.id
  # Add the zurn code
  design$barcode <- paste0(crop.id, location.id, trial.id, design$replication, design$plot)
  design$barcode[design$line_name == "FILLER"] <- NA 
  
  ## If the line_name is FILLER, and fill.with is not check, entry, or fill, use the fill with
  if (! fill.with %in% c("check", "entry", "filler")) {
    design$line_name[design$line_name == "FILLER"] <- fill.with
  }
  
  # Make a planting order
  design1 <- subset(design, , c("row", "column", "plant_order"))
  plant_order <- reshape(data = design1, idvar = "row", timevar = "column", direction = "wide")
  ## Flip
  plant_order <- plant_order[rev(seq(nrow(plant_order))),,drop = FALSE]
  ## Add a bottom row
  plant_order <- rbind(plant_order, c("", seq(ncol(plant_order) - 1)))
  # Remove row and column names
  row.names(plant_order) <- NULL; colnames(plant_order) <- colnames(rep.lay)


  # ## Combine layouts and order to a map
  # rep.lay1 <- rep.lay[-nrow(rep.lay),]; colnames(rep.lay1) <- c("row/col", seq(ncol(rep.lay1) - 1))
  # check.lay1 <- check.lay[-nrow(check.lay),]; colnames(check.lay1) <- c("row/col", seq(ncol(check.lay1) - 1))

  
  ## Number of columns
  n_col <- ncol(plot.lay)
  
  
  ## Combine
  plot.map1 <- rbind(c("Plot Layout", rep(NA, n_col - 1)), plot.lay, NA, c("Rep Layout", rep(NA, n_col - 1)), rep.lay,
                     NA, c("Check Layout", rep(NA, n_col - 1)), check.lay, NA, c("Plant Order", rep(NA, n_col - 1)), plant_order)
  colnames(plot.map1) <- NULL
  
  # Edit trial 
  design$trial <- paste(trial, year, location, sep = "_")
  # Edit line name
  design$line_name <- toupper(design$line_name)
  
  # Keep relevant columns
  design_toprint <- design[,c("trial", "line_name", "plot", "replication", "row", "column", "line_code", "entry", "plant_order", "barcode")]
  
  ## Create a list of maps
  map_list <- list(plot.layout = plot.lay, rep.layout = rep.lay, check.layout = check.lay, plant.order = plant_order)
  
  ## Output an excel file, if desired
  if (write.excel) {
    filename <- paste0(design$trial[1], "_rand.xlsx")
    write_xlsx(x = list(data.book = design_toprint, map.file = plot.map1), path = filename, col_names = TRUE)
    
    ## Create a list
    list_to_print <- list(data.book = design_toprint, map.file = map_list)
    
  } else {
    ## Include the maps to print in the list
    list_to_print <- list(data.book = design_toprint, map.file = map_list, map.to.print = plot.map1) 
    
  }
  
  return(structure(list_to_print, class = c("exp.design", "rcbd")))
  
}




#' Create experimental design randomizations
#' 
#' @param minimum.blocks The minimum number of incomplete blocks. Defalut is 6
#' @param chk2rep The number of replications of the secondary check. Default is 3.
#' @rdname make.rcbd
#' 
#' @examples 
#' 
#' @import dplyr
#' @import stringr
#' @import writexl
#' 
#' @export
#' 
#' 
make.aibd <- function(location, location.id, trial, trial.id, entries, plot.start = 1, minimum.blocks = 6, number.rows, year, crop.id, 
                      checks, chk2rep = 3, fill.with = c("check", "entry", "filler"), write.excel = FALSE) {
  
  ## Check use inputs
  location <- as.character(location)
  location.id <- formatC(x = as.integer(location.id), width = 2, flag = "0")
  trial <- as.character(trial)
  trial.id <- formatC(x = as.integer(trial.id), width = 2, flag = "0")
  plot.start <- as.integer(plot.start)
  if (!nchar(plot.start) == 4) stop("'plot.start' must be 4 digits.")
  minimum.blocks <- as.integer(minimum.blocks)
  year <- as.integer(year)
  crop.id <- as.integer(crop.id)
  number.rows <- as.integer(number.rows)
  chk2rep <- as.integer(chk2rep)
  
  ## Fill.with must be length 1
  if (length(fill.with) > 1) stop("'fill.with' must be a character vector of length 1.")
  
  
  ## Files to read in
  # Check extensions
  if (!str_detect(entries, ".csv")) stop ("The 'entries' file does not have the .csv extension.")
  if (!str_detect(checks, ".csv")) stop ("The 'checks' file does not have the .csv extension.")
  
  entries_df <- read.csv(entries, stringsAsFactors = FALSE)
  checks_df <- read.csv(checks, stringsAsFactors = FALSE)

  
  # Check if any checks appear in the entry list
  conflict.chk <- intersect(checks_df[,1], entries_df[,1])
  
  ## Entries are the individuals in the entries file, minus any overlap with the checks
  entries <- setdiff(x = entries_df[,1], y = checks_df[,1])
  # Checks are lines in the check file
  checks <- checks_df[,1]
  
  
  
  ###
  # Fill with options
  fillWithEntry <- fill.with == "entry"
  fillWithChk <- fill.with == "check"

  ###
  aibd <- design.aibd(enviro = location, exp.name = trial, chks = checks, nChk2.min = chk2rep,
                      entries = entries, nFieldRows = number.rows, nBlks.min = minimum.blocks,
                      plot.start = plot.start, fillWithEntry = fillWithEntry, fillWithChk = fillWithChk)
  
  
  # Make the map file
  plot.lay <- as.data.frame(aibd$plot.layout, stringsAsFactors = FALSE)
  blk.lay <- as.data.frame(aibd$blk.layout, stringsAsFactors = FALSE)
  check.lay <- as.data.frame(aibd$check.layout, stringsAsFactors = FALSE)
  
  # Make the data file
  # Make the data file
  design <- aibd$aibd.dgn
  # Sort on plot
  design <- design[order(design$plot), , drop = FALSE]
  
  
  design$environment <- as.character(design$environment)
  # Add the location code
  design$loc_code <- location.id
  # Add the experiment code and experiment
  design$trial <- trial
  design$trial_code <- trial.id
  # Add the zurn code
  design$barcode <- paste0(crop.id, location.id, trial.id, design$replication, design$plot)
  design$barcode[design$line_name == "FILLER"] <- NA 
  
  ## If the line_name is FILLER, and fill.with is not check, entry, or fill, use the fill with
  if (! fill.with %in% c("check", "entry", "filler")) {
    design$line_name[design$line_name == "FILLER"] <- fill.with
  }
  
  
  # Make a planting order
  design1 <- subset(design, , c("row", "column", "plant_order"))
  plant_order <- reshape(data = design1, idvar = "row", timevar = "column", direction = "wide")
  ## Flip
  plant_order <- plant_order[rev(seq(nrow(plant_order))),,drop = FALSE]
  ## Add a bottom row
  plant_order <- rbind(plant_order, c("", seq(ncol(plant_order) - 1)))
  # Remove row and column names
  row.names(plant_order) <- NULL; colnames(plant_order) <- colnames(blk.lay)
  
  
  # ## Combine layouts and order to a map
  # rep.lay1 <- rep.lay[-nrow(rep.lay),]; colnames(rep.lay1) <- c("row/col", seq(ncol(rep.lay1) - 1))
  # check.lay1 <- check.lay[-nrow(check.lay),]; colnames(check.lay1) <- c("row/col", seq(ncol(check.lay1) - 1))
  
  
  ## Number of columns
  n_col <- ncol(plot.lay)
  
  
  ## Combine
  plot.map1 <- rbind(c("Plot Layout", rep(NA, n_col - 1)), plot.lay, NA, c("Rep Layout", rep(NA, n_col - 1)), blk.lay,
                     NA, c("Check Layout", rep(NA, n_col - 1)), check.lay, NA, c("Plant Order", rep(NA, n_col - 1)), plant_order)
  colnames(plot.map1) <- NULL
  
  # Edit trial 
  design$trial <- paste(trial, year, location, sep = "_")
  # Edit line name
  design$line_name <- toupper(design$line_name)
  
  # Keep relevant columns
  design_toprint <- design[,c("trial", "line_name", "plot", "blk", "row", "column", "row_blk", "col_blk", "line_code", "entry", "plant_order", "barcode")]
  
  ## Create a list of maps
  map_list <- list(plot.layout = plot.lay, rep.layout = blk.lay, check.layout = check.lay, plant.order = plant_order)
  
  ## Output an excel file, if desired
  if (write.excel) {
    filename <- paste0(design$trial[1], "_rand.xlsx")
    write_xlsx(x = list(data.book = design_toprint, map.file = plot.map1), path = filename, col_names = TRUE)
    
    ## Create a list
    list_to_print <- list(data.book = design_toprint, map.file = map_list)
    
  } else {
    ## Include the maps to print in the list
    list_to_print <- list(data.book = design_toprint, map.file = map_list, map.to.print = plot.map1) 
    
  }
  

  return(structure(list_to_print, class = c("exp.design", "aibd")))
  
}


#' Summarize an experimental design object
#' 
#' @param x The output from an experimental design function, such as make.rcbd or make.aibd.
#' 
#' @method summary exp.design
#' 
#' @export
#' 
summary.exp.design <- function(x) {
  
  # Extract commomn information
  trial <- unique(x$data.book$trial)
  plots <- length(unique(x$data.book$plot))
  rows <- max(x$data.book$row)
  cols <- max(x$data.book$column)

  n_entries <- sum(x$data.book$line_code == 0, na.rm = TRUE)
  n_check <- sum(x$data.book$line_code != 0, na.rm = TRUE)
  n_fillers <- sum(is.na(x$data.book$line_code))
  
  # Blocks or reps
  if ("rcbd" %in% class(x)) {
    reps <- length(unique(x$data.book$replication))
  } else if ("aibd" %in% class(x)) {
    reps <- length(unique(x$data.book$blk))
  }
  
  ## Print
  cat("\nTrial name: ", trial)
  cat("\nDesign type: ", toupper(class(x)[2]))
  
  cat("\n\nNumber of rows: ", rows)
  cat("\nNumber of columns: ", cols)
  cat("\nNumber of plots: ", plots)
  
  cat("\n\nNumber of entries: ", n_entries)
  cat("\nNumber of checks: ", n_check)
  cat("\nNumber of filled plots: ", n_fillers)
  cat("\nNumber of reps/blocks: ", reps)

}

#' Summarize an experimental design object
#' 
#' @rdname summary.exp.design
#' 
#' @method print exp.design
#' 
#' @export
#' 
print.exp.design <- function(x) summary(x)

#' Summarize an experimental design object
#' 
#' @rdname summary.exp.design
#' 
#' @method plot exp.design
#' 
#' @export
#' 
plot.exp.design <- function(x) {
  
  ## Plot the map for a design object
  
  # Get the maps
  x_maps <- x$map.file
  
  # For each map, convert to matrix
  x_maps1 <- lapply(X = x_maps[-1], FUN = function(m) {
    m1 <- apply(X = as.matrix(m)[-nrow(m),-1], MARGIN = 2, FUN = as.numeric)
    row.names(m1) <- seq(nrow(m1))
    m1
  })
  
  
  ## Plot setting
  par(mfrow = c(1, 2))
  
  
  ## Number of rows/columns
  rows <- nrow(x_maps1$rep.layout)
  cols <- ncol(x_maps1$rep.layout)
  nCheck <- length(unique(as.numeric(x_maps1$check.layout))) - 1
  nRep <- length(unique(as.numeric(x_maps1$rep.layout)))
  
  ## x and y limits
  x_limit <- seq(0, 1, length.out = cols)
  y_limit <- seq(0, 1, length.out = rows)
  x_diff <- diff(x_limit)[1] / 2
  y_diff <- diff(y_limit)[1] / 2
  
  
  ## Plot checks
  toplot <- t(x_maps1$check.layout)
  # Empty plot
  blank_plot <- matrix(data = 0, nrow = nrow(toplot), ncol = ncol(toplot))
  # Colors
  colors <- c("white", rainbow(n = nCheck))
  
  image(blank_plot, col = "white", axes = FALSE, xlab = "Columns", ylab = "Rows", main = "Check Positions")
  axis(2, at = y_limit, labels = seq(rows))
  axis(1, at = x_limit, labels = seq(cols))
  ## Iterate and create rectangles
  for (i in seq_along(x_limit)) {
    for (j in seq_along(y_limit)) {
      x <- x_limit[i]; y <- y_limit[j]
      # Check or 
      rect(xleft = x - x_diff, ybottom = y - y_diff, xright = x + x_diff, ytop = y + y_diff, col = colors[toplot[i,j] + 1])
    }
  }
  
  
  
  ## Plot reps
  toplot <- t(x_maps1$rep.layout)
  # Empty plot
  blank_plot <- matrix(data = 0, nrow = nrow(toplot), ncol = ncol(toplot))
  # Colors
  colors <- rainbow(n = nRep)
  
  image(blank_plot, col = "white", axes = FALSE, xlab = "Columns", ylab = "Rows", main = "Rep/Block Positions")
  axis(2, at = y_limit, labels = seq(rows))
  axis(1, at = x_limit, labels = seq(cols))
  ## Iterate and create rectangles
  for (i in seq_along(x_limit)) {
    for (j in seq_along(y_limit)) {
      x <- x_limit[i]; y <- y_limit[j]
      # Check or 
      rect(xleft = x - x_diff, ybottom = y - y_diff, xright = x + x_diff, ytop = y + y_diff, col = colors[toplot[i,j]])
    }
  }
  
}






