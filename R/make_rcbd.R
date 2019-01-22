#' Create an randomized complete block experiment.
#' 
#' @description 
#' This function creates randomized complete block designed (RCBD) experiments.
#' 
#' @param loc.to.use The name of the location, writting as a three-letter (or more) code.
#' @param loc.id The two-digit zurn number for the location.
#' @param trial.to.use The name of the trial.
#' @param trial.id The two-digit zurn number for trial.
#' @param entries A csv file i.e.("18 VT 9jan18.csv") with one column "line" which has the list of the entries for the VT
#' @param num.beds The number of beds / field rows.
#' @param plot.start The number of the first plot i.e. 0 or 1 or 1000 which ever, Default is 1001
#' @param number.blocks The number of blocks to use i.e. 0 or 1 or 2 or 3. Defalut is 3
#' @param year The year of the planting.
#' @param zurn.seed The zurn code for the crop i.e. 5 for oat or 3 for barley 
#' @param checks A csv file i.e.("18 VT 9jan18.csv") with one column "check" which has the list of the checks.
#' @param num.reps.chk The number of check replications. Default is 1.
#' @param fill.with A character determining what should fill empty plots. If "check," plots will be filled with checks; if "entry", plots will
#' be filled with entries; if "filler", plots will be filled with "FILLER"; alternatively, a custom string can be supplied (i.e. "RASMUSSEN").
#' @param write.excel Should the design and map files be exported to an .xlsx file? The name of the file will be "trial_year_loc_rand.xlsx"
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
make.rcbd <- function(loc.to.use, loc.id, trial, trial.id, entries, plot.start = 1001, number.blocks = 3, num.beds, year, zurn.seed, 
                      checks, num.reps.chk = 1, fill.with = c("check", "entry", "filler"), write.excel = FALSE) {
  
  ## Check use inputs
  loc.to.use <- as.character(loc.to.use)
  loc.id <- formatC(x = as.integer(loc.id), width = 2, flag = "0")
  trial <- as.character(trial)
  trial.id <- formatC(x = as.integer(trial.id), width = 2, flag = "0")
  plot.start <- as.integer(plot.start)
  if (!nchar(plot.start) == 4) stop("'plot.start' must be 4 digits.")
  number.blocks <- as.integer(number.blocks)
  year <- as.integer(year)
  zurn.seed <- as.integer(zurn.seed)
  num.beds <- as.integer(num.beds)
  num.reps.chk <- as.integer(num.reps.chk)
  
  ## Fill.with must be length 1
  if (length(fill.with) > 1) stop("'fill.with' must be a character vector of length 1.")
  
  
  ## Files to read in
  # Check extensions
  if (!str_detect(entries, ".csv")) stop ("The 'entries' file does not have the .csv extension.")
  if (!str_detect(checks, ".csv")) stop ("The 'checks' file does not have the .csv extension.")
  
  entries_df <- read.csv(entries, stringsAsFactors = FALSE)
  if (!all(c("line") %in% names(entries_df))) stop("Columns in 'entries' should be 'line'.")
  checks_df <- read.csv(checks, stringsAsFactors = FALSE)
  if (!all(c("line") %in% names(checks_df))) stop("Columns in 'checks' should be 'line'.")

  
  # Checking if the checks are not a entry
  conflict.chk <- intersect(checks_df$line, entries_df$line)
  if(length(conflict.chk) > 0) stop(paste0("Checks must not appear in the entry list, suggest add '_chk' to ", conflict.chk))
  #
    
  ###
  # Fill with options
  fillWithEntry <- fill.with == "entry"
  fillWithChk <- fill.with == "check"
  
  ###
  rcbd <- design.rcbd(enviro = loc.to.use, exp.name = trial, chks = checks_df$line, nChkReps = num.reps.chk,
                      nBlks = number.blocks, entries = entries_df$line, nChks = 0, nFieldRows = num.beds, 
                      plot.start = plot.start, fillWithEntry = fillWithEntry, fillWithChk = fillWithChk)
  
  
  # Make the map file
  plot.lay <- as.data.frame(rcbd$plot.layout, stringsAsFactors = FALSE)
  rep.lay <- as.data.frame(rcbd$rep.layout, stringsAsFactors = FALSE)
  check.lay <- as.data.frame(rcbd$check.layout, stringsAsFactors = FALSE)

  # Make the data file
  design <- rcbd$rcbd.dgn
  design$environment <- as.character(design$environment)
  # Add the location code
  design$loc_code <- loc.id
  # Add the experiment code and experiment
  design$trial <- trial
  design$trial_code <- trial.id
  # Add the zurn code
  design$barcode <- paste0(zurn.seed, loc.id, trial.id, design$replication, design$plot)
  design$barcode[design$line_name == "FILLER"] <- NA 
  
  ## If the line_name is FILLER, and fill.with is not check, entry, or fill, use the fill with
  if (! fill.with %in% c("check", "entry", "filler")) {
    design$line_name[design$line_name == "FILLER"] <- fill.with
  }
  
  # Make a planting order
  design1 <- subset(design, , c("row", "column", "plant_order"))
  plant_order <- reshape(data = design1, idvar = "row", timevar = "column", direction = "wide")
  colnames(plant_order) <- c("row/col", seq(ncol(plant_order) - 1))
  
  
  ## Combine layouts and order to a map
  plot.lay1 <- plot.lay[-nrow(plot.lay),]; colnames(plot.lay1) <- c("row/col", seq(ncol(plot.lay1) - 1))
  rep.lay1 <- rep.lay[-nrow(rep.lay),]; colnames(rep.lay1) <- c("row/col", seq(ncol(rep.lay1) - 1))
  check.lay1 <- check.lay[-nrow(check.lay),]; colnames(check.lay1) <- c("row/col", seq(ncol(check.lay1) - 1))

  ## Number of columns
  n_col <- ncol(plot.lay1)
  # Column names
  colnm <- colnames(plot.lay1)
  
  
  ## Combine
  plot.map1 <- rbind(c("Plot Layout", rep(NA, n_col - 1)), colnm, plot.lay1, NA, c("Rep Layout", rep(NA, n_col - 1)), colnm, rep.lay1,
                     NA, c("Check Layout", rep(NA, n_col - 1)), colnm, check.lay1, NA, c("Plant Order", rep(NA, n_col - 1)), colnm, plant_order)
  colnames(plot.map1) <- NULL
  
  # Edit trial 
  design$trial <- paste(trial, year, loc.to.use, sep = "_")
  # Edit line name
  design$line_name <- toupper(design$line_name)
  
  # Keep relevant columns
  design_toprint <- design[,c("trial", "line_name", "plot", "replication", "row", "column", "line_code", "entry", "plant_order", "barcode")]
  
  ## Create a list
  list_to_print <- list(data.book = design_toprint, map.file = plot.map1)
  
  ## Output an excel file, if desired
  if (write.excel) {
    filename <- paste0(design$trial[1], "_rand.xlsx")
    write_xlsx(x = list_to_print, path = filename, col_names = TRUE)
  }
  
  return(list_to_print)
  
}


