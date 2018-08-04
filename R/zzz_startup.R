# This script is from the tidyverse package

# Function to run when the package is attached
.onAttach <- function(...) {
  
  packageStartupMessage("Loading BreedR...")
  
  packageStartupMessage("Loading required packages...")
  suppressPackageStartupMessages(invisible(lapply(X = c("readr", "dplyr"), FUN = library, character.only = TRUE, warn.conflicts = FALSE)))
  
  
  ## Species input - use while loop
  species <- ""
  species_accept <- c("barley", "oat", "silphium")
  acceptable <- species %in% species_accept
  
  while (!acceptable) {
    species <- readline(prompt = "Enter a species (barley/oat/silphium): ")
    
    # Error check
    acceptable <- species %in% species_accept
    if (!acceptable) cat("Species not accepted. Try again.")
    
  }
  
  # A function for reading in dbs
  read_db <- function(dir, pattern) {
    # List files
    files <- list.files(path = dir, pattern = pattern, full.names = TRUE)
    
    # If more than one, prompt
    len_files <- length(files)
    
    if (len_files > 1) {
      db_i <- readline(prompt = paste0("Multiple databases found. Enter the desired db number:\n\n", 
                                       paste(paste(seq(len_files), basename(files)), collapse = "\n")))
      
      # Make sure it's an integer
      db_i <- suppressWarnings(as.integer(db_i))
      while(is.na(db_i) | db_i > len_files) {
        cat("Integer not accepted. Try again.")
        
        db_i <- readline(prompt = paste0("Multiple databases found. Enter the desired db number:\n\n", 
                                         paste(paste(seq(len_files), basename(files)), collapse = "\n")))
        
        db_i <- suppressWarnings(as.integer(db_i))
      }
      
      # Designate the file to read
      path <- files[db_i]
      
    } else {
      path <- files
      
    }
    
    ## Read in the file and return
    suppressWarnings(suppressMessages(read_csv(path)))
    
  }
  
  ## Load the databases - only works for barley now
  if (species == "barley") {
    ## Find the network drive
    search_drives <- sapply(LETTERS, FUN = function(a) dir.exists(paste0(a, ":/BARLEY_LAB/")))
    if (!any(search_drives)) stop("The shared drive is not mounted. Check the connection and try again.")
    
    # Create the barley lab path
    barley_db_dir <- paste0(names(search_drives)[search_drives], ":/BARLEY_LAB/barley_db")
    
    ## Load the experiment db
    cat("Reading the experiment database...\n")
    expt_db <- read_db(dir = barley_db_dir, pattern = "expt")
    expt_db <- expt_db[,!startsWith(names(expt_db), "X")]
    
    cat("Reading the field database...\n")
    fld_db <- read_db(dir = barley_db_dir, pattern = "field")
    fld_db <- fld_db[,!startsWith(names(fld_db), "X")]
    
    cat("Reading the post-harvest database...\n")
    post_db <- read_db(dir = barley_db_dir, pattern = "post_harvest")
    post_db <- post_db[,!startsWith(names(post_db), "X")]
    
    
    
    ### 
    
  }

  
}
