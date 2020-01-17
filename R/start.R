#' Startup and connect to the database
#' 
#' @description 
#' Connects to the database for the crop of interest.
#' 
#' @param species The crop species you are interested in.
#' 
#' @import dplyr
#' @import dbplyr
#' @import DBI
#' @importFrom RMariaDB MariaDB
#' @importFrom rstudioapi askForPassword
#' @import readr
#' 
#' @export
#' 
start <- function(species = c("barley", "oat", "silphium"), ssl.ca) {
  
  # Check the species
  species <- match.arg(species)
  
  ## Control flow depending on species
  if (species == "barley") {
    
    # Establish connection
    host <- "mysql-prod3.oit.umn.edu"
    user <- askForPassword(prompt = "Enter your username (x500)")
    pass <- askForPassword(prompt = "Enter your password")
    
    # connection
    con <- dbConnect(drv = MariaDB(), host = host, user = user, password = pass, db = "cfans_barleybreeding",
                     ssl.ca = ssl.ca, client.flag = "ENABLE_CLEARTEXT_PLUGIN")
    
  } else if (species == "oat") {
    
    stop("The oat database is not yet accessible.")
    
  } else if (species == "silphium") {
    
    stop("The silphium database is not yet accessible.")
    
  }
  
  ## Tell the user
  cat("Database loaded.")
  assign(x = "con", value = con, envir = .GlobalEnv)
    
}
