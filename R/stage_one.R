#' Calculate adjusted means from a single trial 
#' 
#' @description 
#' Perform the first step of two-step phenotypic analysis, where genotype means are first
#' calculated on a per-trial basis, then analysis is performed using data across all trials.
#' 
#' @param data A data.frame with at least the following information: trait name, genotype name, and the trait value. Other
#' columns must be included if specified in models
#' @param checks A vector of genotype names to serve as checks. If not provided, any line that is replicated will be a check.
#' @param fixed A formula specifying fixed effects.
#' @param random A formula specifying random effects.
#' @param alpha The significance level at which to remove random effects during backwards elimination.
#' 
#' @importFrom lmerTest step lmer
#' @import broom
#' @import stringr
#' 
#' @export
#' 
fit_stage_one <- function(data, checks, fixed = ~ -1 + line + checks, random = ~ row + column + blk,
                          alpha = 0.05) {
  
  # Convert data to a data.frame
  data <- as.data.frame(data)
  
  ## Make sure a list of important columns is present
  needed_cols <- c("trait", "line_name", "value")
  # Error if these are not present
  cols_in_data <- needed_cols[!needed_cols %in% names(data)]
  if (length(cols_in_data) > 0) stop(paste("The following columns are needed by not present: ", cols_in_data))
  
  ## Issue warning if checks is missing
  if (missing(checks)) {
    warning("'checks' not provided. Any replicated entries will be used as checks.")
    line_count <- table(data[["line_name"]])
    
    checks <- names(line_count[line_count > 1])
    
  }
  
  ## Parse the fixed formula
  fixed_terms <- attr(terms(fixed), "term.labels")
  # If line_name is one of them, error
  if (any(fixed_terms == "line_name")) stop("'line_name' should not be used as a fixed effect. Use 'line' instead.")
  
  # If there are more than 2 fixed effects, error
  if (length(fixed_terms) > 2) stop("We advise setting genotype as the only fixed effects in the first stage of analysis.")
  
  # Get the non-check fixed effect
  non_check <- setdiff(fixed_terms, "checks")
  
  # Warn if checks is not present
  if (!"checks" %in% fixed_terms) {
    warning("'checks' was not specified as a fixed-effect.")
    data[[non_check]] <- data$line_name
    
  } else {
    # Add dummies for checks
    data[[non_check]] <- as.factor(ifelse(!data$line_name %in% checks, data$line_name, "00check"))
    data[["checks"]] <- as.factor(ifelse(data$line_name %in% checks, data$line_name, "00line"))
    
  }
  
  ## Parse the random effects
  rand_terms <- attr(terms(random), "term.labels")
  # If line_name is one of them, error
  if (any(rand_terms == "line_name")) stop("We advise setting genotype as the only fixed effects in the first stage of analysis.")
  
  # Contrasts
  contrs <- list()
  contrs[[non_check]] <- "contr.sum"
  
  
  ## Message
  cat("Fitting the full model...\n")
  
  ## If there are no random effects, continue with lm
  if (length(rand_terms) == 0) {
    
    # Create the full model formula
    full_form <- as.formula(paste0("value ~ ", as.character(fixed)[-1]))
    
    # Fit - no need for backwards elimination
    fit_final <- lm(formula = full_form, data = data, contrasts = contrs)
    back_elim <- NA
    
    
  } else {
    
    ## Otherwise use lmer
    # Make sure they are all in the data
    cols_in_data <- rand_terms[!rand_terms %in% names(data)]
    if (length(cols_in_data) > 0) stop(paste("The following columns are needed as random effects, by not present: ", cols_in_data))
    
    ## Remove random terms if the values are NA
    which_na <- sapply(data[rand_terms], function(x) all(is.na(x)))
    rand_terms <- rand_terms[!which_na]
    
    # Convert to a lmer formula
    rand_form <- as.formula(paste0("~ ", paste0("(1|", rand_terms, ")", collapse = " + ")))
    
    # Create the full model formula
    full_form <- as.formula(paste0("value ~", paste0(c(as.character(fixed)[-1], as.character(rand_form)[-1]), collapse = " + ")))
    
    # Fit
    fit <- lmer(formula = full_form, data = data, contrasts = contrs)
    
    # Message
    cat("Running backwards elimination...\n")
    
    # Backwards elimination
    back_elim <- step(fit, alpha.random = alpha, reduce.fixed = FALSE)
    fit_final <- get_model(back_elim)
    
  }
  
  
  ## Tidy based on model class
  # Tidy up
  if (class(fit_final) == "lm") {
    fit_final_tidy <- tidy(fit_final)
    
  } else {
    fit_final_tidy <- subset(tidy(structure(fit_final, class = "merMod")), group == "fixed")
    
  }
  
  # Get the levels of the checks
  check_levels <- levels(data[["checks"]])
  
  fit_tidy1 <- fit_final_tidy
  fit_tidy1$term <- if_else(fit_tidy1$term == "line00check", tail(check_levels, 1), 
                            str_replace_all(fit_tidy1$term, pattern = paste0(fixed_terms, collapse = "|"), replacement = ""))
  fit_tidy1$estimate <- if_else(fit_tidy1$term %in% head(check_levels, -1), fit_tidy1$estimate + fit_tidy1$estimate[1], fit_tidy1$estimate)
  names(fit_tidy1)[1] <- non_check
  fit_tidy2 <- fit_tidy1[c(non_check, "estimate", "std.error")]
  
  # Extract the formula from the best model
  final_form <- formula(fit_final)
  
  # Return the BLUEs and metadata
  list(
    BLUES = fit_tidy2,
    metadata = list(elimination = back_elim, final_formula = final_form)
  )
    
  
}
