# ui.R

library(shiny)

# To label inputs, the section title is give first, followed by the section parameter
# i.e. hwe_p

# Initialize the layout with a navbar
fluidPage(
  
  ## Just one page - generate randomization
  
  # Title 
  titlePanel("Experimental design randomization generator"),
  
  # Create a sidebar panel
  sidebarLayout(
    
    # Panel for the side bar
    sidebarPanel(
      
      ## Input - select design type (i.e. RCBD or AIBD)
      radioButtons("des", "Design Type", choices = c("RCBD", "AIBD"), selected = "RCBD"),
      
      ## Input - location
      textInput(inputId = "loc",
                label = "Location Code:"),
      
      numericInput(inputId = "loc.id",
                   label = "Location id:",
                   value = 1),
      
      ## Input - experiment
      textInput(inputId = "exp",
                label = "Experiment:"),
      
      ## Input - experiment code
      numericInput(inputId = "exp.id",
                   label = "Experiment code:",
                   value = 1),

      ## Zurn seed
      selectInput(inputId = "zurn",
                  label = "Zurn crop code:",
                  choices = c("3 - barley", "5 - oat")),
      
      ## Input - number of rows, blocks, etc.
      numericInput(inputId = "plot.start",
                   label = "Starting plot number:",
                   value = 1001),
      
      numericInput(inputId = "rows",
                   label = "Number of beds/field rows:",
                   value = 10),
      
      numericInput(inputId = "year",
                   label = "Experiment year:",
                   value = format(Sys.Date(), "%Y")),
      
      numericInput(inputId = "blk",
                   label = "Number of blocks/reps:",
                   value = 10),
      
      ## Conditional panel with number of checks
      conditionalPanel(
        condition = "input.des == 'RCBD'",

        # Input - check reps (RCBD)
        numericInput(inputId = "check.rep",
                     label = "Number of check replicates:",
                     value = 1)
      ),
      
      
      ## Conditional panel with number of checks
      conditionalPanel(
        condition = "input.des == 'AIBD'",
        
        ## Input - check 2 reps (AIBD)
        numericInput(inputId = "check2.rep",
                     label = "Number of secondary check replicates:",
                     value = 1)
      ),
      

      ## Input - fill with
      selectInput(inputId = "fillWith",
                  label = "Fill extra plots with...",
                  choices = c("check", "entry", "filler", "custom")),
      
      ## Conditional panel with custom fill
      conditionalPanel(
        condition = "input.fillWith == 'custom'",
        # If custon, requre input
        textInput(inputId = "fill.with.custom",
                  label = "Custom fill:")
      ),
      
      
      ## Horizontal line
      tags$hr(),
      
      ## Uploading files
      # Entries
      fileInput("entry", "Choose CSV file for entries:",
                multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      # Checks
      fileInput("checks", "Choose CSV file for checks:",
                multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      
      ## Horizontal line
      tags$hr(),
      
      ## Creation button
      actionButton(inputId = "create", label = "Create Randomization"),
      
      tags$p(),
      
      
      ## Download button - this will depend on the above data being passed
      downloadButton("randomization", "Download")
      
      
    ),
    
    ## Main panel for output
    mainPanel(
      
      ## Plot with colors assigned to checks, entries, and filler
      plotOutput("entry_plot"),
      
      # Summary
      verbatimTextOutput("summ"),
      
      # Head
      tableOutput("data_head")
      
    )
    
  )
  
) # Close the page
      
## End of UI