# server.R

library(shiny)
library(BreedR, quietly = TRUE)
library(writexl)


shinyServer(function(input, output) {
  
  design_rand <- eventReactive(eventExpr = input$create, valueExpr = {
  
    # Create an AIBD or RCBD design
  
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, the RCBD or AIBD design will be generated
    
    req(input$entry, input$checks)
    
    ## Save the other inputs
    loc.to.use <- input$loc
    loc.id <- input$loc.id
    plot.start <- input$plot.start
    trial <- input$exp
    trial.id <- input$exp.id
    zurn.seed <- substr(x = input$zurn, start = 1, stop = 1)
    rows <- input$rows
    year <- input$year
    block <- input$blk
    fill.with <- input$fillWith
    
    ## These will depend on other inputs
    check2.rep <- input$check2.rep
    check.rep <- input$check.rep
    fill.with.custom <- input$fill.with.custom
    
    ## The design
    design <- input$des
    
    # Determine the fill
    fill.with.use <- if (fill.with == "custom") fill.with.custom else fill.with
    
    ## Create the specified design
    if (design == "RCBD") {
    
      rand <- make.rcbd(loc.to.use = loc.to.use, loc.id = loc.id, trial = trial, trial.id = trial.id, plot.start = plot.start, 
                        number.blocks = block, num.beds = rows, year = year, zurn.seed = zurn.seed, fill.with = fill.with.use, num.reps.chk = check.rep, 
                        entries = input$entry$datapath, checks = input$checks$datapath, write.excel = FALSE)
      
    } else {
      
      rand <- make.aibd(loc.to.use = loc.to.use, loc.id = loc.id, trial = trial, trial.id = trial.id, plot.start = plot.start, number.blocks = block,
                        num.beds = rows, year = year, zurn.seed = zurn.seed, num.reps.chk = num.reps.chk, chk2rep = check2.rep,
                        fill.with = fill.with.use, entries = input$entry$datapath, checks = input$checks$datapath, write.excel = FALSE)
      
    }
    
    # Return the randomization
    rand
    
  })
  
  
  ## Plot
  output$entry_plot <- renderPlot({
    plot(design_rand())
  })
  
  ## Summarize
  output$summ <- renderPrint({
    summary(design_rand())
  })
  
  # First few lines of table
  output$data_head <- renderTable({
    head(design_rand()$data.book)
  })
  
  
  
  ## Separate server for downloading the data
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    
    filename = function() {
      filename <- paste0(design_rand()$data.book$trial[1], "_rand.xlsx")
    },
    
    content = function(file) {
      write_xlsx(x = design_rand()[c(1,3)], path = file)
    },
    
    contentType = "text/xlsx"
    
  )
  
})
  
  
  
  
  ## Attempt to 