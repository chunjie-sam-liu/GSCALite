print(user_dir)


# For reseting ------------------------------------------------------------

observeEvent(input$input_gene_set_reset, {
  shinyjs::reset("input_gene_set")
  output$output_gene_set <- shiny::renderText({""})
})


# For seaching ------------------------------------------------------------

observeEvent(input$input_gene_set_search, {
  
  output$output_gene_set <- shiny::renderText({
    msg <- check_gene_set(.s = input$input_gene_set, status = status, error = error)
    
    if (status$gene_set) { return(paste0(error$gene_set, collapse = "\n")) }
    
    msg <- validate_gene_set(
      .v = msg$gene_set, 
      user_dir = user_dir, 
      user_logs = user_logs, 
      total_gene_symbol = total_gene_symbol
      )
    
    if (length(msg$errors > 0)) {
      return(paste0(msg$errors, collapse = "\n"))
    } else {
      withProgress(message = 'Making plot', value = 0, {
        # Number of times we'll go through the loop
        n <- 10
        
        for (i in 1:n) {
          # Each time through the loop, add another row of data. This is
          # a stand-in for a long-running computation.
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Doing part", i))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
        }
      })
    }
    
    return("")
  })
})

