print(user_dir)
output$gene_set <- shiny::renderText({
  if (input$analysis == 0) return()
  
  raw_gene_set <- local(isolate({
    input$gene_set
  }))
  
  msg <- check_gene_set(.s = raw_gene_set)
  if (length(msg$errors > 0)) return(paste0(msg$errors, collapse = "\n"))
  
  msg <- validate_gene_set(.v = msg$gene_set, user_dir = user_dir, user_logs = user_logs)
  if (length(msg$errors > 0)) return(paste0(msg$errors, collapse = "\n"))
  msg$gene_set
})