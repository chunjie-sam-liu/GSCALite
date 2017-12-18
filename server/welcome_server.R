print(user_dir)
output$output_gene_set <- shiny::renderText({
  # input <- reactiveValues("input_gene_set" = "tp53,PTeN,,hello,foo,bar,Atg5")
  if (input$input_gene_set_search == 0) return()
  
  input$input_gene_set_search
  
  msg <- check_gene_set(.s = input$input_gene_set)
  if (length(msg$errors > 0)) return(paste0(msg$errors, collapse = "\n"))
  
  msg <- validate_gene_set(.v = msg$gene_set, user_dir = user_dir, user_logs = user_logs)
  if (length(msg$errors > 0)) return(paste0(msg$errors, collapse = "\n"))
  msg$gene_set
})