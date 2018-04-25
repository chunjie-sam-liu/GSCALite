# sourced by 'ui.R'
# save as 'about_ui.R'
# ui elements for about


tabItem(
  tabName = "contact", align = "center",
  
  # Welcome message ----
  fluidRow(
    style = "width:80%;", 
    
    column(
      width = 12, offset = 0, align = "left",
      
      shiny::tags$h1(
        class = "text-success text-left",
        shiny::icon(name = "angle-double-right", class = "fa-fw"),
        "Contact"
      ),
      shiny::hr(),
      
      shiny::tags$address(
        shiny::tags$p(shiny::tags$strong("An-Yuan Guo, Ph.D. Professor of Bioinformatics")),
        shiny::tags$p(shiny::tags$strong("Email: guoay@hust.edu.cn"))
        ),
      shiny::tags$address(
        shiny::tags$p(shiny::tags$strong("Chun-Jie Liu, Ph.D. Candidate")),
        shiny::tags$p(shiny::tags$strong("Email: samliu@hust.edu.cn"))
      ),
      shiny::tags$address(
        shiny::tags$p(shiny::tags$strong("Fei-Fei LHu, Ph.D. Candidate")),
        shiny::tags$p(shiny::tags$strong("Email: hufeifei@hust.edu.cn"))
      ),
      shiny::tags$address(
        shiny::tags$p(shiny::tags$strong("Qiong Zhang, Postdoc Fellow")),
        shiny::tags$p(shiny::tags$strong("Email: zhangqiong@hust.edu.cn"))
      )
      # shiny::tags$img(
      #   src = "./imgs/about.png",
      #   class = "center-block img-responsive",
      #   style = "height: 600px;"
      # ),
      # shiny::tags$h1("Main authors contribute to this work.")
    )
  ),
  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
) # End of tabItem