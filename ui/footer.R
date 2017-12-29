# saved as footer.r
shiny::fluidRow(
  shiny::tags$hr(),
  shiny::tags$p(
    "Copyright ©",
    shiny::tags$a("Guo Lab", href = "http://bioinfo.life.hust.edu.cn/home_page#!/", target = "_blank", style = "color:#008176"),
    ",",
    shiny::tags$a("College of Life Science and Technology", href = "http://life.hust.edu.cn/", target = "_blank", style = "color:#008176"),
    ",",
    shiny::tags$a("HUST", href = "http://www.hust.edu.cn/", target = "_blank", style = "color:#008176"),
    ", China"
  ),
  shiny::tags$p(
    shiny::tags$a("Han Lab", href = "https://med.uth.edu/bmb/faculty/leng-han-ph-d/", target = "_blank", style = "color:#008176"),
    ",",
    shiny::tags$a("UTHealth", href = "https://med.uth.edu/", target = "_blank", style = "color:#008176"),
    " Houston, USA"
  ),
  shiny::tags$p("Any comments and suggestions, please contact us.")
)