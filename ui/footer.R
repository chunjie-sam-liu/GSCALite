# saved as footer.r
shiny::fluidRow(
  style = "text-align:center",
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
  shiny::tags$p("Any comments and suggestions, please contact us."),
  shiny::tags$div(
    class = "row",
    shiny::tags$div(
      class = "col-md-offset-3 col-md-3",
      shiny::HTML(text = '<div style="display:inline-block;width:200px;">
              <script type="text/javascript" src="//rf.revolvermaps.com/0/0/7.js?i=5yt30uug30e&amp;m=1&amp;c=ff0000&amp;cr1=ffffff&amp;sx=0" async="async"></script>
                  </div>')
    ),
    shiny::tags$div(
      class = "col-md-3",
      shiny::HTML(text = '
                  <script type="text/javascript" src="https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js"></script>
                  <div style="display:inline-block;width:200px;margin-top:20px;" class="altmetric-embed" data-badge-type="donut" data-altmetric-id="42431062"></div>')
    )
  )
)