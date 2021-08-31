email <- function(.txt) {
  grid::grid.newpage()
  grid::grid.text(
    label = .txt, hjust = 0, vjust = 1, x = 0, y = 1, 
    gp = grid::gpar(col = "#008176", fontsize = 18)
  )
}

output$ay <- shiny::renderPlot({email('guoay@hust.edu.cn')})
output$cj <- shiny::renderPlot({email('chunjie-sam-liu@foxmail.com')})
output$ff <- shiny::renderPlot(email('hufeifei@wust.edu.cn'))
output$zq <- shiny::renderPlot(email('zhangqiong@hust.edu.cn'))
