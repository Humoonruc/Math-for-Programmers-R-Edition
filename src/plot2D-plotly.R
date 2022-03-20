## plot2D-plotly.R


# config
axis_template <- list(
  zeroline = TRUE,
  ticks = "outside",
  gridcolor = "white",
  gridwidth = 1.5,
  nticks = 20
)


# 建立画布（坐标系）
create_canvas_2d <- function(title_x = "", title_y = "") {
  plot_ly(
    data = data.table(x = 0, y = 0),
    x = ~x, y = ~y,
    showlegend = F,
    width = 800,
    height = 600
  ) %>%
    layout(
      title = list(
        font = list(size = 28),
        y = 0, yanchor = "top", yref = "paper",
        pad = list(t = 35)
      ),
      margin = list(b = 80, l = 30, r = 20),
      plot_bgcolor = "#ebebeb",
      ## legend = list(
      ##   y = 0.5
      ## ),
      xaxis = c(axis_template, title = title_x),
      yaxis = c(axis_template, list(
        scaleanchor = "x", # 使y轴与x轴的比例尺相同
        scaleratio = 1, # 从而grid为方格
        title = title_y
      ))
    )
}


# 画点，该函数可与3D版共用
draw_point <- function(p, points, color = "blue") {
  p %>% add_markers(
    data = points,
    marker = list(size = 4, color = color)
  )
}


# 画线段，该函数可与3D版共用
draw_line <- function(p, points,
                      color = "royalblue",
                      linetype = "solid",
                      width = 1.5) {
  p %>% add_lines(
    data = points,
    line = list(
      color = color,
      width = width,
      dash = linetype
    )
  )
}


# 画箭头
draw_arrow_2d <- function(p, begin, end,
                          color = "dimgrey",
                          linetype = "solid",
                          width = 2) {
  p %>%
    draw_line(rbind(begin, end),
      color = color, linetype = linetype, width = width
    ) %>%
    add_annotations(
      x = end$x, # 箭头坐标
      y = end$y,
      xref = "x",
      yref = "y",
      ax = 0.1 * begin$x + 0.9 * end$x, # 箭尾坐标
      ay = 0.1 * begin$y + 0.9 * end$y,
      axref = "x",
      ayref = "y",
      arrowcolor = color,
      arrowhead = 2, # 箭头类型
      arrowsize = 1.5,
      arrowwidth = 1.5,
      text = ""
    )
}


# 在数据点上标注文本，该函数可与3D版共用
draw_text <- function(p, points, texts,
                      color = "black", position = "top center") {
  p %>%
    add_text(
      data = points,
      text = texts,
      textfont = list(
        size = 16, color = color
      ),
      textposition = position
    )
}


# 绘制多边形
draw_polygon <- function(p, points, fill = "grey", opacity = 1) {
  p %>% add_polygons(
    data = points,
    fillcolor = fill,
    line = list(color = "black", width = 1),
    opacity = opacity
  )
}
