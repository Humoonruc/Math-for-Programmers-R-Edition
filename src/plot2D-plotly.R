## plot2D-plotly.R


# config
axis_template <- list(
  zeroline = TRUE,
  ticks = "outside",
  gridcolor = "white",
  gridwidth = 1.5,
  nticks = 20
)
title_template <- list(
  font = list(size = 28),
  y = 0, yanchor = "top", yref = "paper",
  pad = list(t = 35)
)
margin_tempalte <- list(b = 80, l = 30, r = 20)


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
      title = title_template,
      margin = margin_tempalte,
      plot_bgcolor = "#ebebeb",
      xaxis = c(axis_template, title = title_x),
      yaxis = c(
        axis_template,
        list(
          # 使两个轴的比例尺相同
          scaleanchor = "x",
          scaleratio = 1,
          title = title_y
        )
      ),
      legend = list(
        y = 0.5
      )
    )
}


# 画点，该函数可与3D版共用
draw_point <- function(p, points, color = "blue") {
  p %>% add_markers(
    data = points,
    x = ~x, y = ~y,
    marker = list(size = 6, color = color)
  )
}


# 画线段，该函数可与3D版共用
draw_line <- function(p, point1, point2,
                      color = "royalblue",
                      linetype = "solid",
                      width = 1.5) {
  p %>% add_lines(
    data = rbind(point1, point2),
    x = ~x, y = ~y,
    line = list(
      color = color,
      width = width,
      dash = linetype
    )
  )
}


# 画箭头
draw_arrow_2d <- function(p, point1, point2,
                          color = "dimgrey",
                          linetype = "solid",
                          width = 2) {
  p %>%
    draw_line(
      point1, point2,
      color = color, linetype = linetype, width = width
    ) %>%
    add_annotations(
      x = point2$x, # 箭头坐标
      y = point2$y,
      xref = "x",
      yref = "y",
      ax = 0.1 * point1$x + 0.9 * point2$x, # 箭尾坐标
      ay = 0.1 * point1$y + 0.9 * point2$y,
      axref = "x",
      ayref = "y",
      arrowcolor = color,
      arrowhead = 2, # 箭头类型
      arrowsize = 1.5,
      arrowwidth = 1.5,
      text = ""
    )
}


# 绘制多边形
draw_polygon <- function(p, points, color = "blue") {
  n <- nrow(points)
  for (i in 1:n) {
    if (i < n) {
      p <- p %>% draw_line(points[i, ], points[i + 1], color)
    } else {
      p <- p %>% draw_line(points[n, ], points[1], color)
    }
  }
  return(p)
}