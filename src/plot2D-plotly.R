## plot2D-plotly.R

# 建立简洁风格的画布（坐标系）
create_canvas_2d <- function() {
  plot_ly(
    data = data.table(x = 0, y = 0),
    x = ~x, y = ~y,
    ## width = 600, # 图形size，像素值
    ## height = 600,
    showlegend = F
  ) %>%
    layout(
      plot_bgcolor = "#ebebeb",
      xaxis = list(
        zeroline = FALSE,
        ticks = "outside",
        gridcolor = "white",
        gridwidth = 1.5,
        title = ""
      ),
      yaxis = list(
        zeroline = FALSE,
        ticks = "outside",
        gridcolor = "white",
        gridwidth = 1.5,
        title = ""
      ),
      legend = list(
        y = 0.5
      )
    )
}

# 画点，该函数可与3D版共用
draw_points <- function(p, points, color = "blue") {
  p %>% add_markers(
    data = points,
    marker = list(size = 5, color = color)
  )
}

# 两点间画线，该函数可与3D版共用
draw_segment <- function(p, point1, point2, color = "lightblue", linetype = "solid") {
  p %>% add_lines(
    data = rbind(point1, point2),
    line = list(
      color = color,
      width = 3,
      dash = linetype
    )
  )
}

# 绘制多边形
draw_polygon <- function(p, points, color = "blue") {
  n <- nrow(points)
  for (i in 1:n) {
    if (i < n) {
      p <- p %>% draw_segment(points[i, ], points[i + 1], color)
    } else {
      p <- p %>% draw_segment(points[n, ], points[1], color)
    }
  }
  return(p)
}