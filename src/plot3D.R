## plot3D.R

create_canvas <- function(range) {
  origin <- data.table(x = 0, y = 0, z = 0)
  axis_x <- data.table(
    x = c(-range, range),
    y = c(0, 0),
    z = c(0, 0)
  )
  axis_y <- data.table(
    x = c(0, 0),
    y = c(-range, range),
    z = c(0, 0)
  )
  axis_z <- data.table(
    x = c(0, 0),
    y = c(0, 0),
    z = c(-range, range)
  )


  plot_ly(
    data = origin,
    x = ~x, y = ~y, z = ~z,
    width = 600, # 图形size，像素值
    height = 600,
    showlegend = F
  ) %>%
    add_lines(data = axis_x, line = list(color = "black")) %>%
    add_lines(data = axis_y, line = list(color = "black")) %>%
    add_lines(data = axis_z, line = list(color = "black")) %>%
    add_trace(
      type = "cone",
      x = range, y = 0, z = 0, # 箭头坐标
      u = 1, v = 0, w = 0,
      sizemode = "absolute", sizeref = 0.5,
      anchor = "tip", # 规定箭头坐标指顶点坐标
      colorscale = list(list(0, "black"), list(1, "black")), # 箭头单一颜色
      showscale = FALSE # 不显示颜色图例
    ) %>%
    add_trace(
      type = "cone",
      x = 0, y = range, z = 0, # 箭头坐标
      u = 0, v = 1, w = 0,
      sizemode = "absolute", sizeref = 0.5,
      anchor = "tip", # 规定箭头坐标指顶点坐标
      colorscale = list(list(0, "black"), list(1, "black")), # 箭头单一颜色
      showscale = FALSE # 不显示颜色图例
    ) %>%
    add_trace(
      type = "cone",
      x = 0, y = 0, z = range, # 箭头坐标
      u = 0, v = 0, w = 1,
      sizemode = "absolute", sizeref = 0.5,
      anchor = "tip", # 规定箭头坐标指顶点坐标
      colorscale = list(list(0, "black"), list(1, "black")), # 箭头单一颜色
      showscale = FALSE # 不显示颜色图例
    ) %>%
    add_text(x = range + 1.5, y = 0, z = -0.5, text = "X") %>%
    add_text(x = 0, y = range + 1.5, z = -0.5, text = "Y") %>%
    add_text(x = 0, y = 0, z = range + 1, text = "Z") %>%
    layout(
      scene = list(
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        zaxis = list(title = ""),
        camera = list(
          eye = list(x = 1.76, y = -1.3, z = 0.92)
        )
      ),
      legend = list(
        y = 0.5
      )
    )
}


draw_point <- function(p, points, color = "blue") {
  p %>%
    add_markers(
      data = points,
      marker = list(
        size = 3,
        color = color
      )
    )
}

draw_annotation <- function(p, text, point, color = "black") {
  p %>%
    add_text(
      text = text,
      x = point$x, y = point$y, z = point$z - 0.3,
      textfont = list(
        size = 18,
        family = "Arial",
        color = color
      )
    )
}

draw_segment <- function(p, point1, point2, color = "lightblue", linetype = "solid") {
  p %>%
    add_lines(
      data = rbind(point1, point2),
      line = list(
        color = color,
        width = 4,
        dash = linetype
      )
    )
}

draw_arrow <- function(p, begin = data.table(x = 0, y = 0, z = 0), end, color = "orangered", linetype = "solid") {
  p %>%
    draw_segment(begin, end, color = color, linetype = linetype) %>%
    add_trace(
      type = "cone",
      x = end$x, y = end$y, z = end$z, # 箭头坐标
      u = end$x - begin$x,
      v = end$y - begin$y,
      w = end$z - begin$z, # 箭头本身在三个坐标轴上的投影
      # sizemode = "absolute"后，三个投影之规定箭头的朝向，与箭头大小无关
      # 否则按默认值 sizemode='scaled'，上述投影大小即决定箭头的大小
      sizemode = "absolute", sizeref = 1,
      anchor = "tip", # 规定箭头坐标指顶点坐标
      colorscale = list(c(0, color), c(1, color)), # 箭头单一颜色
      showscale = FALSE # 不显示颜色图例
    )
}

draw_box <- function(p, point) {
  x <- point$x
  y <- point$y
  z <- point$z
  vertex1 <- data.table(x = x, y = 0, z = 0)
  vertex2 <- data.table(x = x, y = y, z = 0)
  vertex3 <- data.table(x = 0, y = y, z = 0)
  vertex4 <- data.table(x = x, y = 0, z = z)
  vertex5 <- data.table(x = 0, y = 0, z = z)
  vertex6 <- data.table(x = 0, y = y, z = z)

  p %>%
    draw_segment(vertex1, vertex2, "grey", "longdash") %>%
    draw_segment(vertex2, vertex3, "grey", "longdash") %>%
    draw_segment(vertex1, vertex4, "grey", "longdash") %>%
    draw_segment(vertex2, point, "grey", "longdash") %>%
    draw_segment(vertex3, vertex6, "grey", "longdash") %>%
    draw_segment(vertex4, vertex5, "grey", "longdash") %>%
    draw_segment(vertex4, point, "grey", "longdash") %>%
    draw_segment(vertex5, vertex6, "grey", "longdash") %>%
    draw_segment(vertex6, point, "grey", "longdash")
}


# 两个向量所张成的平行四边形
draw_cross_mesh <- function(p, u, v, color = "lightgrey") {
  mesh <- rbind(data.table(x = 0, y = 0, z = 0), u, v, vector_add(u, v))
  p %>%
    add_mesh(
      data = mesh,
      facecolor = rep(color, nrow(mesh)), # 不能比三角面数少
      x = ~x, y = ~y, z = ~z
    ) %>%
    draw_segment(u, vector_add(u, v), color = "grey") %>%
    draw_segment(v, vector_add(u, v), color = "grey")
}