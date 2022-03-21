## plot-element.R


# 1 初始化画布
# 2 点和线
# 3 箭头和标注
# 4 面


#############################################################
# 1 初始化画布
#############################################################

# 建立2d画布（坐标系）
axis_template <- list(
  zeroline = TRUE,
  ticks = "outside",
  gridcolor = "white",
  gridwidth = 1.5,
  nticks = 20
)
create_canvas_2d <- function(title_x = "", title_y = "", showlegend = FALSE) {
  plot_ly(
    data = data.table(x = 0, y = 0),
    x = ~x, y = ~y,
    showlegend = showlegend,
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

# 建立3d画布（坐标系）
origin_3d <- data.table(x = 0, y = 0, z = 0) # 3d 原点
create_canvas_3d <- function(range,
                             title_x = "<b>x</b>",
                             title_y = "<b>y</b>",
                             title_z = "<b>z</b>") {
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
    data = origin_3d,
    x = ~x, y = ~y, z = ~z,
    showlegend = F
  ) %>%
    add_lines(data = axis_x, line = list(color = "black")) %>%
    add_lines(data = axis_y, line = list(color = "black")) %>%
    add_lines(data = axis_z, line = list(color = "black")) %>%
    add_trace(
      type = "cone",
      x = range, y = 0, z = 0, # 箭头坐标
      u = 1, v = 0, w = 0, # 箭头方向向量
      sizemode = "absolute", sizeref = 0.5,
      anchor = "tip", # 规定箭头坐标指顶点坐标
      colorscale = list(list(0, "black"), list(1, "black")), # 箭头单一颜色
      showscale = FALSE # 不显示颜色图例
    ) %>%
    add_trace(
      type = "cone",
      x = 0, y = range, z = 0,
      u = 0, v = 1, w = 0,
      sizemode = "absolute", sizeref = 0.5,
      anchor = "tip",
      colorscale = list(list(0, "black"), list(1, "black")), # 箭头单一颜色
      showscale = FALSE
    ) %>%
    add_trace(
      type = "cone",
      x = 0, y = 0, z = range,
      u = 0, v = 0, w = 1,
      sizemode = "absolute", sizeref = 0.5,
      anchor = "tip",
      colorscale = list(list(0, "black"), list(1, "black")), # 箭头单一颜色
      showscale = FALSE
    ) %>%
    add_text(
      x = range + 1.5, y = 0, z = -0.5,
      text = title_x, textposition = "top center"
    ) %>%
    add_text(
      x = 0, y = range + 1.5, z = -0.5,
      text = title_y, textposition = "top center"
    ) %>%
    add_text(
      x = 0, y = 0, z = range + 1,
      text = title_z, textposition = "top center"
    ) %>%
    layout(
      title = list(
        font = list(size = 28),
        y = 0, yanchor = "top", yref = "paper",
        pad = list(t = 35)
      ),
      margin = list(b = 80, l = 30, r = 20),
      scene = list(
        # annotations 定义注释及箭头的样式
        # x,y,z 定义注释箭头指向的点的位置
        # ax, ay 定义注释文本（箭尾）的位置
        annotations = list(

          ## list(
          ##   showarrow = F,
          ##   x = "2017-01-01",
          ##   y = "A",
          ##   z = 0,
          ##   text = "Point 1",
          ##   xanchor = "left",
          ##   xshift = 10,
          ##   opacity = 0.7
          ## ),
          ## list(
          ##   x = "2017-02-10",
          ##   y = "B",
          ##   z = 4,
          ##   text = "Point 2",
          ##   textangle = 0,
          ##   ax = 0,
          ##   ay = -75,
          ##   font = list(
          ##     color = "black",
          ##     size = 12
          ##   ),
          ##   arrowcolor = "black",
          ##   arrowsize = 3,
          ##   arrowwidth = 1,
          ##   arrowhead = 1
          ## ),
          ## list(
          ##   x = "2017-03-20",
          ##   y = "C",
          ##   z = 5,
          ##   ax = 50,
          ##   ay = 0,
          ##   text = "Point 3",
          ##   arrowhead = 1,
          ##   xanchor = "left",
          ##   yanchor = "bottom"
          ## )
        ),
        # 定义三个轴比例尺的比例
        aspectratio = list(
          x = 1,
          y = 1,
          z = 1
        ),
        xaxis = list(title = "", nticks = 10),
        yaxis = list(title = "", nticks = 10),
        zaxis = list(title = "", nticks = 10),
        camera = list(
          center = list(
            x = 0,
            y = 0,
            z = 0
          ),
          eye = list(
            x = 1.96903462608,
            y = -1.09022831971,
            z = 0.405345349304
          ),
          up = list(
            x = 0,
            y = 0,
            z = 1
          ),
          projection = list(
            type = "perspective" # 透视，另一个值是 orthographic
          )
        )
      ),
      legend = list(
        y = 0.5
      )
    )
}


#############################################################
# 2 点和线
#############################################################

# 画点
draw_point <- function(p, points, color = "blue", name = "") {
  p %>% add_markers(
    data = points,
    name = name,
    marker = list(size = 4, color = color)
  )
}

# 画线段
draw_line <- function(p, points,
                      color = "royalblue",
                      linetype = "solid",
                      width = 1.5, name = "", showlegend = FALSE) {
  p %>% add_lines(
    data = points,
    name = name,
    showlegend = showlegend,
    line = list(
      color = color,
      width = width,
      dash = linetype
    )
  )
}

# 画3d盒子
draw_box_3d <- function(p, point) {
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
    draw_line(rbind(vertex1, vertex2), "grey", "longdash") %>%
    draw_line(rbind(vertex2, vertex3), "grey", "longdash") %>%
    draw_line(rbind(vertex1, vertex4), "grey", "longdash") %>%
    draw_line(rbind(vertex2, point), "grey", "longdash") %>%
    draw_line(rbind(vertex3, vertex6), "grey", "longdash") %>%
    draw_line(rbind(vertex4, vertex5), "grey", "longdash") %>%
    draw_line(rbind(vertex4, point), "grey", "longdash") %>%
    draw_line(rbind(vertex5, vertex6), "grey", "longdash") %>%
    draw_line(rbind(vertex6, point), "grey", "longdash")
}


#############################################################
# 3 箭头和标注
#############################################################

# 画箭头及注释 annotation(适用于2d图)
draw_arrow_2d <- function(p, begin, end,
                          color = "dimgrey",
                          linetype = "solid",
                          width = 2, name = "", showlegend = FALSE) {
  p %>%
    draw_line(rbind(begin, end),
      color = color, linetype = linetype, width = width, name = name,
      showlegend = showlegend
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

# 画箭头（适用于3d图）
draw_arrow_3d <- function(p, begin, end,
                          color = "dimgrey",
                          linetype = "solid",
                          width = 2, showlegend = FALSE) {
  p %>%
    draw_line(rbind(begin, end),
      color = color, linetype = linetype, width = width, showlegend = showlegend
    ) %>%
    add_trace(
      type = "cone",
      x = end$x, y = end$y, z = end$z, # 箭头坐标
      u = end$x - begin$x,
      v = end$y - begin$y,
      w = end$z - begin$z, # 箭杆在三个坐标轴上的投影长度
      sizemode = "absolute", # 三个投影只规定箭头的朝向，与箭头大小无关
      # 否则按默认值 sizemode='scaled'，上述投影大小即决定箭头的大小
      sizeref = 1,
      anchor = "tip", # 规定箭头坐标指顶点坐标
      colorscale = list(list(0, color), list(1, color)), # 箭头单一颜色
      showscale = FALSE # 不显示颜色图例
    )
}

# 在数据点上标注文本(text)
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


#############################################################
# 4 面
#############################################################

# 绘制多边形，主要作用是填充色块
# 可通过fillcolor属性定义面的颜色
draw_polygon <- function(p, points, fill = "grey", color = "black", opacity = 1) {
  p %>%
    add_polygons(
      # 该函数不会自动补画第一个端点和最后一个端点的连线
      data = points,
      fillcolor = fill,
      line = list(color = color, width = 1),
      opacity = opacity
    ) %>%
    add_lines(
      data = points[c(1, .N), ],
      line = list(color = color, width = 1),
      opacity = opacity
    )
}

# 画3d表面
# 核心是 add_mesh()
draw_face_3d <- function(p, points,
                         facecolor = "lightgrey", opacity = 0.5,
                         linecolor = "grey", linetype = "dash",
                         width = 1.5) {
  p %>%
    add_mesh(
      data = points,
      facecolor = facecolor,
      ## lightposition = list(x = 1, y = -10, z = 0),
      opacity = opacity
    ) %>%
    add_lines(
      data = points,
      line = list(
        color = "grey",
        width = width,
        dash = linetype
      )
    )
}

# 两个向量所张成的平行四边形
draw_cross_mesh <- function(p, u, v, color = "lightgrey") {
  mesh <- rbind(origin_3d, u, v, vector_add(u, v))
  p %>%
    add_mesh(
      data = mesh,
      facecolor = rep(color, nrow(mesh)), # 不能比三角面数少
    ) %>%
    draw_line(rbind(u, vector_add(u, v)), color = "grey", showlegend = FALSE) %>%
    draw_line(rbind(v, vector_add(u, v)), color = "grey", showlegend = FALSE)
}