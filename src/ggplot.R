## plot2D.R

# 建立简洁风格的画布（坐标系）
create_canvas <- function() {
  data.table(x = 0, y = 0) %>%
    ggplot(mapping = aes(x, y)) +
    geom_point(size = 4, shape = 4, alpha = 0.5) +
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    theme_light() +
    theme(
      panel.border = element_rect(color = "black"),
      axis.title = element_blank()
    )
}
create_canvas()

# 散点图
draw_scatter <- function(p, points, color = "blue") {
  p + geom_point(data = points, size = 3, colour = color)
}

# 两点间添加一条连线
draw_segment <- function(p, x1, y1, x2, y2, color = "blue") {
  p + geom_segment(x = x1, y = y1, xend = x2, yend = y2, colour = color)
}

# 绘制多边形
draw_polygon <- function(p, points, color = "blue") {
  p <- p + ggplot2::geom_point(data = points, alpha = 0)
  n <- nrow(points)
  xs <- points$x
  ys <- points$y
  for (i in 1:n) {
    if (i < n) {
      p <- draw_segment(p, xs[i], ys[i], xs[i + 1], ys[i + 1], color)
    } else {
      p <- draw_segment(p, xs[n], ys[n], xs[1], ys[1], color)
    }
  }
  return(p)
}