### Chapter02-plotly.R

library(tidyverse)
library(magrittr)
library(data.table)
library(plotly)
library(htmlwidgets)

# import source
source("./src/plot2D-plotly.R")
source("./src/calculate2D.R")
source("./src/transform2D.R")

# data
dinosaur <- fread("data/data.csv")


###############################################################
## plotting vectors
###############################################################

# plot and save
p <- create_canvas_2d() %>%
  draw_points(dinosaur) %>%
  draw_polygon(dinosaur)
p
saveWidget(p, "./img/dinosaur.html", selfcontained = F, libdir = "lib")


# vector translation
dinosaur2 <- dinosaur %>%
  translate(data.table(x = -1.5, y = -2.5))

p <- create_canvas_2d() %>%
  draw_points(dinosaur) %>%
  draw_polygon(dinosaur) %>%
  draw_points(dinosaur2, color = "red") %>%
  draw_polygon(dinosaur2, color = "red")
p
saveWidget(p, "./img/dinosaur2.html", selfcontained = F, libdir = "lib")


# 81 只小恐龙
p <- create_canvas_2d()
for (dx in -4:4 * 12) {
  for (dy in -4:4 * 10) {
    translation <- data.table(x = dx, y = dy)
    little_dinosaur <- dinosaur %>% translate(translation)
    p <- p %>%
      draw_polygon(little_dinosaur)
  }
}
p
saveWidget(p, "./img/multi-dinosaur.html", selfcontained = F, libdir = "lib")


###############################################################
## vector arithmetic
###############################################################

# operation
scale(dinosaur, 5)
length <- get_length(dinosaur)
dinosaur[which(length == max(length)), ] # 距离原点最远的点
dinosaur[which(length == min(length)), ] # 距离原点最仅的点
get_perimeter(dinosaur) # 周长


# r·z + s·v 的范围
z <- data.table(x = -1, y = 1)
v <- data.table(x = 1, y = 1)
n <- 2000 # 样本容量
random_r <- runif(n, -3, 3)
random_s <- runif(n, -1, 1)

possible_space <- map2_dfr(random_r, random_s, function(x, y) {
  vector_add(scale(z, x), scale(v, y))
})

p <- create_canvas_2d() %>%
  draw_points(possible_space, color = "black")
p
saveWidget(p, "./img/possible-space.html", selfcontained = F, libdir = "lib")


###############################################################
## trigonometry and polar coordinate system
###############################################################

# flower
i <- 0:1000
points_polor <- data.table(r = cos(i * pi / 100), theta = 2 * pi * i / 1000)
points <- points_polor %>% to_cartesian()

p <- create_canvas_2d() %>%
  draw_polygon(points, color = "blue")
p
saveWidget(p, "./img/flower.html", selfcontained = F, libdir = "lib")


# rotate dinosaur
dinosaur_rotated <- rotate(dinosaur, pi / 4)

p <- create_canvas_2d() %>%
  draw_points(dinosaur) %>%
  draw_polygon(dinosaur) %>%
  draw_points(dinosaur_rotated, color = "red") %>%
  draw_polygon(dinosaur_rotated, color = "red")
p
saveWidget(p, "./img/dinosaur_rotated.html", selfcontained = F, libdir = "lib")


###############################################################
## linear transforming
###############################################################

# 生成正n边形
regular_polygon <- function(n) {
  l_points <- list(data.table(x = 1, y = 0))
  for (i in 1:(n - 1)) {
    l_points[[i + 1]] <- l_points[[1]] %>% rotate(2 * pi * i / n)
  }
  l_points %>%
    reduce(.f = \(p1, p2) rbind(p1, p2)) # 匿名函数
}

n <- 7

p <- create_canvas_2d() %>%
  draw_points(regular_polygon(n)) %>%
  draw_polygon(regular_polygon(n))
p
saveWidget(p, "./img/regular-polygon.html", selfcontained = F, libdir = "lib")