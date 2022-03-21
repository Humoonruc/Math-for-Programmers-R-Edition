### Chapter02-plotly.R

library(tidyverse)
library(magrittr)
library(data.table)
library(plotly)
library(htmlwidgets)


# import source
source("./src/plot-element.R")
source("./src/vector-calculate.R")
source("./src/linear-transform.R")


# data
dinosaur <- fread("data/data.csv")

###############################################################
## plotting vectors
###############################################################

# plot and save
p <- create_canvas_2d() %>%
  draw_point(dinosaur) %>%
  draw_polygon(dinosaur, color = "blue", opacity = 0.3) %>%
  layout(
    title = list(text = "A dinasaur: scatters and lines")
  )
p
saveWidget(p, "./img/dinosaur.html", selfcontained = F, libdir = "lib")


# translation
dinosaur2 <- dinosaur %>%
  translate(data.table(x = -1.5, y = -2.5))

p <- create_canvas_2d() %>%
  draw_point(dinosaur) %>%
  draw_polygon(dinosaur, color = "blue", opacity = 0.3) %>%
  draw_point(dinosaur2, color = "red") %>%
  draw_polygon(dinosaur2, color = "red", opacity = 0.3) %>%
  reduce(
    .x = 1:nrow(dinosaur),
    .f = function(p, i) {
      p %>%
        draw_arrow_2d(dinosaur[i, ], dinosaur2[i, ], linetype = "dash")
    },
    .init = .
  ) %>%
  layout(
    title = list(text = "2 dinasaurs: translation")
  )
p
saveWidget(p, "./img/dinosaur-translation.html", selfcontained = F, libdir = "lib")


# 81 只小恐龙
p <- create_canvas_2d() %>%
  layout(
    title = list(text = "many dinasaurs: translation")
  )
for (dx in -4:4 * 12) {
  for (dy in -4:4 * 10) {
    translation <- data.table(x = dx, y = dy)
    little_dinosaur <- dinosaur %>% translate(translation)
    p <- p %>%
      draw_polygon(little_dinosaur, color = "blue", opacity = 0.5)
  }
}
p
saveWidget(p, "./img/dinosaur-multi.html", selfcontained = F, libdir = "lib")


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
random_s <- runif(n, -2, 2)
z_max <- scale(z, 3)
z_min <- scale(z, -3)
v_max <- scale(v, 2)
v_min <- scale(v, -2)

possible_space <- map2_dfr(
  .x = random_r, .y = random_s,
  .f = \(x, y) {
    vector_add(scale(z, x), scale(v, y))
  }
)

p <- create_canvas_2d() %>%
  draw_point(possible_space, color = "grey") %>%
  draw_line(rbind(z_max, z_min), color = "tomato", linetype = "dash", showlegend = FALSE) %>%
  draw_arrow_2d(data.table(x = 0, y = 0), z, color = "red") %>%
  draw_text(z, text = plotly::TeX("\\boldsymbol{z}=(-1,1)"), color = "red") %>%
  draw_line(rbind(v_max, v_min), color = "royalblue", linetype = "dash", showlegend = FALSE) %>%
  draw_arrow_2d(data.table(x = 0, y = 0), v, color = "blue") %>%
  draw_text(v, text = plotly::TeX("\\boldsymbol{v}=(1,1)"), color = "blue") %>%
  layout(
    title = list(text = plotly::TeX("r\\boldsymbol{z} + s\\boldsymbol{v}, r\\in (-3,3), s\\in (-2,2)"))
  ) %>%
  config(mathjax = "cdn")
p
saveWidget(p, "./img/possible-space.html", selfcontained = F, libdir = "lib")


###############################################################
## trigonometry and polar coordinate system
###############################################################

# flower
i <- 0:1000
points_polor <- data.table(
  r = cos(i * pi / 100),
  theta = 2 * pi * i / 1000
)
points <- points_polor %>% to_cartesian_2d()

p <- create_canvas_2d() %>%
  draw_polygon(points, color = "blue", fill = "yellow", opacity = 0.5) %>%
  layout(
    title = list(text = "flower consists 1,000 points")
  )
p
saveWidget(p, "./img/flower.html", selfcontained = F, libdir = "lib")


# rotate dinosaur
dinosaur_rotated <- rotate_2d(dinosaur, pi / 4)

p <- create_canvas_2d() %>%
  draw_point(dinosaur) %>%
  draw_polygon(dinosaur, color = "blue", opacity = 0.3) %>%
  draw_point(dinosaur_rotated, color = "red") %>%
  draw_polygon(dinosaur_rotated, color = "red", opacity = 0.3) %>%
  layout(
    title = list(text = "dinosaur: 2D rotation")
  )
p
saveWidget(p, "./img/dinosaur-rotation.html", selfcontained = F, libdir = "lib")


# 生成正n边形
regular_polygon <- function(n) {
  0:(n - 1) %>%
    map(.f = \(i) {
      data.table(x = 1, y = 0) %>%
        rotate_2d(2 * pi * i / n)
    }) %>%
    reduce(.f = rbind)
}
heptagon <- regular_polygon(7)
p <- create_canvas_2d() %>%
  draw_point(heptagon) %>%
  draw_polygon(heptagon, color = "blue", opacity = 0.5) %>%
  layout(
    title = list(text = "heptagon: 2D rotation")
  )
p
saveWidget(p, "./img/regular-polygon.html", selfcontained = F, libdir = "lib")
