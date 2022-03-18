## chapter03.R

library(tidyverse)
library(magrittr)
library(data.table)
library(plotly)
library(htmlwidgets)


## import functions
source("./src/plot3D.R")
source("./src/calculate3D.R")
source("./src/transform3D.R")


# initiate canvas
range <- 5
canvas <- create_canvas(range)
canvas


# data
origin <- data.table(x = 0, y = 0, z = 0)
point1 <- data.table(x = 2, y = 1, z = 1)
point2 <- data.table(x = 1, y = -2, z = -2)
point3 <- vector_add(point1, point2)


# plotting
p <- canvas %>%
  draw_point(point1) %>%
  draw_point(point2) %>%
  draw_arrow(end = point1) %>%
  draw_arrow(end = point2) %>%
  draw_segment(point1, point2) %>%
  draw_box(point1) %>%
  draw_box(point2)
p
saveWidget(p, "./img/plot-3D.html", selfcontained = F, libdir = "lib")

p <- canvas %>%
  draw_arrow(end = point1, color = "blue") %>%
  draw_arrow(begin = point1, end = point3, linetype = "longdash") %>%
  draw_arrow(end = point2) %>%
  draw_arrow(begin = point2, end = point3, color = "blue", linetype = "longdash") %>%
  draw_arrow(end = point3, color = "purple")
p
saveWidget(p, "./img/add-vector.html", selfcontained = F, libdir = "lib")


# Figure: 3-23
t <- 0:24
vector_chain <- data.table(
  x = sin(t * pi / 6),
  y = cos(t * pi / 6),
  z = rep(1 / 3, 25)
)
vector_sum(vector_chain[1:24, ])

p <- create_canvas(10)
begin <- origin
end <- vector_chain[1, ]
for (i in 1:24) {
  p <- p %>% draw_arrow(begin = begin, end = end)
  begin <- end
  end <- vector_add(end, vector_chain[i + 1, ])
}
p
saveWidget(p, "./img/vector-chain.html", selfcontained = F, libdir = "lib")


# cross product
u <- data.table(x = 2, y = 1, z = 0)
v <- data.table(x = -2, y = 3, z = 2)
w <- cross_product(u, v)

p <- create_canvas(10) %>%
  draw_point(u, color = "orangered") %>%
  draw_annotation("u", u, color = "orangered") %>%
  draw_arrow(end = u, color = "orangered") %>%
  draw_point(v, color = "blue") %>%
  draw_annotation("v", v, color = "blue") %>%
  draw_arrow(end = v, color = "blue") %>%
  draw_cross_mesh(u, v) %>%
  draw_point(w, color = "purple") %>%
  draw_annotation("u \u00D7 v", w, color = "purple") %>%
  draw_arrow(end = w, color = "purple")
p
saveWidget(p, "./img/cross-product.html", selfcontained = F, libdir = "lib")


# Rendering 3D object in 2D

# 正八面体

vertex_A <- data.table(x = 5, y = 0, z = 0)
vertex_B <- data.table(x = 0, y = 5, z = 0)
vertex_C <- data.table(x = 0, y = 0, z = 5)
vertex_D <- data.table(x = -5, y = 0, z = 0)
vertex_E <- data.table(x = 0, y = -5, z = 0)
vertex_F <- data.table(x = 0, y = 0, z = -5)

octahedron_vertex <- rbind(vertex_A, vertex_B, vertex_C, vertex_D, vertex_E, vertex_F)

create_canvas(5) %>%
  draw_point(octahedron_vertex) %>%
  draw_annotation("A", vertex_A) %>%
  draw_annotation("B", vertex_B) %>%
  draw_annotation("C", vertex_C) %>%
  draw_annotation("D", vertex_D) %>%
  draw_annotation("E", vertex_E) %>%
  draw_annotation("F", vertex_F)

# 各面中点（向量）的排列方式，要保证(v2-v1)×(v3-v1)指向八面体的外部，后面用以判断亮度
octahedron_face <- list(
  rbind(vertex_A, vertex_B, vertex_C),
  rbind(vertex_A, vertex_F, vertex_B),
  rbind(vertex_A, vertex_C, vertex_E),
  rbind(vertex_A, vertex_E, vertex_F),
  rbind(vertex_D, vertex_C, vertex_B),
  rbind(vertex_D, vertex_B, vertex_F),
  rbind(vertex_D, vertex_E, vertex_C),
  rbind(vertex_D, vertex_F, vertex_E)
)

create_canvas(8) %>%
  draw_point(octahedron_vertex) %>%
  draw_annotation("v1", vertex_C) %>%
  draw_annotation("v2", vertex_E) %>%
  draw_annotation("v3", vertex_A) %>%
  draw_arrow(begin = vertex_C, end = vertex_E) %>%
  draw_arrow(begin = vertex_C, end = vertex_A) %>%
  draw_annotation(
    "v2-v1",
    data.table(
      x = (vertex_C$x + vertex_E$x) / 2,
      y = (vertex_C$y + vertex_E$y) / 2,
      z = (vertex_C$z + vertex_E$z) / 2
    ),
    color = "orangered"
  ) %>%
  draw_annotation(
    "v3-v1",
    data.table(
      x = (vertex_C$x + vertex_A$x) / 2,
      y = (vertex_C$y + vertex_A$y) / 2,
      z = (vertex_C$z + vertex_A$z) / 2
    ),
    color = "orangered"
  )






projection <- octahedron_face %>% project_polytope()
