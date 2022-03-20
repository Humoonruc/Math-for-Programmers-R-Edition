## chapter03.R

library(tidyverse)
library(magrittr)
library(data.table)
library(plotly)
library(htmlwidgets)


## import functions
source("./src/plot2D-plotly.R")
source("./src/plot3D.R")
source("./src/calculate2D.R")
source("./src/calculate3D.R")
source("./src/transform2D.R")
source("./src/transform3D.R")


## initiate canvas
range <- 5
canvas <- create_canvas_3d(range)
canvas


## data
point1 <- data.table(x = 2, y = 1, z = 1)
point2 <- data.table(x = 1, y = -2, z = -2)
point3 <- vector_add(point1, point2)


## plotting 3d vectors
p <- canvas %>%
  draw_point(point1) %>%
  draw_point(point2) %>%
  draw_arrow_3d(begin = origin_3d, end = point1) %>%
  draw_arrow_3d(begin = origin_3d, end = point2) %>%
  draw_line(rbind(point1, point2)) %>%
  draw_box_3d(point1) %>%
  draw_box_3d(point2)
p
saveWidget(p, "./img/3d-vectors.html", selfcontained = F, libdir = "lib")


p <- canvas %>%
  draw_arrow_3d(origin_3d, point1, "blue") %>%
  draw_arrow_3d(point1, point3, linetype = "longdash") %>%
  draw_arrow_3d(origin_3d, point2) %>%
  draw_arrow_3d(point2, point3, "blue", linetype = "longdash") %>%
  draw_arrow_3d(origin_3d, point3, "purple")
p
saveWidget(p, "./img/3d-vector-add.html", selfcontained = F, libdir = "lib")


# vector chain
t <- 0:24
vector_chain <- data.table(
  x = sin(t * pi / 6),
  y = cos(t * pi / 6),
  z = rep(1 / 3, 25)
)
vector_sum(vector_chain[1:24, ])

p <- create_canvas_3d(9)
begin <- origin_3d
end <- vector_chain[1, ]
for (i in 1:24) {
  p <- p %>% draw_arrow_3d(begin, end, "orangered")
  begin <- end
  end <- vector_add(end, vector_chain[i + 1, ])
}
p
saveWidget(p, "./img/3d-vector-chain.html", selfcontained = F, libdir = "lib")


## cross product
u <- data.table(x = 2, y = 1, z = 0)
v <- data.table(x = -2, y = 3, z = 2)
w <- cross_product(u, v)

p <- create_canvas_3d(10) %>%
  ## draw_point(u, color = "orangered") %>%
  draw_text(u, "<b>u</b>", color = "orangered") %>%
  draw_arrow_3d(origin_3d, u, "orangered", width = 4) %>%
  ## draw_point(v, color = "blue") %>%
  draw_text(v, "<b>v</b>", color = "blue") %>%
  draw_arrow_3d(origin_3d, v, "blue", width = 4) %>%
  draw_cross_mesh(u, v) %>%
  ## draw_point(w, color = "purple") %>%
  draw_text(w, "<b>u \u00D7 v</b>", "purple") %>%
  draw_arrow_3d(origin_3d, w, "purple", width = 4)
p
saveWidget(p, "./img/3d-cross-product.html", selfcontained = F, libdir = "lib")


## Rendering 3D object in 2D

# 正八面体

# 顶点、边和面
vertex_A <- data.table(x = 5, y = 0, z = 0)
vertex_B <- data.table(x = 0, y = 5, z = 0)
vertex_C <- data.table(x = 0, y = 0, z = 5)
vertex_D <- data.table(x = -5, y = 0, z = 0)
vertex_E <- data.table(x = 0, y = -5, z = 0)
vertex_F <- data.table(x = 0, y = 0, z = -5)

octahedron_vertex <- rbind(
  vertex_A, vertex_B, vertex_C,
  vertex_D, vertex_E, vertex_F
)

octahedron_edge <- list(
  rbind(vertex_C, vertex_A),
  rbind(vertex_C, vertex_B),
  rbind(vertex_C, vertex_D),
  rbind(vertex_C, vertex_E),
  rbind(vertex_F, vertex_A),
  rbind(vertex_F, vertex_B),
  rbind(vertex_F, vertex_D),
  rbind(vertex_F, vertex_E),
  rbind(vertex_A, vertex_B),
  rbind(vertex_B, vertex_D),
  rbind(vertex_D, vertex_E),
  rbind(vertex_E, vertex_A)
)

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


# 透视图
p <- create_canvas_3d(7) %>%
  draw_point(octahedron_vertex) %>%
  draw_text(vertex_A, "A") %>%
  draw_text(vertex_B, "B") %>%
  draw_text(vertex_C, "C", position = "top right") %>%
  draw_text(vertex_D, "D") %>%
  draw_text(vertex_E, "E") %>%
  draw_text(vertex_F, "F", position = "bottom right") %>%
  reduce(octahedron_edge, draw_line, .init = .) %>%
  reduce(octahedron_face, draw_face_3d, .init = .)
p
saveWidget(p, "./img/3d-octahedron1.html", selfcontained = F, libdir = "lib")


# 非透视图
p <- create_canvas_3d(7) %>%
  reduce(octahedron_face, function(plot, face) {
    plot %>% draw_face_3d(face, opacity = 1, linetype = "solid")
  }, .init = .)
p
saveWidget(p, "./img/3d-octahedron2.html", selfcontained = F, libdir = "lib")


# 各面中三个 vertex 排列的顺序：
# 若以v1,v2,v3分别代表原点到三个顶点的向量
# 要保证(v2-v1)×(v3-v1)指向八面体的外部
# 这个法向量后面要用来判断亮度

face <- octahedron_face[[3]]
centroid <- summarise(face, x = mean(x), y = mean(y), z = mean(z))
unit_normal <- face_unit_normal(face)
unit_normal_tip <- vector_add(centroid, unit_normal)

p <- create_canvas_3d(7) %>%
  draw_point(origin_3d, color = "blue") %>%
  draw_text(vertex_A, "A", "grey", "top right") %>%
  draw_text(vertex_C, "C", "grey", "top right") %>%
  draw_text(vertex_E, "E", "grey", "top right") %>%
  draw_arrow_3d(origin_3d, vertex_A, "blue") %>%
  draw_arrow_3d(origin_3d, vertex_C, "blue") %>%
  draw_arrow_3d(origin_3d, vertex_E, "blue") %>%
  draw_text(
    vector_midpoiot(origin_3d, vertex_C),
    "<b>v1</b>", "blue", "middle center"
  ) %>%
  draw_text(
    vector_midpoiot(origin_3d, vertex_E),
    "<b>v2</b>", "blue", "middle center"
  ) %>%
  draw_text(
    vector_midpoiot(origin_3d, vertex_A),
    "<b>v3</b>", "blue", "middle center"
  ) %>%
  draw_arrow_3d(vertex_C, vertex_E, "red") %>%
  draw_arrow_3d(vertex_C, vertex_A, "red") %>%
  draw_text(
    vector_midpoiot(vertex_C, vertex_E),
    "<b>v2</b>-<b>v1</b>", "red", "middle center"
  ) %>%
  draw_text(
    vector_midpoiot(vertex_C, vertex_A),
    "<b>v3</b>-<b>v1</b>", "red", "middle center"
  ) %>%
  draw_face_3d(face, opacity = 0.5) %>%
  draw_arrow_3d(centroid, unit_normal_tip, "purple") %>%
  draw_text(
    unit_normal_tip,
    "cross product", "purple", "top center"
  ) %>%
  layout(
    title = list(
      text = "unit normal vector",
      font = list(color = "purple")
    )
  )
p
saveWidget(p, "./img/3d-unit-normal.html", selfcontained = F, libdir = "lib")


# 为不同的面涂不同的颜色，法向量与光源夹角越小，颜色越亮

# 光源向量为(1,1,0)，只能看见两个面
## lightsource <- data.table(x = 1, y = 1, z = 0)
## base1 <- data.table(x = -1 / sqrt(2), y = 1 / sqrt(2), z = 0)
## base2 <- data.table(x = 0, y = 0, z = 1)

# 光源向量为(0,0,1)，即z轴，能看见四个面
## lightsource <- data.table(x = 0, y = 0, z = 1)
## base1 <- data.table(x = 1, y = 0, z = 0)
## base2 <- data.table(x = 0, y = 1, z = 0)

# 任意光源
base1 <- data.table(x = 8 / 9, y = -4 / 9, z = 1 / 9)
base2 <- data.table(x = 1 / sqrt(26), y = 3 / sqrt(26), z = 4 / sqrt(26))
lightsource <- cross_product(base1, base2)


visible_faces <- octahedron_face %>%
  bright_faces(lightsource)

# 用角度表示亮度，范围为[0, pi/2)
face_brightness <- visible_faces %>%
  Map(face_unit_normal, .) %>%
  Map(function(normal) {
    angle_between(normal, lightsource)
  }, .)

faces_2d <- project_polytope(visible_faces, base1, base2)


p <-
  create_canvas_2d() %>%
  reduce2(
    faces_2d, face_brightness,
    function(plot, face, brightness) {
      plot %>%
        draw_polygon(
          face,
          # gray()将[1,0]映射到白色至黑色的灰色带上
          # 将brightness映射到[1,0]后，再适当放缩
          # 距离极端的黑白两色远一点
          fill = (((1 - brightness * 2 / pi) - 0.5) / 2 + 0.5) %>% gray()
        )
    },
    .init = .
  ) %>%
  layout(
    title = list(text = plotly::TeX("\\text{lightsource vector}=(-0.4140, -0.6755, 0.6101)"))
  ) %>%
  config(mathjax = "cdn")
p
# 有一个面非常亮，近乎垂直于光源向量
saveWidget(p, "./img/3d-project-to-2d.html", selfcontained = F, libdir = "lib")
