## chapter03.R


# import modules
source("./config/config.R")
source("./base/transformation.R")
source("./base/plotly.R")


# export path
export_path <- "./export/"


## initiate canvas
# canvas_2d <- create_canvas_2d()
range <- 5
canvas <- create_canvas_3d(range)
canvas
origin_3d <- c(x = 0, y = 0, z = 0) # 3d 原点


## data
# point1 <- data.table(x = 2, y = 1, z = 1)
# point2 <- data.table(x = 1, y = -2, z = -2)
# point3 <- vector_add(point1, point2)
point1 <- c(x = 2, y = 1, z = 1)
point2 <- c(x = 1, y = -2, z = -2)
point3 <- point1 + point2

## plotting 3d vectors
p <- canvas %>%
  draw_points(point1) %>%
  draw_points(point2) %>%
  draw_arrow_3d(begin = origin_3d, end = point1) %>%
  draw_arrow_3d(begin = origin_3d, end = point2) %>%
  draw_lines(cbind(point1, point2)) %>%
  draw_box_3d(point1) %>%
  draw_box_3d(point2)
p
p %>%
  saveWidget(
    str_c(export_path, "3d-vectors.html"),
    selfcontained = F, libdir = "lib"
  )


p <- canvas %>%
  draw_arrow_3d(origin_3d, point1, "blue") %>%
  draw_arrow_3d(point1, point3, linetype = "longdash") %>%
  draw_arrow_3d(origin_3d, point2) %>%
  draw_arrow_3d(point2, point3, "blue", linetype = "longdash") %>%
  draw_arrow_3d(origin_3d, point3, "purple")
p
p %>%
  saveWidget(
    str_c(export_path, "3d-vector-add.html"),
    selfcontained = F, libdir = "lib"
  )


# vector chain
t <- 0:24
vector_chain <- rbind(
  x = sin(t * pi / 6),
  y = cos(t * pi / 6),
  z = rep(1 / 3, 25)
)

p <- create_canvas_3d(9)
begin <- origin_3d
end <- vector_chain[, 1]
for (i in 1:24) {
  p <- p %>% draw_arrow_3d(begin, end, "orangered")
  begin <- end
  end <- end + vector_chain[, i + 1]
}
p
p %>%
  saveWidget(
    str_c(export_path, "3d-vector-chain.html"),
    selfcontained = F, libdir = "lib"
  )


## cross product
u <- c(2, 1, 0)
v <- c(-2, 3, 2)
w <- cross_product(u, v)

p <- create_canvas_3d(10) %>%
  ## draw_points(u, color = "orangered") %>%
  draw_text(u, "<b>u</b>", color = "orangered") %>%
  draw_arrow_3d(origin_3d, u, "orangered", width = 4) %>%
  ## draw_points(v, color = "blue") %>%
  draw_text(v, "<b>v</b>", color = "blue") %>%
  draw_arrow_3d(origin_3d, v, "blue", width = 4) %>%
  draw_cross_mesh(u, v) %>%
  ## draw_points(w, color = "purple") %>%
  draw_text(w, "<b>u \u00D7 v</b>", "purple") %>%
  draw_arrow_3d(origin_3d, w, "purple", width = 4)
p
p %>%
  saveWidget(
    str_c(export_path, "3d-cross-product.html"),
    selfcontained = F, libdir = "lib"
  )



# 以下部分待重构
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
  draw_points(octahedron_vertex) %>%
  draw_text(vertex_A, "A") %>%
  draw_text(vertex_B, "B") %>%
  draw_text(vertex_C, "C", position = "top right") %>%
  draw_text(vertex_D, "D") %>%
  draw_text(vertex_E, "E") %>%
  draw_text(vertex_F, "F", position = "bottom right") %>%
  reduce(octahedron_face, draw_face_3d, .init = .)
p
p %>%
  saveWidget(
    str_c(export_path, "3d-octahedron1.html"),
    selfcontained = F, libdir = "lib"
  )


# 非透视图
p <- create_canvas_3d(7) %>%
  reduce(octahedron_face, function(plot, face) {
    plot %>% draw_face_3d(face, opacity = 1, linetype = "solid")
  }, .init = .)
p
p %>%
  saveWidget(
    str_c(export_path, "3d-octahedron2.html"),
    selfcontained = F, libdir = "lib"
  )


# 各面中三个 vertex 排列的顺序：
# 若以v1,v2,v3分别代表原点到三个顶点的向量
# 要保证(v2-v1)×(v3-v1)指向八面体的外部
# 这个法向量后面要用来判断亮度
face <- octahedron_face[[3]] # 以第三个面为例
centroid <- summarise(face, x = mean(x), y = mean(y), z = mean(z))
unit_normal <- face_unit_normal(face)
unit_normal_tip <- vector_add(centroid, unit_normal) # 法向量箭头的顶端

p <- create_canvas_3d(7) %>%
  draw_points(origin_3d, color = "blue") %>%
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
p %>%
  saveWidget(
    str_c(export_path, "3d-unit-normal.html"),
    selfcontained = F, libdir = "lib"
  )



## 三维物体投影到二维

# 为不同的面涂不同的颜色，法向量与光源夹角越小，使其颜色越亮

# 光源向量为(1,1,0)，只能看见两个面
## lightsource <- data.table(x = 1, y = 1, z = 0)
## base1 <- data.table(x = -1 / sqrt(2), y = 1 / sqrt(2), z = 0)
## base2 <- data.table(x = 0, y = 0, z = 1)

# 光源向量为(0,0,1)，即z轴，能看见四个面
## lightsource <- data.table(x = 0, y = 0, z = 1)
## base1 <- data.table(x = 1, y = 0, z = 0)
## base2 <- data.table(x = 0, y = 1, z = 0)

# base1 和 base2 是与观察者视线垂直的平面的一组基
base1 <- data.table(x = 8 / 9, y = -4 / 9, z = 1 / 9)
base2 <- data.table(x = 1 / sqrt(26), y = 3 / sqrt(26), z = 4 / sqrt(26))
# 而光源向量并不一定与观察者的视线重合，所以其实光源向量是任意的
lightsource <- cross_product(base1, base2)
lightsource


p <- render_polytope(octahedron_face, lightsource, base1, base2) %>%
  layout(
    title = list(text = plotly::TeX("\\text{lightsource vector}=(-0.4140, -0.6755, 0.6101)")),
    xaxis = list(title = ""), yaxis = list(title = "")
  ) %>%
  config(mathjax = "cdn")
p
# 有一个面很亮，更接近垂直于光源向量
p %>%
  saveWidget(
    str_c(export_path, "3d-project-to-2d.html"),
    selfcontained = F, libdir = "lib"
  )



## 光源与立方体的实时演算

# 定义一个 transform 函数，接收一个 list，其中包含：
# 平移向量，旋转角度，放缩规模，光源向量
# 一个多面体数据框，受 transform() 转换，生成新数据框
# 多个 list 参数一并传给 accumulate()，生成所有中间状态
# reduce() 行合并后，就可以绘图了

library(scales)
num_to_color <- function(num_domain, color_range) {
  color_scale <- colour_ramp(color_range)

  num_domain %>%
    rescale() %>%
    color_scale()
}

scale <- function(polyhedron, scalar) {
  polyhedron %>%
    mutate(x = x * scalar, y = y * scalar, z = z * scalar)
}

translate <- function(polyhedron, dx, dy, dz) {
  polyhedron %>%
    mutate(
      x = x + dx,
      y = y + dy,
      z = z + dz
    )
}

rotate_z <- function(polyhedron, theta) {
  x0 <- polyhedron$x
  y0 <- polyhedron$y
  polyhedron %>%
    mutate(
      x = x0 * cos(theta) - y0 * sin(theta),
      y = x0 * sin(theta) + y0 * cos(theta)
    )
}

shed_light_on <- function(polyhedron, lightsource) {
  points <- polyhedron %>%
    select(x, y, z) %>%
    drop_na()

  faces <- polyhedron %>% select(i, j, k)

  face_list <- 1:nrow(faces) %>% map(function(t) {
    point_index <- c(faces[t, ]$i + 1, faces[t, ]$j + 1, faces[t, ]$k + 1)
    points[point_index, ]
  })

  normal_list <- face_list %>% map(~ face_normal(.x)) # 法向量列表

  if_bright_list <- normal_list %>%
    map_lgl(~ dot_product(.x, lightsource) > 0)

  # 能够被照亮的面的序号
  bright_faces_index <- (1:nrow(faces))[if_bright_list]

  bright_face_color <- bright_faces_index %>%
    map_dbl(~ angle_between(normal_list[[.x]], lightsource))

  # 颜色向量
  colors <- rep(pi / 2, nrow(faces)) # 无法被照亮的面，颜色赋值为夹角上限
  colors[bright_faces_index] <- bright_face_color
  # print(colors)

  polyhedron %>% mutate(facecolor = colors)
}


# 变换函数，先旋转，再放缩，最后平移
transform_polyhedron <- function(polyhedron, transformation, lightsource) {
  polyhedron %>%
    rotate_z(transformation$theta) %>%
    scale(transformation$scalar) %>%
    translate(transformation$dx, transformation$dy, transformation$dz) %>%
    shed_light_on(lightsource)
}


cube <- data.table(
  x = c(0, 0, 10, 10, 0, 0, 10, 10, NA, NA, NA, NA),
  y = c(0, 10, 10, 0, 0, 10, 10, 0, NA, NA, NA, NA),
  z = c(0, 0, 0, 0, 10, 10, 10, 10, NA, NA, NA, NA),
  i = c(0, 0, 0, 0, 4, 4, 2, 2, 0, 0, 2, 2),
  j = c(3, 7, 1, 2, 7, 6, 1, 5, 4, 5, 6, 7),
  k = c(7, 4, 2, 3, 6, 5, 5, 6, 5, 1, 7, 3)
)
cube[, frame := 0][, facecolor := acos(sqrt(1 / 3))]
cube


# fig <- create_canvas_3d(10) %>%
#   add_mesh(
#     data = cube,
#     x = ~x,
#     y = ~y,
#     z = ~z,
#     i = ~i,
#     j = ~j,
#     k = ~k,
#     facecolor = rep("white", 12),
#     opacity = 0.5
#   ) %>%
#   layout(
#     scene = list(
#       camera = list(
#         eye = list(
#           x = 1,
#           y = -1,
#           z = 1
#         )
#       )
#     )
#   )
# fig


transformation <- data.table(
  theta = rep(2 * pi / 100, 100),
  scalar = rep(1.007, 100),
  dx = rep(0, 100),
  dy = rep(0, 100),
  dz = rep(-0.2, 100)
  # theta = rep(0, 100),
  # scalar = rep(1, 100),
  # dx = rep(0, 100),
  # dy = rep(0, 100),
  # dz = rep(0, 100)
)


# 旋转光源，重新计算面的亮度
# 每个光源为数据增添一个 frame 属性值，最后用上 frame 做动图
light_rotate <- 1:100 * pi / 50 + pi / 4 # 光源在xy平面上转一圈
lightsource <- data.table(
  x = rep(1, 100),
  y = rep(1, 100),
  # x = sqrt(2) * cos(light_rotate),
  # y = sqrt(2) * sin(light_rotate),
  z = rep(1, 100)
)

cube_timeline <- 1:100 %>%
  accumulate(.f = function(data, t) {
    data %>%
      transform_polyhedron(transformation[t, ], lightsource[t, ]) %>%
      mutate(frame = t)
  }, .init = cube) %>%
  map_dfr(~.x) %>%
  mutate(facecolor = num_to_color(
    facecolor,
    c("white", "lightblue", "royalblue", "blue", "darkblue", "black")
  ))


fig <- create_canvas_3d(28) %>%
  add_mesh(
    data = cube_timeline,
    x = ~x,
    y = ~y,
    z = ~z,
    i = ~i,
    j = ~j,
    k = ~k,
    facecolor = ~facecolor,
    opacity = 1,
    frame = ~frame
  ) %>%
  layout(
    scene = list(
      camera = list(
        eye = list(
          x = 1,
          y = -1,
          z = 1
        )
      )
    )
  ) %>%
  animation_opts(frame = 50)
fig
fig %>%
  saveWidget(
    str_c(export_path, "3d-fixed-lightsource.html"),
    selfcontained = F, libdir = "lib"
  )



## helicopter 数据中已包含颜色属性，此处演示如何计算颜色

helicopter <- fread("./data/3d-mesh-helicopter.csv")
helicopter

# create_canvas_3d(150) %>%
#   add_mesh(
#     data = helicopter, x = ~x, y = ~y, z = ~z,
#     i = ~i, j = ~j, k = ~k, # 各三角面三个顶点的索引
#     facecolor = ~facecolor,
#     # facecolor = facecolor,
#     opacity = 1
#   ) %>%
#   layout(
#     scene = list(
#       camera = list(
#         eye = list(
#           x = 1,
#           y = -1,
#           z = 1
#         )
#       )
#     )
#   )


lightsource <- data.table(x = 1, y = -1, z = 1) # 光源

helicopter <- shed_light_on(helicopter, lightsource) %>%
  mutate(facecolor = num_to_color(
    facecolor,
    c("white", "lightblue", "royalblue", "blue", "darkblue", "black")
  ))

p <- create_canvas_3d(150) %>%
  add_mesh(
    data = helicopter,
    x = ~x, y = ~y, z = ~z,
    i = ~i, j = ~j, k = ~k, # 各三角面三个顶点的索引
    facecolor = ~facecolor,
    opacity = 1
  ) %>%
  layout(
    scene = list(
      camera = list(
        eye = list(
          x = 1,
          y = -1,
          z = 1
        )
      )
    )
  )
p