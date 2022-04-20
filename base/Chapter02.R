## Chapter02.R


# import modules
source("./config/config.R")
source("./base/transformation.R")
source("./base/plotly.R")


# data
dinosaur <- fread("./data/data.csv") %>% t() # 原始数据文件
# save(dinosaur, file = "./data/dinosaur.rda")
# load(file = "./data/dinosaur.rda")
distances <- dinosaur %>% apply(2, modulus)
dinosaur[, which.max(distances)] # 距离原点最远的点
dinosaur[, which.min(distances)] # 距离原点最仅的点
perimeter(dinosaur) # 周长


# export path
export_path <- "./export/"


# canvas
canvas_2d <- create_canvas_2d()


###############################################################
## 1 plotting vectors
###############################################################

# plot and save
p <- canvas_2d %>%
  draw_points(dinosaur) %>%
  draw_polygon(dinosaur, opacity = 0.3) %>%
  layout(
    title = list(text = "A dinasaur: scatters and lines"),
    xaxis = list(title = ""), yaxis = list(title = "")
  )
p
p %>% saveWidget(
  str_c(export_path, "dinosaur.html"),
  selfcontained = F, libdir = "lib"
)


# translation
dinosaur2 <- dinosaur + c(x = -1.5, y = -2.5)

p <- canvas_2d %>%
  draw_points(dinosaur) %>%
  draw_polygon(dinosaur, color = "blue", opacity = 0.3) %>%
  draw_points(dinosaur2, color = "red") %>%
  draw_polygon(dinosaur2, color = "red", opacity = 0.3) %>%
  reduce(
    .x = 1:ncol(dinosaur),
    .f = function(p, i) {
      p %>%
        draw_arrow_2d(dinosaur[, i], dinosaur2[, i],
          linetype = "dash"
        )
    },
    .init = .
  ) %>%
  layout(
    title = list(text = "2 dinasaurs: translation"),
    xaxis = list(title = ""), yaxis = list(title = "")
  )
p
p %>%
  saveWidget(
    str_c(export_path, "dinosaur-translation.html"),
    selfcontained = F, libdir = "lib"
  )


# 81 只小恐龙
dx <- -4:4 * 12
dy <- -4:4 * 10

# p <- expand.grid(dx, dy) %>% # 交叉遍历
#   t() %>%
#   as.data.frame() %>% # 矩阵变成数据框，才能一列一列地传给高阶函数
#   reduce(
#     .f = function(p, translation) {
#       p %>%
#         draw_polygon(dinosaur + translation,
#           color = "blue", opacity = 0.5
#         )
#     },
#     .init = canvas_2d %>% layout(
#       title = list(text = "many dinasaurs: translation")
#     )
#   )

p <- canvas_2d %>% layout(
  title = list(text = "many dinasaurs: translation"),
  xaxis = list(title = ""), yaxis = list(title = "")
)
expand.grid(dx, dy) %>% # 交叉遍历
  apply(1, function(translation) {
    p <<- (dinosaur + translation) %>%
      draw_polygon(p, ., color = "blue", opacity = 0.5)
    return(NULL)
  })

p
p %>%
  saveWidget(
    str_c(export_path, "dinosaur-multi.html"),
    selfcontained = F, libdir = "lib"
  )


###############################################################
## vector arithmetic
###############################################################

# r·z + s·v = (z, v) %*% (r, s)' 的范围
z <- c(x = -1, y = 1)
v <- c(x = 1, y = 1)
z_max <- z * 3
z_min <- z * (-3)
v_max <- v * 2
v_min <- v * (-2)

n <- 2000 # 样本容量
random_r <- runif(n, -3, 3)
random_s <- runif(n, -2, 2)

possible_space <- cbind(z, v) %*% rbind(random_r, random_s)

# 标量思想的写法
# possible_space <- map2_dfr(
#   random_r, random_s,
#   .f = function(r, s) {
#     z * r + v * s
#   }
# ) %>% t()


p <- canvas_2d %>%
  draw_points(possible_space, color = "grey", opacity = 0.3) %>%
  draw_lines(cbind(z_max, z_min), color = "tomato", linetype = "dash") %>%
  draw_arrow_2d(c(x = 0, y = 0), z, color = "red") %>%
  draw_text(z, texts = plotly::TeX("\\vec{z}=(-1,1)"), color = "red") %>%
  draw_lines(cbind(v_max, v_min), color = "royalblue", linetype = "dash") %>%
  draw_arrow_2d(c(x = 0, y = 0), v, color = "blue") %>%
  draw_text(v, texts = plotly::TeX("\\vec{v}=(1,1)"), color = "blue") %>%
  layout(
    title = list(text = plotly::TeX("r\\vec{z} + s\\vec{v}, r\\in (-3,3), s\\in (-2,2)")),
    xaxis = list(title = ""), yaxis = list(title = "")
  ) %>%
  config(mathjax = "cdn")
p
p %>%
  saveWidget(
    str_c(export_path, "possible-space.html"),
    selfcontained = F, libdir = "lib"
  )

###############################################################
## trigonometry and polar coordinate system
###############################################################

# flower
i <- 0:1000
points_polor <- rbind(
  r = cos(i * pi / 100),
  theta = 2 * pi * i / 1000
)
points <- points_polor %>% apply(2, to_cartesian_2d)

p <- canvas_2d %>%
  draw_polygon(points, fill = "yellow", opacity = 0.2) %>%
  layout(
    title = list(text = "flower consists 1,000 points"),
    xaxis = list(title = ""), yaxis = list(title = "")
  ) %>%
  config(mathjax = "cdn")
p
p %>%
  saveWidget(
    str_c(export_path, "flower.html"),
    selfcontained = F, libdir = "lib"
  )


# rotate dinosaur
dinosaur_rotated <- rotate_2d(dinosaur, pi / 4)

p <- canvas_2d %>%
  draw_points(dinosaur) %>%
  draw_polygon(dinosaur, color = "blue", opacity = 0.3) %>%
  draw_points(dinosaur_rotated, color = "red") %>%
  draw_polygon(dinosaur_rotated, color = "red", opacity = 0.3) %>%
  layout(
    title = list(text = "dinosaur: 2D rotation"),
    xaxis = list(title = ""), yaxis = list(title = "")
  )
p
p %>%
  saveWidget(
    str_c(export_path, "dinosaur-rotation.html"),
    selfcontained = F, libdir = "lib"
  )


# 生成正n边形
regular_polygon <- function(n) {
  # mapply 可以接收向量，返回矩阵
  mapply(
    function(k) {
      rotate_2d(c(x = 1, y = 0), 2 * pi * k / n)
    },
    0:(n - 1)
  )

  # 也可以用 map_dfc() 再转换为 matrix
  # 0:(n - 1) %>%
  #   map_dfc(function(k) {
  #     rotate_2d(c(x = 1, y = 0), 2 * pi * k / n)
  #   }) %>%
  #   as.matrix()
}

heptagon <- regular_polygon(7)

p <- canvas_2d %>%
  draw_points(heptagon) %>%
  draw_polygon(heptagon, color = "blue", opacity = 0.5) %>%
  layout(
    title = list(text = "heptagon: 2D rotation"),
    xaxis = list(title = ""), yaxis = list(title = "")
  )
p
p %>%
  saveWidget(
    str_c(export_path, "regular-polygon.html"),
    selfcontained = F, libdir = "lib"
  )
