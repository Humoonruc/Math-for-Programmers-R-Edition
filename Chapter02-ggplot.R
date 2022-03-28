## Chapter02-ggplot.R


# config
source("./config/config.R")


# import modules
source("./src/ggplot.R")
source("./src/vector-calculate.R")
source("./src/linear-transform.R")


# data
dinosaur <- fread("./data/data.csv")
dinosaur


###############################################################
## 1 plotting vectors
###############################################################

# plot and save
p <- create_canvas() %>%
  draw_scatter(dinosaur) %>%
  draw_polygon(dinosaur) +
  scale_x_continuous(breaks = seq(-10, 10, 1), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(-10, 10, 1), minor_breaks = NULL)
p
ggsave("./export/ggplot/dinosaur.png", width = 6, height = 6, dpi = 600)


# vector translation
dinosaur2 <- dinosaur %>% translate(c(-1.5, -2.5))

create_canvas() %>%
  draw_scatter(dinosaur) %>%
  draw_polygon(dinosaur) %>%
  draw_scatter(dinosaur2, color = "red") %>%
  draw_polygon(dinosaur2, color = "red") +
  scale_x_continuous(breaks = seq(-10, 10, 1), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(-10, 10, 1), minor_breaks = NULL)

ggsave("./export/ggplot/dinosaur2.png", width = 6, height = 6, dpi = 600)


# 81 只小恐龙

p <- data.table(x = 0, y = 0) %>%
  ggplot(mapping = aes(x, y)) +
  geom_point(size = 4, shape = 4) +
  theme_light() +
  theme(
    panel.border = element_rect(color = "black"),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

x_domain <- -4:4 * 12
y_domain <- -4:4 * 10

# 循环写法一
for (dx in x_domain) {
  for (dy in y_domain) {
    translation <- data.table(x = dx, y = dy)
    p <- p %>% draw_polygon(dinosaur %>% translate(translation))
  }
}

# 循环写法二
# translation <- x_domain %>% map_dfr(function(dx) {
#   y_domain %>% map_dfr(function(dy) {
#     data.table(x = dx, y = dy)
#   })
# })

# p <- 1:nrow(translation) %>% reduce(
#   .f = function(plot, k) {
#     plot %>% draw_polygon(dinosaur %>% translate(translation[k, ]))
#   },
#   .init = p
# )


p +
  scale_x_continuous(breaks = seq(-100, 100, 20)) +
  scale_y_continuous(breaks = seq(-100, 100, 20))

ggsave("./export/ggplot/multi-dinosaur.png", width = 6, height = 6, dpi = 600)


###############################################################
## 2 vector arithmetic
###############################################################

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

possible_space <- map2_dfr(random_r, random_s, ~ vector_add(scale(z, .x), scale(v, .y)))

create_canvas() %>%
  draw_scatter(possible_space, color = "black") +
  scale_x_continuous(breaks = seq(-10, 10, 1), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(-10, 10, 1), minor_breaks = NULL)

ggsave("./export/ggplot/possible-space.png", width = 6, height = 6, dpi = 600)


###############################################################
## 3 trigonometry and polar coordinate system
###############################################################

# flower
i <- 0:1000
points_polor <- data.table(r = cos(i * pi / 100), theta = 2 * pi * i / 1000)
points <- points_polor %>% to_cartesian_2d()

create_canvas() %>%
  draw_polygon(points, color = "blue") +
  scale_x_continuous(breaks = seq(-10, 10, 1), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(-10, 10, 1), minor_breaks = NULL)

ggsave("./export/ggplot/flower.png", width = 6, height = 6, dpi = 600)


# rotate dinosaur
dinosaur_rotated <- rotate_2d(dinosaur, pi / 4)

create_canvas() %>%
  draw_scatter(dinosaur) %>%
  draw_polygon(dinosaur) %>%
  draw_scatter(dinosaur_rotated, color = "red") %>%
  draw_polygon(dinosaur_rotated, color = "red") +
  scale_x_continuous(breaks = seq(-10, 10, 1), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(-10, 10, 1), minor_breaks = NULL)

ggsave("./export/ggplot/dinosaur_rotated.png", width = 6, height = 6, dpi = 600)


###############################################################
## 4 linear transforming
###############################################################

# 生成正n边形
regular_polygon <- function(n) {
  vertex0 <- data.table(x = 1, y = 0)
  0:(n - 1) %>%
    map_dfr(~ vertex0 %>% rotate_2d(2 * pi * .x / n))
}

n <- 7

create_canvas() %>%
  draw_scatter(regular_polygon(n)) %>%
  draw_polygon(regular_polygon(n)) +
  scale_x_continuous(breaks = seq(-10, 10, 1), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(-10, 10, 1), minor_breaks = NULL)

ggsave("./export/ggplot/regular-polygon.png", width = 6, height = 6, dpi = 600)
