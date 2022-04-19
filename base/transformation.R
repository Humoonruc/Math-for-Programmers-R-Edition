## vector-calculate.R

# 1 向量加减法
# 2 向量模长
# 3 极坐标与直角坐标的转换
# 4 点积
# 5 三维向量的叉积

## 图形（点的集合，一个矩阵）运算
# 6 放缩，可以直接数乘
# 7 平移，每行加一个向量，即加一个由向量扩充而来的矩阵
# 8 旋转，矩阵乘法
# 9 投影
# 10 再平面上渲染三维物体


source("./config/config.R")

# 二维平面中四个点
point1 <- c(1, 2)
point2 <- c(3, 4)
point3 <- c(5, 4)
point4 <- c(7, 2)

# 它们的集合，组织为一个矩阵，代表该多边形（是个梯形）
polygon <- c(v1, v2, v3, v4) %>% matrix(nrow = 2)


#############################################################
# 1 向量加减法
#############################################################

# 普通的加减直接用运算符，R 默认支持向量化操作

# 两点连线的中点
midpoiot <- function(v1, v2) {
  (v1 + v2) / 2
}

#############################################################
# 2 向量模长
#############################################################

get_length <- function(v) {
  sum(v^2) %>%
    sqrt()
}

# 两点（两个向量端点之间的）距离
get_distance <- function(v1, v2) {
  get_length(v1 - v2)
}

# 计算多边形周长
get_perimeter <- function(polygon) {
  n <- ncol(polygon)
  perimeter <- 0
  for (i in 1:n) {
    if (i < n) {
      perimeter <- perimeter + get_distance(polygon[, i], polygon[, i + 1])
      # 此处，矩阵的 subset 自动转换为最简单的数据类型，即向量
    } else {
      perimeter <- perimeter + get_distance(polygon[, n], polygon[, 1])
    }
  }
  return(perimeter)
}


#############################################################
# 3 极坐标与直角坐标的转换
#############################################################

to_cartesian_2d <- function(points_polar) {
  c(points_polar[1] * cos(points_polar[2]), points_polar[1] * sin(points_polar[2]))
}

to_polor_2d <- function(points) {
  c(get_length(points), atan2(points[2], points[1]))
}


#############################################################
# 4 点积
#############################################################

dot_product <- function(v1, v2) {
  sum(v1 * v2)
}


# 用点积求向量夹角，范围: [0,pi]
angle_between <- function(v1, v2) {
  acos(
    dot_product(v1, v2) / (get_length(v1) * get_length(v2))
  )
}

# 用点积求投影分量的长度
component <- function(v, direction) {
  dot_product(v, direction) / get_length(direction)
}


#############################################################
# 5 三维向量的叉积
#############################################################

cross_product <- function(u, v) {
  c(
    u[2] * v[3] - u[3] * v[2],
    u[3] * v[1] - u[1] * v[3],
    u[1] * v[2] - u[2] * v[1]
  )
}

# 求一个（三角）面的法向量
# face 是一个3行向量，每行代表一个点
face_normal <- function(face) {
  cross_product(face[2, ] - face[1, ], face[3, ] - face[1, ])
}

# 求一个面的单位法向量
face_unit_normal <- function(face) {
  face_normal(face) / get_length(face_normal(face))
}