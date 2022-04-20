## transformation.R


# 1 线段中点
# 2 向量模长、两点距离、多边形周长
# 3 极坐标与直角坐标的转换
# 4 点积、向量夹角、某方向的分量长度
# 5 三维向量的叉积、三角面的法向量
# 6 放缩，直接数乘
# 7 平移，直接加向量
# 8 旋转，矩阵乘法
# 9 投影


source("./config/config.R")


# 二维平面中四个点
point1 <- c(1, 2)
point2 <- c(3, 4)
point3 <- c(5, 4)
point4 <- c(7, 2)

# 它们的集合，组织为一个矩阵，代表该多边形（是个梯形）
polygon <- c(point1, point2, point3, point4) %>%
  matrix(nrow = 2)


#############################################################
# 1 向量加减法，直接用运算符，R 默认支持向量化操作
#############################################################

# 两点连线的中点
midpoiot <- function(v1, v2) {
  (v1 + v2) / 2
}


#############################################################
# 2 向量模长, modulus of a vector
#############################################################

modulus <- function(v) {
  sum(v^2) %>%
    sqrt()
}

# 两点（两个向量端点之间的）距离
distance <- function(v1, v2) {
  modulus(v1 - v2)
}

# 多边形周长
perimeter <- function(polygon) {
  n <- ncol(polygon)
  perimeter <- 0

  1:(n - 1) %>%
    reduce(
      .f = function(perimeter, k) {
        perimeter + distance(polygon[, k], polygon[, k + 1])
      },
      .init = 0
    ) + distance(polygon[, n], polygon[, 1])
}


#############################################################
# 3 极坐标与直角坐标的转换
#############################################################

to_cartesian_2d <- function(points) { # point 为极坐标下的 c(r, theta)
  if (is.matrix(points)) {
    points %>% apply(2, function(point) {
      c(
        point[1] * cos(point[2]),
        point[1] * sin(point[2])
      )
    })
  } else {
    c(
      points[1] * cos(points[2]),
      points[1] * sin(points[2])
    )
  }
}

to_polor_2d <- function(points) {
  if (is.matrix(points)) {
    points %>% apply(2, function(point) {
      c(
        modulus(point),
        atan2(point[2], point[1]) # atan2() 能返回 [0, 2pi) 范围内的唯一值
      )
    })
  } else {
    c(
      modulus(points),
      atan2(points[2], points[1]) # atan2() 能返回 [0, 2pi) 范围内的唯一值
    )
  }
}


#############################################################
# 4 点积
#############################################################

dot_product <- function(v1, v2) {
  sum(v1 * v2)
}

# 用点积求向量夹角，范围: [0,pi]
angle_between <- function(v1, v2) {
  dot_product(v1, v2) %>%
    `/`(modulus(v1) * modulus(v2)) %>%
    acos()
}

# 用点积求投影到某方向的分量长度
component <- function(v, direction) {
  dot_product(v, direction) / modulus(direction)
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
normal <- function(face) {
  # face 是一个3列矩阵，每列代表一个点
  cross_product(face[, 2] - face[, 1], face[, 3] - face[, 1])
}

# 求一个（三角）面的单位法向量
unit_normal <- function(face) {
  normal(face) / modulus(normal(face))
}


#############################################################
# 6 放缩，直接数乘即可
#############################################################

# scale <- function(points, scalar) {
#   # points矩阵代表多个点，scalar为放缩倍数（标量）
#   points * scalar
# }


#############################################################
# 7 平移，直接加一个平移向量即可，原生向量化运算自动将向量扩展为矩阵
#############################################################

# translate <- function(points, translation) {
#   # points矩阵代表多个点，translation为平移向量
#   points + translation
# }


#############################################################
# 8 旋转
#############################################################

# 二维旋转
rotate_2d <- function(points, rotation) {
  # rotation是逆时针旋转的弧度

  # 方法一：转换到极坐标系中改变角度
  # points %>%
  #   apply(2, FUN = function(point) to_polor_2d(point)) %>%
  #   `+`(c(0, rotation)) %>%
  #   apply(2, FUN = function(point) to_cartesian_2d(point))

  # 方法二：用矩阵乘法实现这个线性变换
  rotate_matrix <- c(
    cos(rotation), -sin(rotation),
    sin(rotation), cos(rotation)
  ) %>%
    matrix(nrow = 2, byrow = TRUE) # 二维旋转矩阵
  rotate_matrix %*% points
}


#############################################################
# 9 投影
#############################################################

# 三维点集投影到二维(投影本质上是一种降维的线性变换)
project <- function(points,
                    base1 = c(x = 1, y = 0, z = 0),
                    base2 = c(x = 0, y = 1, z = 0)) {
  # points 是一个矩阵，每列是一个点
  # base1, base2 是二维平面的一组正交基的三维坐标

  rbind(t(base1), t(base2)) %*% points
}

## 测试：
points <- cbind(c(x = 1, y = 1, z = 1), c(x = 2, y = 2, z = 2))
project(points)