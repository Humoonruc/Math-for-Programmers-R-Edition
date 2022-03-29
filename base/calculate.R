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



v1 <- c(1, 2)
v2 <- c(3, 4)
v3 <- c(5, 6)
v4 <- c(7, 8)

vs <- list(v1, v2, v3, v4) %>%
  reduce(~ rbind(.x, .y))

vs
nrow(vs)



#############################################################
# 1 向量加减法
#############################################################

# 普通的加减直接用运算符

vector_sum <- function(vs) {
  vs %>% apply(2, sum)
}

# 两点连线的中点
vector_midpoiot <- function(v1, v2) {
  v1 / 2 + v2 / 2
}

#############################################################
# 2 向量模长
#############################################################

get_length <- function(v) {
  sum(v * v) %>%
    sqrt()
}

# 两点（两个向量端点之间的）距离
get_distance <- function(v1, v2) {
  get_length(v1 - v2)
}

# 计算多边形周长
get_perimeter <- function(points) {
  n <- nrow(points)
  s <- 0
  for (i in 1:n) {
    if (i < n) {
      s <- s + get_distance(points[i, ], points[i + 1, ])
    } else {
      s <- s + get_distance(points[n, ], points[1, ])
    }
  }
  return(s)
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