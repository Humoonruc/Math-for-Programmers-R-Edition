## calculate3D.R


# 向量取模
get_length <- function(points) {
  points %>%
    map_dfc(\(x) x^2) %>%
    rowSums() %>%
    sqrt()
}


# 向量加减法
vector_add <- function(v1, v2) {
  rbind(v1, v2) %>%
    map_dfc(\(x) sum(x)) %>%
    as.data.table()
}

vector_subtract <- function(v1, v2) {
  v2 %>%
    map_dfc(\(x) -x) %>%
    as.data.table() %>%
    vector_add(v1, .)
}

vector_sum <- function(vs) {
  vs %>%
    map_dfc(\(x) sum(x)) %>%
    as.data.table()
}


# 两点（两个向量端点之间的）距离
get_distance <- function(point1, point2) {
  get_length(vector_subtract(point1, point2))
}


# 点积
dot_product <- function(v1, v2) {
  v1 %>%
    as.matrix() %*% t(v2) %>%
    as.vector()
}


# 用点积求向量夹角
angle_between <- function(v1, v2) {
  acos(
    dot(v1, v2) / (get_length(v1) * get_length(v2))
  )
}

# 用点积求投影分量的长度
component <- function(v, direction) {
  dot_product(v, direction) / get_length(direction)
}

# 三维向量的叉积
cross_product <- function(u, v) {
  data.table(
    x = u$y * v$z - u$z * v$y,
    y = u$z * v$x - u$x * v$z,
    z = u$x * v$y - u$y * v$x
  ) %>% return()
}

# 返回一个面的单位法向量
unit_normal <- function(face) {
  v2_v1 <- vector_subtract(face[2, ], face[1, ])
  v3_v1 <- vector_subtract(face[3, ], face[1, ])
  normal <- cross_product(v2_v1, v3_v1)
  scale(normal, 1 / get_length(normal))
}