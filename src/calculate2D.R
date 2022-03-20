## calculate2D.R


#############################################################
# 向量加减法
vector_add <- function(v1, v2) {
  rbind(v1, v2) %>%
    map_dfc(\(x) sum(x)) %>%
    as.data.table()
}

vector_sum <- function(vs) {
  vs %>%
    map_dfc(\(x) sum(x)) %>%
    as.data.table()
}

vector_subtract <- function(v1, v2) {
  v2 %>%
    map_dfc(\(x) -x) %>%
    as.data.table() %>%
    vector_add(v1, .)
}

# 两点的中点
vector_midpoiot <- function(v1, v2) {
  vector_add(v1, v2) %>% scale(0.5)
}

#############################################################
# 向量取模
get_length <- function(vs) {
  vs %>%
    map_dfc(\(x) x^2) %>%
    rowSums() %>%
    sqrt()
}

# 两点（两个向量端点之间的）距离
get_distance <- function(point1, point2) {
  get_length(vector_subtract(point1, point2))
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
# 2d极坐标与直角坐标的转换
to_cartesian_2d <- function(points_polar) {
  points_polar %>%
    mutate(x = r * cos(theta), y = r * sin(theta)) %>%
    select(x, y)
}

to_polor_2d <- function(points) {
  points %>%
    mutate(r = sqrt(x^2 + y^2), theta = atan2(y, x)) %>%
    select(r, theta)
}


#############################################################
# 点积
dot_product <- function(v1, v2) {
  v1 %>%
    as.matrix() %*% t(v2) %>%
    as.vector()
}

# 用点积求向量夹角
angle_between <- function(v1, v2) {
  acos(
    dot_product(v1, v2) / (get_length(v1) * get_length(v2))
  )
}

# 用点积求投影分量的长度
component <- function(v, direction) {
  dot_product(v, direction) / get_length(direction)
}