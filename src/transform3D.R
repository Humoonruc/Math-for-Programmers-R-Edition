## transform3D.R


# 平移
translate <- function(points, translation) {
  n <- ncol(points)
  variable_names <- c("b", colnames(points))
  m1 <- rep(0, (n + 1)) %>% t() # 第一行
  m2 <- cbind(t(translation), diag(n)) # 后n行
  m_translate <- rbind(m1, m2)

  m_points <- points %>%
    mutate(b = 1) %>%
    select(b, everything()) %>%
    t()

  return(
    m_translate %*% m_points %>%
      t() %>%
      as.data.frame() %>%
      set_colnames(variable_names) %>%
      select(-b) %>%
      as.data.table()
  )
}


# 放缩
scale <- function(points, scalar) {
  n <- ncol(points)
  variable_names <- colnames(points)
  m_scale <- diag(n) * scalar # 放缩矩阵
  m_points <- points %>% t()

  return(
    m_scale %*% m_points %>%
      t() %>%
      as.data.frame() %>%
      set_colnames(variable_names) %>%
      as.data.table()
  )
}


# 旋转，第二个参数是旋转弧度
rotate <- function(points, m_rotate) {
  variable_names <- colnames(points)
  m_points <- points %>% t()
  ## m_rotate <- c(cos(rotation), sin(rotation), -sin(rotation), cos(rotation)) %>%
  ##   matrix(nrow = 2, byrow = FALSE) # 二维旋转矩阵

  return(
    m_rotate %*% m_points %>%
      t() %>%
      as.data.frame() %>%
      set_colnames(variable_names) %>%
      as.data.table()
  )
}


# 将三维向量/向量数组投影到二维，需要二维空间的一组正交基
project <- function(points,
                    base1 = data.table(x = 1, y = 0, z = 0),
                    base2 = data.table(x = 0, y = 1, z = 0)) {
  rbind(base1, base2) %>%
    as.matrix() %*% t(points) %>%
    t() %>%
    as.data.frame() %>%
    set_colnames(c("x", "y")) %>%
    as.data.table()
}

# 三维多面体投影到二维
# 默认观察者从上向下看，立体图形将被投影到x-y平面
project_polytope <- function(polytope,
                             base1 = data.table(x = 1, y = 0, z = 0),
                             base2 = data.table(x = 0, y = 1, z = 0)) {
  polytope %>% map(.f = project)
}