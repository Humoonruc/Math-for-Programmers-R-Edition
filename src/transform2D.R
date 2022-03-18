## transform2D.R


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
  m_points <- points %>%
    as.matrix() %>%
    t()

  return(
    m_scale %*% m_points %>%
      t() %>%
      as.data.frame() %>%
      set_colnames(variable_names) %>%
      as.data.table()
  )
}


# 旋转，第二个参数是旋转弧度
rotate <- function(points, rotation) {
  # 方法二：转换到极坐标系中改变角度
  ## points %>%
  ##   to_polor() %>%
  ##   mutate(theta = theta + rotation) %>%
  ##   to_cartesian()

  # 方法二：用矩阵乘法实现线性变换
  variable_names <- colnames(points)
  m_rotate <- c(cos(rotation), sin(rotation), -sin(rotation), cos(rotation)) %>%
    matrix(nrow = 2, byrow = FALSE) # 二维旋转矩阵
  m_points <- points %>%
    as.matrix() %>%
    t()

  return(
    m_rotate %*% m_points %>%
      t() %>%
      as.data.frame() %>%
      set_colnames(variable_names) %>%
      as.data.table()
  )
}