## linear-transform.R

# 1 平移
# 2 放缩
# 3 旋转
# 4 投影
# 5 在平面上渲染三维物体


#############################################################
# 1 平移
#############################################################

# translation为平移向量(也是一个数据框)
translate <- function(points, translation) {
  n <- ncol(points)
  variable_names <- c("b", colnames(points))
  m1 <- c(0, t(translation)) # 第一列
  m2 <- rbind(t(rep(0, n)), diag(n)) # 后n列
  m_translate <- cbind(m1, m2)

  m_points <- points %>%
    mutate(b = 1) %>%
    select(b, everything()) %>%
    t()

  # return value
  m_translate %*% m_points %>%
    t() %>%
    as.data.frame() %>%
    set_colnames(variable_names) %>%
    select(-b) %>%
    as.data.table()
}


#############################################################
# 2 放缩
#############################################################

# scalar为标量
scale <- function(points, scalar) {
  n <- ncol(points)
  variable_names <- colnames(points)
  m_scale <- diag(n) * scalar # 放缩矩阵
  m_points <- points %>%
    as.matrix() %>%
    t()

  # return value
  m_scale %*% m_points %>%
    t() %>%
    as.data.frame() %>%
    set_colnames(variable_names) %>%
    as.data.table()
}


#############################################################
# 3 旋转
#############################################################

# 二维旋转
# rotation是逆时针旋转的弧度
rotate_2d <- function(points, rotation) {
  # 方法一：转换到极坐标系中改变角度
  ## points %>%
  ##   to_polor_2d() %>%
  ##   mutate(theta = theta + rotation) %>%
  ##   to_cartesian_2d()

  # 方法二：用矩阵乘法实现线性变换
  variable_names <- colnames(points)
  m_rotate <- c(cos(rotation), sin(rotation), -sin(rotation), cos(rotation)) %>%
    matrix(nrow = 2, byrow = FALSE) # 二维旋转矩阵
  m_points <- points %>%
    as.matrix() %>%
    t()

  # return value
  m_rotate %*% m_points %>%
    t() %>%
    as.data.frame() %>%
    set_colnames(variable_names) %>%
    as.data.table()
}


#############################################################
# 4 投影
#############################################################

# 三维投影到二维
# points是一个数据框
# base1, base2是二维平面的一组正交基的三维坐标
# 投影本质上是一种降维的线性变换
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
# polytope是多个三角面数据框组成的list
project_polytope <- function(polytope, ...) {
  polytope %>%
    map(function(face) project(face, ...))
}

## 测试：
## points <- list(data.table(x = 1, y = 1, z = 1), data.table(x = 2, y = 2, z = 2))
## project(points[[1]])
## project_polytope(points)


#############################################################
# 5 在平面上渲染三维物体
#############################################################

# 筛选出能够被照亮的面
# 即法向量与光源向量的点乘为正（假设光源投来平行光）
bright_faces <- function(faces,
                         lightsource = data.table(x = 0, y = 0, z = 1)) {
  faces %>%
    keep(~ dot_product(face_normal(.x), lightsource) > 0)
}


# 渲染三维多面体
# ... 是二维平面的一组正交基的三维坐标
render_polytope <- function(polytope, lightsource, ...) {
  visible_faces <- polytope %>% bright_faces(lightsource)

  faces_2d <- project_polytope(visible_faces, ...)
  face_brightness <- visible_faces %>%
    map(face_unit_normal) %>%
    map(~ angle_between(.x, lightsource)) %>% # 用角度表示亮度，此处范围为[0, pi/2)
    map(~ ((1 - .x * 2 / pi) - 0.5) / 2 + 0.5) # 归一化，然后以0.5为中心适当缩放，距离极端的黑白两色远一点

  create_canvas_2d() %>%
    reduce2(
      .x = faces_2d,
      .y = face_brightness,
      .f = function(plot, face, brightness) {
        plot %>%
          draw_polygon(face, fill = brightness %>% gray())
      },
      # gray()将[1,0]映射到白色至黑色的灰色带上
      .init = .
    ) 
}
