## transform3D.R


# 将三维向量/向量数组投影到二维，需要二维空间的一组正交基
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


# 筛选出能够被照亮的面
# 法线与光源向量的点乘为正（假设光源投来平型光）
bright_faces <- function(faces,
                         lightsource = data.table(x = 0, y = 0, z = 1)) {
  Filter(
    function(face) {
      normal <- face_normal(face) # 法向量
      dot_product(normal, lightsource) > 0
    },
    faces
  )
}


# 三维多面体投影到二维
# 默认观察者从上向下看，立体图形将被投影到x-y平面
project_polytope <- function(polytope,
                             base1 = data.table(x = 1, y = 0, z = 0),
                             base2 = data.table(x = 0, y = 1, z = 0)) {
  polytope %>%
    Map(function(face) {
      project(face, base1 = base1, base2 = base2)
    }, .)
}
