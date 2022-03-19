## transform3D.R


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