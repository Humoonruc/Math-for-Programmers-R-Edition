## calculate3D.R

# 三维向量的叉积
cross_product <- function(u, v) {
  data.table(
    x = u$y * v$z - u$z * v$y,
    y = u$z * v$x - u$x * v$z,
    z = u$x * v$y - u$y * v$x
  ) %>% return()
}


# 返回一个面的法向量
face_normal <- function(face) {
  v2_v1 <- vector_subtract(face[2, ], face[1, ])
  v3_v1 <- vector_subtract(face[3, ], face[1, ])
  cross_product(v2_v1, v3_v1)
}

# 返回一个面的单位法向量
face_unit_normal <- function(face) {
  scale(face_normal(face), 1 / get_length(face_normal(face)))
}
