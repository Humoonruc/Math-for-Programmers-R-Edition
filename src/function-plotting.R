## function-plotting.R


# 绘制任意函数，默认为蓝色
# 不关心其他属性，都放入...
plot_function <- function(plot, f, xmin, xmax, n = 1000,
                          color = "royalblue", ...) {
  xs <- seq(xmin, xmax, length.out = n + 1)
  ys <- xs %>% map_dbl(f)
  points <- data.table(x = xs, y = ys)

  draw_line(plot, points, color = color, ...)
}


# 割线斜率
secant_slope <- function(f, x1, x2) {
  (f(x2) - f(x1)) / (x2 - x1)
}

# 函数两点形成的割线
secant_line <- function(f, x1, x2) {
  function(x) {
    f(x1) + (x - x1) * secant_slope(f, x1, x2)
  }
}

# 绘制割线
plot_secant <- function(plot, f, xmin, xmax, n, x1, x2,
                        color = "orangered", ...) {
  secant_points <- data.table(x = c(x1, x2), y = c(f(x1), f(x2)))
  secant_function <- secant_line(f, x1, x2)

  plot_function(plot, secant_function, xmin, xmax, n,
    color = color, linetype = "dash", showlegend = FALSE, ...
  ) %>%
    draw_line(secant_points, color = color, showlegend = FALSE)
}


# 导数数组及其绘制
derivative_array <- function(f, xmin, xmax, n) {
  xs <- seq(xmin, xmax, length.out = n + 1)
  delta <- xs[2] - xs[1]

  xs[1:n] %>%
    map_dbl(~ secant_slope(f, .x, .x + delta)) %>%
    data.table(x = xs[1:n] + delta / 2, y = .)
}

plot_derivative_array <- function(plot, f, xmin, xmax, n,
                                  color = "orangered",
                                  name = "derivative array",
                                  showlegend = TRUE, ...) {
  draw_line(
    plot,
    points = derivative_array(f, xmin, xmax, n),
    color = color, name = name, showlegend = showlegend, ...
  )
}


# 任意点导数
derivative <- function(f, x, digits = 6) {
  tolerance <- 10^(-digits)
  delta <- 1
  approx <- secant_slope(f, x - delta, x + delta)
  for (i in 0:(2 * digits)) {
    delta <- delta / 10
    next_approx <- secant_slope(f, x - delta, x + delta)
    if (abs(next_approx - approx) < tolerance) {
      return(round(next_approx, digits))
    } else {
      approx <- next_approx
    }
  }
  stop("Derivative did not converge.")
}

# 导函数及其绘制
get_derivative_function <- function(f, digits = 6) {
  function(x) {
    derivative(f, x, digits = digits)
  }
}

plot_derivative_function <- function(plot, primitive, xmin, xmax, n,
                                     color = "orangered", ...) {
  derivative_function <- get_derivative_function(primitive)

  plot_function(plot, derivative_function, xmin, xmax, n,
    color = color, ...
  )
}


## 原函数及其绘制

# 求黎曼和
definite_integration <- function(f, x1, x2, dx) {
  xs <- seq(x1, x2, by = dx)
  n <- length(xs)

  # 有可能xs的最后一项不等于x2
  (xs[-n] + dx / 2) %>%
    map_dbl(\(x) f(x) * dx) %>%
    sum() %>%
    add(f((xs[n] + x2) / 2) * (x2 - xs[n]))
}

# 求原函数
# dx给出黎曼和的小柱形宽度
get_primitive_function <- function(f, v0, x0, dx) {
  function(x) {
    v0 + definite_integration(f, x0, x, dx)
  }
}

## plot_primitive_function <- function(plot, derivative, xmin, xmax, v0, x0, dx, color = "orangered", linetype = "solid", name = "") {
##   n <- (xmax - xmin) / dx
##   get_primitive_function(derivative, v0, x0, dx) %>%
##     plot_function(plot, ., xmin, xmax, n,
##       color = color, linetype = linetype, name = name
##     )
## }


# 求含精度参数的原函数
# 在一定范围内的定积分要满足一定的精度
get_optimal_primitive <- function(f, v0, x0, xmin, xmax, digits = 6) {
  tolerance <- 10^(-digits)
  dx <- 1
  approx <- definite_integration(f, xmin, xmax, dx)
  for (i in 0:(2 * digits)) {
    dx <- dx / 10
    next_approx <- definite_integration(f, xmin, xmax, dx)
    if (abs(next_approx - approx) < tolerance) {
      return(
        list(get_primitive_function(f, v0, x0, dx * 10), dx * 10)
      )
    } else {
      approx <- next_approx
    }
  }
  stop("Derivative did not converge.")
}