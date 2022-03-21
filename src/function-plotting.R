## function-plotting.R


# 绘制任意函数
plot_function <- function(plot, f, xmin, xmax, n,
                          color = "royalblue", linetype = "solid",
                          name = "", showlegend = TRUE) {
  xs <- seq(xmin, xmax, length.out = n + 1)
  ys <- xs %>% map_dbl(f)
  points <- data.table(x = xs, y = ys)
  plot %>%
    draw_line(
      points,
      color = color, linetype = linetype,
      name = name, showlegend = showlegend
    )
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
                        color = "orangered", linetype = "dash", name = "") {
  secant <- data.table(x = c(x1, x2), y = c(f(x1), f(x2)))
  secant_line(f, x1, x2) %>%
    plot_function(plot, ., xmin, xmax, n,
      color = color, linetype = linetype, name = name, showlegend = FALSE
    ) %>%
    draw_line(secant, color = color)
}


# 导数数组
derivative_array <- function(f, xmin, xmax, n) {
  xs <- seq(xmin, xmax, length.out = n + 1)
  delta_x <- xs[2] - xs[1]

  xs[1:n] %>%
    map_dbl(function(x) {
      secant_slope(f, x, x + delta_x)
    }) %>%
    data.table(x = xs[1:n] + delta_x / 2, y = .)
}

plot_derivative_array <- function(plot, f, xmin, xmax, n,
                                  color = "orangered",
                                  name = "", showlegend = TRUE) {
  derivative_array(f, xmin, xmax, n) %>%
    draw_line(
      plot,
      points = ., color = color,
      name = name, showlegend = showlegend
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

# 获取导函数
get_derivative_function <- function(f) {
  function(x) {
    derivative(f, x)
  }
}

plot_derivative_function <- function(plot, primitive, xmin, xmax, n,
                                     color = "orangered",
                                     linetype = "solid", name = "") {
  derivative_function <- get_derivative_function(primitive)
  plot_function(plot, derivative_function, xmin, xmax, n,
    color = color, linetype = linetype, name = name
  )
}