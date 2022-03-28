## Chapter08.R

# config
source("./config/config.R")

source("./src/plot-element.R")
source("./src/function-plotting.R")


# config
title <- ""
xtitle <- ""
ytitle <- ""
xmin <- 0 # x domain 的最小值
xmax <- 10 # x domain 的最大值
n <- 1000 # x domain 值的个数


################################################
## 求导函数
################################################

volume <- function(t) {
  (t - 4)^3 / 64 + 3.3
}

# plotting
p <- create_canvas_2d(xtitle, ytitle, showlegend = TRUE) %>%
  layout(
    title = list(text = title)
  ) %>%
  plot_function(volume, xmin, xmax, n, name = "primitive function", showlegend = TRUE)
p

p %>% plot_secant(volume, xmin, xmax, n, 4, 9)
p %>% plot_derivative_array(volume, xmin, xmax, n, name = "derivative array", showlegend = TRUE)
p %>% plot_derivative_function(volume, xmin, xmax, n, name = "derivative function", showlegend = TRUE)



################################################
## 求原函数
################################################


flow_rate <- function(t) {
  3 * (t - 4)^2 / 64
}

p <- create_canvas_2d(xtitle, ytitle, showlegend = TRUE) %>%
  layout(
    title = list(text = title)
  ) %>%
  plot_function(volume, xmin, xmax, n, name = "primitive function")
p


p %>%
  plot_function(
    f = get_primitive_function(flow_rate, volume(xmin), xmin, dx = 1),
    xmin, xmax, n = (xmax - xmin) / 1, color = "orangered",
    name = "Riemann Sum approximate"
  )


optimal_primitive <- get_optimal_primitive(
  flow_rate, volume(xmin), xmin,
  xmin, xmax,
  digits = 4
)[[1]]
dx <- get_optimal_primitive(
  flow_rate, volume(xmin), xmin,
  xmin, xmax,
  digits = 4
)[[2]]
p %>%
  plot_function(
    f = optimal_primitive,
    xmin, xmax, n = (xmax - xmin) / dx, color = "orangered",
    name = "optimal primitive"
  )

# 黎曼和近似值与精确值的差距
volume(xmax) - get_primitive_function(
  flow_rate, volume(xmin), xmin,
  dx = 1
)(xmax)
volume(xmax) - get_primitive_function(
  flow_rate, volume(xmin), xmin,
  dx = 0.1
)(xmax)
volume(xmax) - get_primitive_function(
  flow_rate, volume(xmin), xmin,
  dx = 0.01
)(xmax)
volume(xmax) - get_optimal_primitive(
  flow_rate, volume(xmin), xmin, xmin, xmax,
  digits = 6
)[[1]](xmax)
get_optimal_primitive(
  flow_rate, volume(xmin), xmin, xmin, xmax,
  digits = 6
)[[2]]

