## Chapter08.R

library(tidyverse)
library(magrittr)
library(data.table)
library(plotly)
library(htmlwidgets)

source("./src/plot-element.R")
source("./src/function-plotting.R")


# config
title <- ""
xtitle <- "Time(hour)"
ytitle <- "Volume(bbl)"
xmin <- -2 # x domain 的最小值
xmax <- 10 # x domain 的最大值
n <- 1000 # x domain 值的个数

volume <- function(t) {
  (t - 4)^3 / 64 + 3.3
}

# plotting
p <- create_canvas_2d(xtitle, ytitle, showlegend = TRUE) %>%
  layout(
    title = list(text = title)
  ) %>%
  plot_function(volume, xmin, xmax, n, name = "primitive function")
p

p %>% plot_secant(volume, xmin, xmax, n, 4, 9)
p %>% plot_derivative_array(volume, xmin, xmax, n, name = "derivative array")
p %>% plot_derivative_function(volume, xmin, xmax, n, name = "derivative function")