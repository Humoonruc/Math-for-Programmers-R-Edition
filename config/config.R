# import modules
library(tidyverse)
library(data.table)
library(magrittr)
library(kableExtra) # 表格美化
library(plotly) # 绘图
library(htmlwidgets)
library(downloadthis) # 提供资源下载的html部件
library(zeallot) # 解构赋值
library(ivreg) # 做 2SLS regression 很方便
library(numDeriv) # Package for computing f'(x)
library(rootSolve) # 求解非线性方程（组）和最优化