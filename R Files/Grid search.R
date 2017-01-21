library(NMOF)
library(forecast)
library(TTR)

#This is how the gridSearch works via the help file
#testFun <- function(x) x[1L] + x[2L]^2
#sol <- gridSearch(fun = testFun, levels = list(1:2, c(2, 3, 5)))
#sol$minfun
#sol$minlevels

iPhone <- read.csv(file.choose())
iPhone_ts <- ts(iPhone$Sales, start=2007, frequency=4)

test2fun <- function(x) HoltWinters(iPhone_ts, x[1L], x[2L], x[3L])$SSE
sol2 <- gridSearch(fun=test2fun, levels = list(
  c(0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40),
  c(0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40),
  c(0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40))
)
sol2$minfun
sol2$minlevels

