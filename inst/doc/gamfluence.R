## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6.5,
                      fig.height = 6.5, out.width = "85%", fig.align = "center")

## ----data, message = FALSE----------------------------------------------------
library(mgcv)
library(gamfluence)
str(coral_trout)
table(coral_trout$REGION, coral_trout$NTR)   # surveys by region and zone

## ----model--------------------------------------------------------------------
m <- gam(count ~ REGION * NTR + EXPOSURE + depth +
           s(YEAR, by = REGION, k = 5) +
           s(rugosity, k = 5) + s(LHC, k = 5) + s(kd490, k = 5) +
           s(maxDHW, k = 5) + s(Cyclone, k = 5) + s(SITE, bs = "re"),
         family = nb(), data = coral_trout, method = "REML")
m$family$getTheta(TRUE)       # theta, estimated
round(m$sp, 3)                # the eight estimated smoothing parameters

## ----run, eval = FALSE--------------------------------------------------------
#  infl <- gam_influence(m, cluster = "SITE")

## ----load---------------------------------------------------------------------
infl <- readRDS(system.file("extdata", "coral_trout_influence.rds",
                            package = "gamfluence"))

## ----two-sites----------------------------------------------------------------
two <- gam_influence(m, cluster = "SITE", which = c("OE4", "B2"), progress = FALSE)
two$sites[, c("id", "fixed", "added", "total")]
infl$sites[infl$sites$id %in% c("OE4", "B2"), c("id", "fixed", "added", "total")]

## ----print--------------------------------------------------------------------
infl

## ----top----------------------------------------------------------------------
top_units(infl, by = "fixed", n = 5)[, c("id", "fixed", "added", "total", "status")]

## ----numbers, include = FALSE-------------------------------------------------
s <- infl$sites
h <- infl$hyper
palm <- h[h$parameter == "s(YEAR):REGIONPalm", ]
straight <- palm$id[palm$ratio > 100]
st <- s[s$id %in% straight, ]
others <- s[!s$id %in% straight, ]

## ----plot, fig.alt = "Scatter plot of each site's fixed change against its added change. Most sites sit near the dashed diagonal below 0.03; seven sites sit far above it, near 0.06 on the added axis."----
plot(infl, n = 8)

## ----oe4----------------------------------------------------------------------
h[h$id == "OE4", c("parameter", "type", "full", "deleted", "ratio")]

## ----profile------------------------------------------------------------------
grid <- c(0.01, 0.1, 1, 10, 1e3, 1e5)
reml <- sapply(grid, function(l) {
  sp <- rep(-1, length(m$sp))
  sp[1] <- l                                  # s(YEAR):REGIONPalm
  as.numeric(gam(formula(m), family = nb(), data = coral_trout,
                 method = "REML", sp = sp)$gcv.ubre)
})
data.frame(lambda = grid, REML_minus_best = round(reml - min(reml), 3))

## ----optima-------------------------------------------------------------------
table(s$status)
x <- s[s$id %in% c("OE4", "W2"), ]
writeLines(paste0(x$id, ": ", x$note))

## ----surveys, eval = FALSE----------------------------------------------------
#  by_survey <- gam_influence(m, cluster = paste(coral_trout$REGION, coral_trout$YEAR))
#  plot(by_survey)

