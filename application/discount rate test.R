source("discount rate.R")

################################################################################
### TESTING
################################################################################

data.DGS10 <- disc.clean.data(ticker = "DGS10", 
                              src = "FRED", 
                              dt = "monthly", 
                              start.date = "2013-01-01", 
                              end.date = Sys.Date())

vasicek.model <- disc.calibrate(hist.data = data.DGS10, 
                            model = "vasicek", 
                            dt = "monthly") 

vasicek.model.pred <- disc.predict(model.fit = vasicek.model, 
                           sim.length = 10, n = 100)

disc.plot(hist.data = data.DGS10, 
          model.pred = vasicek.model.pred, 
          plot.type = c("short-rate", "zero-coupon-bond-price", "yield-curve"), 
          conf = 0.95, 
          x.coord.interact = NULL, 
          y.coord.interact = NULL)

disc.plot(hist.data = data.DGS10, 
          model.pred = vasicek.model.pred, 
          plot.type = c("short-rate", "zero-coupon-bond-price", "yield-curve"), 
          conf = 0.95,
          x.coord.interact = c("2015-06-06", "2020-01-01"),
          y.coord.interact = c(0.01, 0.05)) # note that the conf int may disappear if this limit is too tight

# ## define model parameters and calibrate
# years <- 4
# N <- years * 12 # each year consists of 252 days
# t <- (1:N)/12 # for plotting purposes
# 
# # some old code removed ... 
# 
# kappa <- calibration$param.est$kappa
# theta <- calibration$param.est$theta
# sigma <- calibration$param.est$sigma
# r0    <- calibration$param.est$r0
# 
# set.seed(666)
# 
# test <- disc.simulate.vasicek(N, r0, kappa, theta, sigma)
# plot(t, test, type = 'l')
# 
# # test with several (M = 20) simultions
# M <- 20
# test.mat <- disc.predict.vasicek(M, N, r0, kappa, theta, sigma)
# 
# # plot the paths
# plot(t, test.mat[, 1], type = 'l', main = 'Short Rates', ylab = 'rt', 
#      ylim = c(0, max(test.mat)), col = 1)
# for (count in 2:ncol(test.mat)) {
#   lines(t, test.mat[, count], col = count)
# }
# # plot the expected rate and +- 2 standard deviations (theoretical)
# expected <- theta + (r0 - theta)*exp(-kappa*t)
# stdev <- sqrt( sigma^2 / (2*kappa)*(1 - exp(-2*kappa*t)))
# lines(t, expected, lty=2) 
# lines(t, expected + 2*stdev, lty=2) 
# lines(t, expected - 2*stdev, lty=2)
# 
# # price the zero coupon bonds 
# disc.predict.vasicek.ZCBondPrice(r0, kappa, theta, sigma, years) 
# 
# # derive a yield curve 
# # (can do this for several values of r0 to get several curves)
# yields <- disc.predict.vasicek.YieldCurve(r0, kappa, theta, sigma, 10)
# plot(1:10, yields, xlab = 'Maturity', type = 'l', ylab = 'Yield', main = 'Yield Curve')