source("investment.R")

################################################################################
### TESTING
################################################################################

data.AAPL <- inv.clean.data(ticker = "SPY", src = "yahoo", dt = "monthly", start_date = "2013-01-01", end_date = Sys.Date())
param.est <- inv.calibrate(hist.data = data.AAPL, model = "GBM")
model.predict <- inv.predict(hist.data = data.AAPL, model = "GBM", param = param.est,
                             sim.length =
                               as.numeric(substr("2027-12-06", start = 1, stop = 4)) -
                               as.numeric(substr("2017-12-06", start = 1, stop = 4)),
                             n = 100, dt = "monthly")
inv.plot(hist.data = data.AAPL, model.pred = model.predict, conf = 0.95, x.coord.interact = NULL, y.coord.interact = NULL)
inv.plot(hist.data = data.AAPL, model.pred = model.predict, conf = 0.95,
         x.coord.interact = c("2015-06-06", "2020-01-01"), y.coord.interact = c(200, 300))

