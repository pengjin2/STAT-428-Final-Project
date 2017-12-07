# Adapted from 
#   https://github.com/bickez/puppy-economics/blob/master/vasicek.R

# R script for simulationg bond short rates with the Vasicek model. It includes
# functions to calibrate the Vasicek model, run simulations and derive yield
# curves. 
#
# Three major sources used for this are below. 
#
# http://delta9hedge.blogspot.com/2013/05/simulation-of-vasicek-interest-rates-in.html
# http://www.sitmo.com/article/calibrating-the-ornstein-uhlenbeck-model/
# http://quantcorner.wordpress.com/2013/11/17/least-squares-and-maximum-likelihood-estimation-calibration-with-r/

# require("quantmod")
# require("zoo")
# require("xts")
# require("scales")
# require("tidyverse")


disc.clean.data <- function(ticker, src, dt, start.date, end.date) {
  # clean data pulled from getSymbol for following analysis
  # 
  # Args:
  #   ticker: ticker of an interest rate
  #   src: data source, e.g. "yahoo", "FRED"
  #   dt: time increment, can take a value from c("daily", "monthly", "quarterly", "annually")
  #   start.date: start date of historic data to query
  #   end.date: end date of historic data to query
  # 
  # Return: 
  #   a "xts" object with cleaned values ready for analysis
  # 
  # Require:
  #   quantmod
  #   xts
  
  time.slice <- paste(as.character(start.date), "/", as.character(end.date), sep = "")
  disc.data <- quantmod::getSymbols(Symbols = ticker, 
                                    src = src, 
                                    auto.assign = FALSE)[time.slice]
  disc.data <- na.omit(disc.data)/100  # FRED quotes 1.00% as 1.00 instead of 0.01 
  dt.period <- switch(dt, "daily" = "days", 
                      "monthly"   = "months", 
                      "quarterly" = "quarters", 
                      "annually"  = "years")
  disc.data <- xts::to.period(disc.data, period = dt.period, indexAt = "startof")
  # note that data comes in OHLC format from quantmod
  # we will only use adjusted closing price
  return(disc.data[, "disc.data.Close"])
}


disc.calibrate <- function(hist.data, model, dt) {
  # calibrate a model using data provided
  # 
  # Arg: 
  #   hist.data: (cleaned) historic data (xts object) for modeling use
  #   model: short rate model, can take value in c("vasicek")
  #   dt: The change in time between observations. Defaults to 1/252 because
  #       we assume generation of daily rates and there are 252 trading days 
  #       per year. 
  #
  # Returns: 
  #   a fitted model object of list type containing ... 
  
  dt.num <- switch(dt, 
                   "daily" = 1/252, 
                   "monthly" = 1/12, 
                   "quarterly" = 1/4, 
                   "annually" = 1)
  model.fit <- 
    switch(model, 
           "vasicek" = disc.calibrate.vasicek(hist.data, dt.num))
  return(list(param.est = list(kappa = model.fit[1], 
                               theta = model.fit[2], 
                               sigma = model.fit[3], 
                               r0    = model.fit[4]), 
              model = model, dt.char = dt, dt.num = dt.num))
}


disc.calibrate.vasicek <- function(hist.data, dt) {
  # Calibrates the vasicek model using the maximum likelihood estimator. Details
  # about the maximum likelihood extomator are at the link below. 
  #
  # http://www.sitmo.com/article/calibrating-the-ornstein-uhlenbeck-model/
  #
  # Args:
  #   hist.data: (cleaned) historic data (xts object) for modeling use
  #   dt: The change in time between observations. Defaults to 1/252 because
  #       we assume generation of daily rates and there are 252 trading days 
  #       per year. 
  #
  # Returns:
  #   A vector of the form c(kappa, theta, sigma, r0), where kappa is the mean
  #   reversion rate, theta is the long-term rate/mean, sigma is the volatility
  #   and r0 is the last observed rate.
  #
  # Requires:
  #   quantmod
  
  n <- length(hist.data)
  
  # do the calculations
  Sx <- sum(hist.data[1:(length(hist.data) - 1)])
  Sy <- sum(hist.data[2:length(hist.data)])
  Sxx <- as.numeric(crossprod(hist.data[1:(length(hist.data) - 1)], hist.data[1:(length(hist.data) - 1)]))
  Sxy <- as.numeric(crossprod(hist.data[1:(length(hist.data) - 1)], hist.data[2:length(hist.data)]))
  Syy <- as.numeric(crossprod(hist.data[2:length(hist.data)], hist.data[2:length(hist.data)]))
  
  theta  <- (Sy * Sxx - Sx * Sxy) / (n* (Sxx - Sxy) - (Sx^2 - Sx*Sy) )
  kappa <- -log((Sxy - theta * Sx - theta * Sy + n * theta^2) /   (Sxx - 2 * theta * Sx + n * theta^2)) / dt
  a <- exp(-kappa*dt)
  sigmah2 <- (Syy - 2 * a * Sxy + a^2 * Sxx - 2 * theta * (1-a) * (Sy - a * Sx) + n * theta^2 * (1 - a)^2)/n
  sigma <- sqrt(sigmah2 * 2 * kappa / (1 - a^2))
  
  r0 <- hist.data[length(hist.data)]
  
  return(c(kappa, theta, sigma, r0))
}


disc.predict <- function(model.fit, sim.length, n) {
  # predict future discount factor based onthe fitted model
  # 
  # Args: 
  #   model.fit: a fitted model object of type list caontaining ...
  #     - model: model type chosen that the data was calibrated on
  #     - param.est: parameter estimation of the model
  #     - dt.char: ime increment, can take a value from c("daily", "monthly", "quarterly", "annually")
  #     - dt.num: dt.char converted to numeric, can take values in c(1/252, 1/30, 1/4, 1)
  #   sim.length: time horizon in unit of years
  #   n: number of paths to simulate
  # 
  # Return: 
  #   a model prediction object containing ... 
  #     short rates, zero-coupon bond price, and derived yield curve
  
  if (model.fit$model == "vasicek") {
    
    # short rate
    short.rate.pred <- 
      disc.predict.vasicek(M = n, # ncol, number of pathes
                           N = sim.length / model.fit$dt.num, # nrow, length of each path
                           r0 = model.fit$param.est$r0, 
                           kappa = model.fit$param.est$kappa, 
                           theta = model.fit$param.est$theta, 
                           sigma = model.fit$param.est$sigma, 
                           dt = model.fit$dt.num)
    
    t.grid <- seq(from = model.fit$dt.num, to = sim.length, by = model.fit$dt.num)
    
    # theoretical confidence interval
    short.rate.exp   <- 
      model.fit$param.est$theta + (model.fit$param.est$r0 - model.fit$param.est$theta) * 
      exp(-model.fit$param.est$kappa*t.grid)
    short.rate.stdev <- 
      sqrt( model.fit$param.est$sigma^2 / (2*model.fit$param.est$kappa) * 
              (1 - exp(-2*model.fit$param.est$kappa*t.grid)))
    conf.int.high <- short.rate.exp + 3 * short.rate.stdev
    conf.int.low  <- short.rate.exp - 3 * short.rate.stdev
    
    # zero-coupon bond price
    ZCBondPrice.pred <- # a vector of predicted zero-coupon bond price
      disc.predict.vasicek.ZCBondPrice(
        r0 = model.fit$param.est$r0, 
        kappa = model.fit$param.est$kappa, 
        theta = model.fit$param.est$theta, 
        sigma = model.fit$param.est$sigma, 
        years = t.grid)
    
    # yield curve
    YieldCurve.pred <- -log(ZCBondPrice.pred)/t.grid
    
    return(list(short.rate.pred  = as.data.frame(short.rate.pred), 
                ZCBondPrice.pred = as.numeric(ZCBondPrice.pred), 
                YieldCurve.pred  = as.numeric(YieldCurve.pred),   
                conf.int = data.frame(high = conf.int.high, 
                                       low = conf.int.low),       # ... new predictions
                sim.length = sim.length, n = n,         # ... meta data for predictions
                model = model.fit$model,                    # copied from model.fit ...
                param.est = model.fit$param.est, 
                dt.char = model.fit$dt.char, 
                dt.num = model.fit$dt.num 
                ))
  }
}


disc.simulate.vasicek.helper <- function(r, kappa, theta, sigma, dt) {
  # Helper function that calculates the next rate based on the discretization
  # of the Varice model. 
  #
  # Args: 
  #   r: The interest rate used to generate the next interest rate.
  #   kappa: The mean reversion rate. 
  #   theta: The mean rate or long term rate. 
  #   sigma: Volatility. 
  #   dt: The change in time between observations. Defaults to 1/252 because
  #       we assume generation of daily rates and there are 252 trading days 
  #       per year. 
  #
  # Returns:
  #   A vector of simulated short rates. 
  
  term1 <- exp(-1 * kappa * dt)
  term2 <- (sigma^2) * (1 - term1^2) / (2*kappa)
  result <- r*term1 + theta*(1-term1) + sqrt(term2)*rnorm(n=1)
  return(result)
}


disc.simulate.vasicek <- function(N, r0, kappa, theta, sigma, dt) {
  # Generates a single short rate simulation using the Vasicek model.
  #
  # Args: 
  #   N: The number of points to generate in each simulation. For example, 
  #      the number of days when simulating daily rates.
  #   r0: The initial interest rate. 
  #   kappa: The mean reversion rate. 
  #   theta: The mean rate or long term rate. 
  #   sigma: Volatility. 
  #   dt: The change in time between observations. Defaults to 1/252 because
  #       we assume generation of daily rates and there are 252 trading days 
  #       per year. 
  #
  # Returns:
  #   A vector of simulated short rates. 
  
  short.rates <- rep(0, N)
  short.rates[1] <- r0
  for (i in 2:N) {
    short.rates[i] <- disc.simulate.vasicek.helper(short.rates[i - 1], kappa, theta, sigma, dt)
  }
  return(short.rates)
}


disc.predict.vasicek <- function(M, N, r0, kappa, theta, sigma, dt) {
  # Generates several short rate simulations using the Vasicek model.
  #
  # Args: 
  #   M: The number of simulations to run. 
  #   N: The number of points to generate in each simulation. For example, 
  #      the number of days when simulating daily rates.
  #   r0: The initial interest rate. 
  #   kappa: The mean reversion rate. 
  #   theta: The mean rate or long term rate. 
  #   sigma: Volatility. 
  #   dt: The change in time between observations. Defaults to 1/252 because
  #       we assume generation of daily rates and there are 252 trading days 
  #       per year. 
  #
  # Returns:
  #   An N row by M column data.frame of simulated short rates. 
  
  sim.mat <- matrix(nrow = N, ncol = M)
  for (i in 1:M) {
    sim.mat[, i] <- disc.simulate.vasicek(N, r0, kappa, theta, sigma, dt)
  }
  return(as.data.frame(sim.mat))
}


disc.predict.vasicek.ZCBondPrice <- function(r0, kappa, theta, sigma, years) {
  # Calculates th zero coupon bond price. 
  #
  # Args: 
  #   r0: The initial interest rate. 
  #   kappa: The mean reversion rate. 
  #   theta: The mean rate or long term rate. 
  #   sigma: Volatility. 
  #   years: The length or maturity of the bond.  
  #
  # Returns:
  #   A decimal price of the bond (i.e. 0.98 for 98). 
  
  b.vas <- (1-exp(-years*kappa)) / kappa
  a.vas <- (theta-sigma^2/(2*kappa^2))*(years-b.vas)+(sigma^2)/(4*kappa)*b.vas^2
  return(exp(-a.vas-b.vas*r0))
}


disc.predict.vasicek.YieldCurve <- function(r0, kappa, theta, sigma, t.grid) {
  # Produces a yield curve from the Vasicek model with maturities ranging 
  # from 1 year to max.maturity.  
  #
  # Args: 
  #   r0: The initial interest rate. 
  #   kappa: The mean reversion rate. 
  #   theta: The mean rate or long term rate. 
  #   sigma: Volatility. 
  #   t.grid: time gird to compute yield
  #
  # Returns:
  #   A vector of decimal price of the bond (i.e. 0.98 for 98) at each point on t.grid
  
  return(-log(disc.predict.vasicek.ZCBondPrice(r0, kappa, theta, sigma, t.grid))/t.grid)
}


disc.plot <- function(hist.data, model.pred, plot.type, conf, x.coord.interact = NULL, y.coord.interact = NULL) {
  # plot historic data and predictions
  # 
  # Args:
  #   hist.data: (cleaned) historic data (xts object) used for modeling
  #   model.pred: a returned object from disc.predict() of list type containing...
  #     - prediction data.frame: short rates (and its conf.int), zero-coupon bond price, and derived yield curve
  #     - model: model type chosen that the data was calibrated on
  #     - param.est: parameter estimation of the model
  #     - dt.char: ime increment, can take a value from c("daily", "monthly", "quarterly", "annually")
  #     - dt.num: dt.char converted to numeric, can take values in c(1/252, 1/30, 1/4, 1)
  #   plot.type: what prediction to plot, can be a vector in c("short-rate", "zero-coupon-bond-price", "yield-curve")
  #   conf: confidence level
  #   x.coord.interact & y.coord.interact: interactive condition
  #     - NULL stands for not adjusting the axis (default)
  #     - a vector of two elements will adjust the corresponding limit(s)
  # 
  # Return:
  #   None (but will output plot)
  # 
  # Require:
  #   zoo
  #   scales
  
  time.idx.hist <- zoo::index(hist.data)
  time.idx.pred <- time.idx.hist[length(time.idx.hist)] + 
    (0:(nrow(model.pred$short.rate.pred)-1)) * 
    switch(model.pred$dt.char, "daily" = 1, "monthly" = 31, "quarterly" = 95, "annually" = 370)
  hist.data.df <- data.frame(time.idx = time.idx.hist, hist.data = as.numeric(hist.data))
  plot.df.short.rate <- as.data.frame(
    t(apply(model.pred$short.rate.pred, MARGIN = 1, # apply on rows
            function(x) c(mean = mean(x), median = median(x), 
                          low  = quantile(x, probs = (1-conf)/2), 
                          high = quantile(x, probs = (1+conf)/2)))
    ) # transpose to make time march in row index
  )
  names(plot.df.short.rate) <- c("mean", "median", "low", "high")
  plot.df.short.rate$conf.int.high <- as.numeric(model.pred$conf.int[, "high"])
  plot.df.short.rate$conf.int.low  <- as.numeric(model.pred$conf.int[, "low"])
  confidence.interval <- paste("Empirical ", 100 * conf, "%", sep = "")
  
  if ("short-rate" %in% plot.type) {
    short.rate.plot <- 
      plot.df.short.rate %>% mutate(time.idx = time.idx.pred) %>% 
      ggplot(aes(x = time.idx)) + 
      geom_line(aes(x = time.idx, y = hist.data, color = "historic data"), data = hist.data.df) + 
      geom_line(aes(y = mean, color = "mean")) +
      geom_line(aes(y = median, color = "median")) +
      geom_ribbon(aes(ymin = conf.int.low, ymax = conf.int.high, 
                      fill = "Theoretical 3 Std.Dev."), 
                  alpha = 0.3) + 
      geom_ribbon(aes(ymin = low, ymax = high, 
                      fill = confidence.interval), 
                  alpha = 0.3) + 
      scale_x_date(labels = scales::date_format(switch(model.pred$dt.char, 
                                                       "daily" = "%Y-%m-%d", 
                                                       "monthly" = "%b %Y", 
                                                       "quarterly" = "%b %Y", 
                                                       "annually" = "%Y")), 
                   date_breaks = switch(model.pred$dt.char, 
                                        "daily" = "3 months", 
                                        "monthly" = "3 months", 
                                        "quarterly" = "1 year", 
                                        "annually" = "1 year"), 
                   limits = switch(as.character(is.null(x.coord.interact[1])), 
                                   "TRUE" = as.null(), 
                                   "FALSE" = c(as.Date(x.coord.interact[1], origin = "1970-01-01"), 
                                               as.Date(x.coord.interact[2], origin = "1970-01-01")))) + 
      scale_y_continuous(limits = y.coord.interact) + 
      xlab("Time") + ylab("Short-Term Discount Rate") + theme_bw() + 
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1), 
            axis.line = element_line(colour = "black"), panel.border = element_blank())
    print(short.rate.plot)
  }
  
  if ("zero-coupon-bond-price" %in% plot.type) {
    ZCBondPrice.plot <- 
      data.frame(time.idx = time.idx.pred, zero.coupon.bond.price = model.pred$ZCBondPrice.pred) %>% 
      ggplot(aes(x = time.idx, y = zero.coupon.bond.price)) + geom_line() + 
      scale_x_date(labels = scales::date_format(switch(model.pred$dt.char, 
                                                       "daily" = "%Y-%m-%d", 
                                                       "monthly" = "%b %Y", 
                                                       "quarterly" = "%b %Y", 
                                                       "annually" = "%Y")), 
                   date_breaks = switch(model.pred$dt.char, 
                                        "daily" = "3 months", 
                                        "monthly" = "3 months", 
                                        "quarterly" = "1 year", 
                                        "annually" = "1 year"), 
                   limits = switch(as.character(is.null(x.coord.interact[1])), 
                                   "TRUE" = as.null(), 
                                   "FALSE" = c(as.Date(x.coord.interact[1], origin = "1970-01-01"), 
                                               as.Date(x.coord.interact[2], origin = "1970-01-01")))) + 
      scale_y_continuous(limits = y.coord.interact) + 
      xlab("Time") + ylab("Zero-Coupon Bond Price") + theme_bw() + 
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1), 
            axis.line = element_line(colour = "black"), panel.border = element_blank())
    print(ZCBondPrice.plot)
  }
  
  if ("yield-curve" %in% plot.type) {
    YieldCurve.plot <- 
      data.frame(time.idx = time.idx.pred, yield.curve = model.pred$YieldCurve.pred) %>% 
      ggplot(aes(x = time.idx, y = yield.curve)) + geom_line() + 
      scale_x_date(labels = scales::date_format(switch(model.pred$dt.char, 
                                                       "daily" = "%Y-%m-%d", 
                                                       "monthly" = "%b %Y", 
                                                       "quarterly" = "%b %Y", 
                                                       "annually" = "%Y")), 
                   date_breaks = switch(model.pred$dt.char, 
                                        "daily" = "3 months", 
                                        "monthly" = "3 months", 
                                        "quarterly" = "1 year", 
                                        "annually" = "1 year"), 
                   limits = switch(as.character(is.null(x.coord.interact[1])), 
                                   "TRUE" = as.null(), 
                                   "FALSE" = c(as.Date(x.coord.interact[1], origin = "1970-01-01"), 
                                               as.Date(x.coord.interact[2], origin = "1970-01-01")))) + 
      scale_y_continuous(limits = y.coord.interact) + 
      xlab("Time") + ylab("Yield") + theme_bw() + 
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1), 
            axis.line = element_line(colour = "black"), panel.border = element_blank())
    print(YieldCurve.plot)
  }
  
}

#### An Extra Helper Function for Shiny App
####  short rate portion of disc.plot()
disc.plot.short.rate.helper <- function(hist.data, model.pred, conf, x.coord.interact = NULL, y.coord.interact = NULL) {
  # plot historic data and predictions
  # 
  # Args:
  #   hist.data: (cleaned) historic data (xts object) used for modeling
  #   model.pred: a returned object from disc.predict() of list type containing...
  #     - prediction data.frame: short rates (and its conf.int), zero-coupon bond price, and derived yield curve
  #     - model: model type chosen that the data was calibrated on
  #     - param.est: parameter estimation of the model
  #     - dt.char: ime increment, can take a value from c("daily", "monthly", "quarterly", "annually")
  #     - dt.num: dt.char converted to numeric, can take values in c(1/252, 1/30, 1/4, 1)
  #   conf: confidence level
  #   x.coord.interact & y.coord.interact: interactive condition
  #     - NULL stands for not adjusting the axis (default)
  #     - a vector of two elements will adjust the corresponding limit(s)
  # 
  # Return:
  #   None (but will output plot)
  # 
  # Require:
  #   zoo
  #   scales
  
  time.idx.hist <- zoo::index(hist.data)
  time.idx.pred <- time.idx.hist[length(time.idx.hist)] + 
    (0:(nrow(model.pred$short.rate.pred)-1)) * 
    switch(model.pred$dt.char, "daily" = 1, "monthly" = 31, "quarterly" = 95, "annually" = 370)
  hist.data.df <- data.frame(time.idx = time.idx.hist, hist.data = as.numeric(hist.data))
  plot.df.short.rate <- as.data.frame(
    t(apply(model.pred$short.rate.pred, MARGIN = 1, # apply on rows
            function(x) c(mean = mean(x), median = median(x), 
                          low  = quantile(x, probs = (1-conf)/2), 
                          high = quantile(x, probs = (1+conf)/2)))
    ) # transpose to make time march in row index
  )
  names(plot.df.short.rate) <- c("mean", "median", "low", "high")
  plot.df.short.rate$conf.int.high <- as.numeric(model.pred$conf.int[, "high"])
  plot.df.short.rate$conf.int.low  <- as.numeric(model.pred$conf.int[, "low"])
  confidence.interval <- paste("Empirical ", 100 * conf, "%", sep = "")

  plot.df.short.rate %>% mutate(time.idx = time.idx.pred) %>% 
  ggplot(aes(x = time.idx)) + 
  geom_line(aes(x = time.idx, y = hist.data, color = "historic data"), data = hist.data.df) + 
  geom_line(aes(y = mean, color = "mean")) +
  geom_line(aes(y = median, color = "median")) +
  geom_ribbon(aes(ymin = conf.int.low, ymax = conf.int.high, 
                  fill = "Theoretical 3 Std.Dev."), 
              alpha = 0.3) + 
  geom_ribbon(aes(ymin = low, ymax = high, 
                  fill = confidence.interval), 
              alpha = 0.3) + 
  scale_x_date(labels = scales::date_format(switch(model.pred$dt.char, 
                                                   "daily" = "%Y-%m-%d", 
                                                   "monthly" = "%b %Y", 
                                                   "quarterly" = "%b %Y", 
                                                   "annually" = "%Y")), 
               date_breaks = switch(model.pred$dt.char, 
                                    "daily" = "3 months", 
                                    "monthly" = "3 months", 
                                    "quarterly" = "1 year", 
                                    "annually" = "1 year"), 
               limits = switch(as.character(is.null(x.coord.interact[1])), 
                               "TRUE" = as.null(), 
                               "FALSE" = c(as.Date(x.coord.interact[1], origin = "1970-01-01"), 
                                           as.Date(x.coord.interact[2], origin = "1970-01-01")))) + 
  scale_y_continuous(limits = y.coord.interact) + 
  xlab("Time") + ylab("Short-Term Discount Rate") + theme_bw() + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.line = element_line(colour = "black"), panel.border = element_blank())
  
}
