# require("quantmod")
# require("zoo")
# require("xts")
# require("fitdistrplus")
# require("scales")
# require("tidyverse")


inv.clean.data <- function(ticker, src, dt, start_date, end_date) {
  # clean data pulled from getSymbol for following analysis
  # 
  # Args:
  #   ticker: ticker of an investment
  #   src: data source, e.g. "yahoo", "FRED"
  #   dt: time increment, can take a value from c("daily", "monthly", "quarterly", "annually")
  #   start_date: start date of historic data to query
  #   end_date: end date of historic data to query
  # 
  # Return: 
  #   a "xts" object with cleaned values ready for analysis
  # 
  # Require:
  #   quantmod
  #   xts
  
  inv.data <- quantmod::getSymbols(Symbols = ticker, 
                                   src = src, 
                                   from = start_date, 
                                   to = end_date, 
                                   auto.assign = FALSE)
  dt.period <- switch(dt, "daily" = "days", 
                      "monthly"   = "months", 
                      "quarterly" = "quarters", 
                      "annually"  = "years")
  inv.data <- xts::to.period(inv.data, period = dt.period, indexAt = "startof")
  # note that data comes in OHLC format from quantmod
  # we will only use adjusted closing price
  return(inv.data[,4])
}


inv.calibrate <- function (hist.data, model) {
  # calibrate a model using data provided
  # 
  # Args: 
  #   hist.data: (cleaned) historic data (xts object) for modeling use
  #   model: equity return model, can take a value in c("GBM)
  # 
  # Return: 
  #   a calibration object of type list containing ...
  #     - param.est: a vector of estimated parameters
  # 
  # Require: 
  #   fitdistrplus
  
  if(model=="GBM"){
    return <- numeric(nrow(hist.data))
    for(i in 1:nrow(hist.data)-1){
      return[i] <- as.numeric(hist.data[i+1])/as.numeric(hist.data[i])
    }
    return <- return[-which(return == 0)]
    param <- as.numeric(fitdistrplus::fitdist(return, 
                                              distr = "lnorm", 
                                              method = "mle")$estimate)
    return(param)
  }
}


inv.predict <- function(hist.data, model, param, sim.length, n, dt) { 
  # predict future equity returns based onthe fitted model
  # 
  # Args: 
  #   hist.data: (cleaned) historic data (xts object) used for modeling
  #   model: model type
  #   param: model estimates
  #   sim.length: time horizon in unit of years
  #   n: number of paths to simulate
  #   dt: time increment
  # 
  # Return: 
  #   a dataframe containing all the predicted stocks paths (ncol = n)
  
  dt.literal <- dt
  dt <- switch(dt, 
               "daily" = 1/252, 
               "monthly" = 1/12, 
               "quarterly" = 1/4, 
               "annually" = 1)
  m <- sim.length / dt
  if (model == "GBM"){
    s0 <- as.numeric(tail(hist.data,1))
    prediction_matrix <- matrix(0,(m+1),n)
    prediction_matrix[1,] <- s0
    for(j in 1:n){
      for(i in 2:(m+1)){
        prediction_matrix[i,j] <- 
          #prediction_matrix[i-1,j] * exp(((param[1]-param[2]**2)/2)*dt+param[2]*sqrt(dt)*rnorm(1))
          prediction_matrix[i-1,j] * exp(param[1]*dt+param[2]*rnorm(n = 1)*sqrt(dt))
      }
    }
    return(list(pred.df = as.data.frame(prediction_matrix), dt = dt.literal))
  }
}


inv.plot <- function(hist.data, model.pred, conf, x.coord.interact = NULL, y.coord.interact = NULL) { 
  # plot historic data and predictions
  # 
  # Args:
  #   hist.data: (cleaned) historic data (xts object) used for modeling
  #   model.pred: a returned object from inv.predict() of list type containing...
  #     - pred.df: prediction data in data.frame format, each column is a path
  #     - dt: time increment
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
    0:(nrow(model.pred$pred.df)-1) * switch(model.pred$dt, 
                                            "daily" = 1, 
                                            "monthly" = 31, 
                                            "quarterly" = 95, 
                                            "annually" = 370)
  hist.data.df <- data.frame(time.idx = time.idx.hist, hist.data = as.numeric(hist.data))
  plot.df <- as.data.frame(
    t(apply(model.pred$pred.df, MARGIN = 1, # apply on rows
            function(x) c(mean = mean(x), median = median(x), 
                          low  = quantile(x, probs = (1-conf)/2), 
                          high = quantile(x, probs = (1+conf)/2)))
      ) # transpose to make time march in row index
    )
  names(plot.df) <- c("mean", "median", "low", "high")
  confidence.interval <- paste(100 * conf, "%", sep = "")
  plot.df %>% mutate(time.idx = time.idx.pred) %>% 
    ggplot(aes(x = time.idx)) + 
    geom_line(aes(x = time.idx, y = hist.data, color = "historic data"), data = hist.data.df) + 
    geom_line(aes(y = mean, color = "mean")) +
    geom_line(aes(y = median, color = "median")) +
    geom_ribbon(aes(ymin = low, ymax = high, 
                    fill = confidence.interval), 
                alpha = 0.3) + 
    scale_x_date(labels = scales::date_format(switch(model.pred$dt, 
                                 "daily" = "%Y-%m-%d", 
                                 "monthly" = "%b %Y", 
                                 "quarterly" = "%b %Y", 
                                 "annually" = "%Y")), 
                 date_breaks = switch(model.pred$dt, 
                                 "daily" = "3 months", 
                                 "monthly" = "3 months", 
                                 "quarterly" = "1 year", 
                                 "annually" = "1 year"), 
                 limits = switch(as.character(is.null(x.coord.interact[1])), 
                                 "TRUE" = as.null(), 
                                 "FALSE" = c(as.Date(x.coord.interact[1], origin = "1970-01-01"), 
                                             as.Date(x.coord.interact[2], origin = "1970-01-01")))) + 
    scale_y_continuous(limits = y.coord.interact) + 
    xlab("Time") + ylab("Equity Investment Value") + theme_bw() + 
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.line = element_line(colour = "black"), panel.border = element_blank())
}

