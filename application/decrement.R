source("Random Number Generator.R")
# require("tidyverse")
# setwd("~/Downloads/STAT-428-Final-Project/")


decrement.clean.data <- function(path.to.file, source, model) {
  # read in CSV file from upload and clean data
  # 
  # Args:
  #   path.to.file: path to the file to read in 
  #     * must be comma seperated, must have column headers {gender, age, lx, dx}
  #     - gender must be character
  #     - age, lx, and dx must be numeric
  #   source: source of decrement, can be one of c("mortality", "lapse", "annuitization")
  #   model: model used for calibration for the source of decrement
  #
  # Return:
  #   return a cleaned data.frame
  
  if (source == "mortality") {
    if (model == "life-table") {
      life.table <- 
        as.data.frame(read.table(path.to.file, header = TRUE, sep = ",")) %>% 
        select(gender, age, lx, dx) %>% 
        mutate(gender = as.character(gender), age.floor = as.integer(age), 
               lx = as.integer(lx), dx = as.integer(dx)) %>% 
        select(-age)
      return(life.table)
    }
  } 
}


decrement.calibrate <- function(hist.data, source, model, model.param) {
  # calibrate a model for a source of decrement using the data and parameters
  # 
  # Args:
  #   hist.data: (cleaned for source and model) historic data used for calibration
  #   source: source of decrement, can be one of c("mortality", "lapse", "annuitization")
  #   model: model used for calibration for the source of decrement
  #   model.param: a list of parameters that the model needs
  #     - source = "mortality", model = "life-table":
  #         model.param = list(dt)
  #     - source = any, model = "const":
  #         model.param = list(const)
  #
  # Return:
  #   return parameter estimations and (if available) accuracy of estimation
  #   - source = "mortality", model = "life-table": 
  #       return an inverse CDF function of age-at-death
  #   - source = "lapse", model = "schedule": 
  #       return an inverse CDF function of time-of-lapse
  #   - source = "annuitization", model = "schedule": 
  #       return an inverse CDF function of time-of-annuitization
  
  if (source == "mortality") {
    if (model == "life-table") {
      life.table.funcs <- 
        decrement.calibrate.mortality.LifeTable(hist.data, model.param$assumption)
      return(list(tqx = life.table.funcs$tqx, 
                  tqx.inv = life.table.funcs$tqx.inv, 
                  source = source, model = model, 
                  model.param = model.param, hist.data = hist.data))
    }
  } else if (source == "lapse") {
    if (model == "const") {
      return(list(const = model.param$const, source = source, model = model))
    }
  } else if (source == "annuitization") {
    if (model == "const") {
      return(list(const = model.param$const, source = source, model = model))
    } 
  }
}


decrement.calibrate.mortality.LifeTable <- function(hist.data, assumption) {
  # helper function to create an inverse CDF out of a life table
  # 
  # Args: 
  #   hist.data: a data.frame containing four columns: gender, age.floor, lx, dx
  #   assumption: fractional age assumptions
  # 
  # Return: 
  #   inverse CDF function
  
  if (assumption == "UDD") {
    
    # remaining lives
    get.lx <- function(x, sex) {
      lx.high <- sapply(ceiling(x), 
                        function(x) as.numeric(unlist(
                          hist.data %>% 
                            filter(gender == sex & age.floor == x) %>% 
                            select(lx))))
      lx.low <- sapply(floor(x), 
                       function(x) as.numeric(unlist(
                         hist.data %>% 
                           filter(gender == sex & age.floor == x) %>% 
                           select(lx))))
      x.decimal <- x - floor(x)
      return( (1-x.decimal) * lx.low + x.decimal * lx.high )
    }
    
    
    # CDF 
    tqx <- function(t, x, sex) {
      lx <- get.lx(x = x, sex = sex)
      lxaddt <- get.lx(x = x + t, sex = sex)
      return( 1 - lxaddt / lx )
    }
    
    
    # inverse CDF
    tqx.inv.helper <- function(p, x, sex) {
      
      if (p == 1) {
        return(as.integer(
          hist.data %>% filter(gender == sex) %>% 
            summarise(max.age.floor = max(age.floor))))
      } else {
        lxaddt <- get.lx(x = x, sex = sex) * (1 - p)
        hist.data.filtered <- 
          hist.data %>% filter(gender == sex) %>% select(age.floor, lx)
        high.idx <- which(hist.data.filtered[,"lx"] <= lxaddt)[1]
        frac <- (lxaddt - hist.data.filtered[high.idx-1, "lx"]) / 
          (hist.data.filtered[high.idx, "lx"] - hist.data.filtered[high.idx-1, "lx"])
        return(hist.data.filtered[high.idx-1, "age.floor"] + frac - x)
      }
      # # generate a grid
      # max.age <- as.integer(hist.data %>% filter(gender == sex) %>% 
      #                         summarise(max.age.floor = max(age.floor)))
      # # upper bound need to be tweaked
      # max.death.prob <- tqx(t = max.age - x, x = x, sex = sex)
      # if (p >= max.death.prob) {
      #   return(max.age - x + 1)
      # } else {
      #   t.interval <- seq(from = 0, to = max.age - x, by = 1)
      #   tqx.t.interval <- tqx(t = t.interval, x = x, sex = sex)
      #   t.high <- as.integer(which(tqx.t.interval > p)[1]) - 1
      #   # find inverse
      #   return(as.numeric(uniroot(function(t) tqx(t, x = x, sex = sex) - p,
      #                             interval = c(t.high-1, t.high),
      #                             extendInt = "yes",
      #                             check.conv = TRUE,
      #                             tol = 0.001)$root))
      # }
    }
    
    tqx.inv <- function(p, x, sex) {
      sapply(p, function(p) tqx.inv.helper(p, x = x, sex = sex))
    }
    
    return(list(tqx = tqx, tqx.inv = tqx.inv))
  }
}


decrement.predict <- function(model.fit, t.now, dt, t.end, n, new.data){
  # predict number of decrements 
  # 
  # Args: 
  #   model.fit: fitted model object, 
  #     - tqx
  #     - tqx.inv
  #     - source: source of decrement, can be one of c("mortality", "lapse", "annuitization")
  #     - model: model used for calibration for the source of decrement
  #     - model.param: a list of parameters that the model needs
  #       + source = "mortality", model = "life-table":
  #           model.param = list(dt)
  #       + source = any, model = "const":
  #           model.param = list(const)
  #   t.now: current time (year number, e.g. 2017)
  #   dt: time increment, can take value in c("daily", "monthly", "quarterly", "annually")
  #   t.end: further horizon to predict
  #   n: number of simulations
  #   new.data: new data to be predicted on, optional
  #     - for mortality: a data.frame containing three columns (count, gender, start.age) and one row
  # 
  # Return:
  #    a prediciton matrix (of data.frame type), nrow = (t.end - t.now) / dt, ncol = n
  
  t.now <- lubridate::year(t.now)
  t.end <- lubridate::year(t.end)
  # TODO: this cannot handle fractional years
  
  dt.char <- dt 
  dt <- switch(dt, "daily" = 1/365, 
               "monthly"   = 1/12, 
               "quarterly" = 1/4, 
               "annually"  = 1)
  
  pred.horizon <- seq(from = t.now, to = t.end, by = dt)
  if (model.fit$source == "mortality") {
    if (model.fit$model == "life-table") {
      N <- new.data[1, "count"]
      gender <- new.data[1, "gender"]
      start.age <- new.data[1, "start.age"]
      x <- STAT428.random.number.1D(n = n*N,
                                    method = "inverseCDF",
                                    method.param =
                                      list(
                                        invCDF =
                                          compiler::cmpfun(function(p) 
                                            model.fit$tqx.inv(p, start.age, gender))))
      # u <- runif(n*N)
      # x <- model.fit$tqx.inv(u, x = start.age, sex = gender)
      pred.mat <- ceiling(matrix(x, nrow = n, ncol = N)/dt)*dt # age at death rounded
      # convert to death rate (out of starting cohort) per period
      return(
        as.data.frame(
          apply(pred.mat, MARGIN = 1, # apply on rows
                function(vec) 
                  sapply(
                    seq(from = 0, to = t.end - t.now, by = dt), 
                    function(t) sum(vec == t))
                )))
    }
  } else if (model.fit$source == "lapse") {
    if (model.fit$model == "const") {
      return(as.matrix(rep(model.fit$const, times = length(pred.horizon)*n), 
                       nrow = n, ncol = length(pred.horizon)))
    }
  } else if (model.fit$source == "annuitization") {
    if (model.fit$model == "const") {
      return(as.matrix(rep(model.fit$const, times = length(pred.horizon)*n), 
                       nrow = n, ncol = length(pred.horizon)))
    } 
  }
}


# TODO: this function needs to be extended
decrement.plot <- function(input.data, model.pred.df, t.now, t.end, dt.char) {
  
  model.pred.cum.death <- cumsum(rowMeans(model.pred.df))
  data.frame(time.idx = seq(from = t.now, to = t.end, 
                            by = switch(dt.char, 
                                        "daily" = "day", 
                                        "monthly" = "month", 
                                        "quarterly" = "quarter", 
                                        "annually" = "year")), 
             life.remaining = input.data[1, "count"] - model.pred.cum.death) %>% 
    ggplot(aes(x = time.idx, y = life.remaining)) + geom_area(fill="lightgrey") + 
    scale_x_date(labels = scales::date_format(switch(dt.char, 
                                                     "daily" = "%Y-%m-%d", 
                                                     "monthly" = "%b %Y", 
                                                     "quarterly" = "%b %Y", 
                                                     "annually" = "%Y")), 
                 date_breaks = switch(dt.char, 
                                      "daily" = "3 months", 
                                      "monthly" = "3 months", 
                                      "quarterly" = "1 year", 
                                      "annually" = "1 year")) + 
    xlab("Time") + ylab("Number of Remaining Lives") + theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.line = element_line(colour = "black"), panel.border = element_blank())
  
}

# The following is some helper code for data import
# # read in seperate life tables for male and female
# # require: data/mltper_1x1.txt and data/fltper_1x1.txt
# life.table.male <- as.data.frame(read.table("data/mltper_1x1.txt", 
#                                             skip = 1, header = TRUE))
# life.table.female <- as.data.frame(read.table("data/fltper_1x1.txt", 
#                                               skip = 1, header = TRUE))
# # combine the life tables, select relevant rows, and modify data types
# life.table.all.2015 <- 
#   bind_rows(life.table.male, life.table.female, .id = "gender") %>% 
#   select(gender, Year, Age, lx, dx) %>% 
#   filter(Year == 2015) %>% 
#   select(-Year) %>% 
#   mutate(gender = recode_factor(gender, "1" = "Male", "2" = "Female"), 
#          age.floor = as.integer(as.character(
#            recode_factor(Age, "110+" = "110")))) %>% 
#   select(-Age)
# # show data 
# life.table.all.2015

