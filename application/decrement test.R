source("decrement.R")

################################################################################
### TESTING
################################################################################

life.table <- 
  decrement.clean.data(
    path.to.file = "data/Empirical Life Table.csv", 
    source = "mortality", 
    model = "life-table")
mortality.model <- 
  decrement.calibrate(
    hist.data = life.table, 
    source = "mortality", 
    model = "life-table", 
    model.param = list(assumption = "UDD"))
mortality.model$tqx(3, 50, "Male")
mortality.model$tqx.inv(0.9999, 50, "Male")
input.data <- data.frame(count = c(20), gender = c("Male"), start.age = c(50))
model.pred <- 
  decrement.predict(model.fit = mortality.model, 
                    t.now = as.Date("2017-01-01"), dt = "monthly", t.end = as.Date("2037-01-01"), 
                    n = 100, new.data = input.data)
decrement.plot(input.data = input.data, model.pred.df = model.pred, 
               t.now = as.Date("2017-01-01"), t.end = as.Date("2037-01-01"), 
               dt.char = "monthly")

