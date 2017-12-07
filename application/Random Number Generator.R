################################################################################
### To be included in helper.R
################################################################################

# Main function ---
STAT428.random.number.1D <- function(n, method, method.param = NA){
  # Generate random numbers using methods taught in STAT 428 Fall 2017
  # 
  # Args: 
  #   n: sample size to generate
  #   method: one of "inverseCDF", "AR", "MHA"
  #   method.param: associated parameters for the method selected
  #     - "inverseCDF" requires method.param = list(invCDF)
  #     - "AR" requires method.param = list(dtarget, dproposal, rproposal, C)
  #     - "MHA" requires method.param = list()
  # 
  # Return: 
  #   a vector of generated iid sample
  
  if (method == "inverseCDF") { ### Inverse CDF Method ---
    return(STAT428.rdist.invCDF(n, invCDF = method.param$invCDF))
  } else if (method == "AR") { ### Accept-Rejection Method ---
    if ("C" %in% names(method.param)) {
      return(STAT428.rdist.AR(n, dtarget = method.param$dtarget, 
                              dproposal = method.param$dproposal, 
                              rproposal = method.param$rproposal, 
                              C = method.param$C)$x)
    } else {
      return(STAT428.rdist.AR(n, dtarget = method.param$dtarget, 
                              dproposal = method.param$dproposal, 
                              rproposal = method.param$rproposal)$x)
    }
  } else if (method == "MHA") { ### Metropolis-Hastings Sampler ---
    if ("x.init" %in% names(method.param)) {
      return(STAT428.rdist.MHA(n, dtarget = method.param$dtarget, 
                              dproposal = method.param$dproposal, 
                              rproposal = method.param$rproposal, 
                              x.init = method.param$x.init)$x)
    } else {
      return(STAT428.rdist.MHA(n, dtarget = method.param$dtarget, 
                              dproposal = method.param$dproposal, 
                              rproposal = method.param$rproposal)$x)
    }
  }
}


### Inverse CDF Method ---
STAT428.rdist.invCDF <- function(n, invCDF) {
  # Args: 
  #   n: sample size
  #   invCDF: inverse CDF function (need to be vectorized)
  # 
  # Return: 
  #   a vector of generated iid sample
  
  u <- runif(n)
  x <- invCDF(u)
  
  return(x)
}


### Accept-Rejection Method ---
STAT428.rdist.AR <- function(n, dtarget, dproposal, rproposal, C = "auto") {
  # Args: 
  #   n: sample size
  #   dtarget(x): density function of target distribution with support on R
  #   dproposal(x): density function of proposal distribution with support on R
  #   rproposal(n): random number generator of proposal distribution
  #   C: normalizing constant, can be a number or "auto"
  # 
  # Return: 
  #   a list consisting of 
  #     x: a vector of generated iid sample
  #     rejections: number of rejections before reaching the sample size
  
  # get normalizing constant
  if (C == "auto") {
    temp.ratiofunc <- function(x) { 
      if (dproposal(x) == 0) {
        return(0)
      } else {
        return( -dtarget(x)/dproposal(x) ) # -1 to switch from max to min
      }
    }
    # abs() to switch the optimal value back
    thres <- abs(optim(par = c(0.1), fn = temp.ratiofunc, method = "BFGS")$value)
  } else { thres <- C }
  
  # initialization
  k <- 0
  rejection.counter <- 0
  x <- numeric(n)
  
  # A-R algorithm
  while ( k < n ) {
    u <- runif(n = 1)
    cand <- rproposal(n = 1)
    ratio <- dtarget(cand) / (thres * dproposal(cand))
    if (u < ratio) {
      k <- k + 1
      x[k] <- cand
    } else {
      rejection.counter <- rejection.counter + 1
    }
  }
  
  return(list(x = x, rejections = rejection.counter))
}


### Metropolis-Hastings Sampler ---
STAT428.rdist.MHA <- function(n, dtarget, dproposal, rproposal, x.init = NA) {
  # Args: 
  #   n: sample size
  #   dtarget(x): density function of target distribution with support on R
  #   dproposal(x, param): density function of proposal dist with support on R
  #   rproposal(n, param): random number generator of proposal distribution
  #   x.init: 
  #     initial point (X0) of the chain, 
  #     if not specified, rproposal(n, param) must have default param value
  # 
  # Return: 
  #   a list consisting of 
  #     x: a vector of generated iid sample
  #     rejections: number of rejections before reaching the sample size
  
  # get initial point
  if (is.na(x.init)) { x0 <- rproposal(n = 1) } else { x0 <- x.init }
  
  # initializations
  chain <- numeric(n)
  chain[1] <- x0
  rejection.counter <- 0
  u <- runif(n)
  
  # generate chain
  for (i in 2:n) {
    xt <- chain[i-1]
    y <- rproposal(n = 1, param = xt) # generate candidate point Y at each transition
    # compute r(X[i-1], Y)
    numerator   <- dtarget(y)  * dproposal(xt, param = y)
    denominator <- dtarget(xt) * dproposal(y , param = xt)
    r <- numerator/denominator
    if (u[i] <= r){ # Y is accepted
      chain[i] <- y
    } else {        # Y is rejected
      # assign xt
      chain[i] <- xt
      # increment rerejction counter
      rejection.counter <- rejection.counter + 1 
    }
  }
  
  return(list(x = chain, rejection = rejection.counter))
}


################################################################################
### Testing
################################################################################


# # inverse CDF
# exp.lambda <- 3
# 
# exp.sample <- STAT428.random.number.1D(
#   n = 1000, method = "inverseCDF",
#   method.param = list(invCDF = function(x) -log(1-x)/exp.lambda))
# 
# mean(exp.sample) / (1/exp.lambda)
# var(exp.sample) / (1/exp.lambda^2)
# median(exp.sample) / (log(2)/exp.lambda)
# 
# 
# # Accept-Rejection Method
# beta.shape1 <- 3
# beta.shape2 <- 3
# 
# beta.sample.1 <- STAT428.random.number.1D(
#   n = 1000, method = "AR", 
#   method.param = list(dtarget   = function(x) dbeta(x, shape1 = beta.shape1, shape2 = beta.shape2), 
#                       dproposal = function(x) dunif(x, min = 0, max = 1), 
#                       rproposal = function(n) runif(n, min = 0, max = 1))) # no normalizing const C
# 
# mean(beta.sample.1) / (beta.shape1/(beta.shape1+beta.shape2))
# var(beta.sample.1) / ((beta.shape1*beta.shape2)/((beta.shape1+beta.shape2)^2*(beta.shape1+beta.shape2+1)))
# median(beta.sample.1) / ((beta.shape1-1/3)/(beta.shape1+beta.shape2-2/3))
# 
# C.thres <- gamma(beta.shape1 + beta.shape2)/(gamma(beta.shape1)*gamma(beta.shape2))*0.5^(beta.shape1+beta.shape2-2)
# beta.sample.2 <- STAT428.random.number.1D(
#   n = 10000, method = "AR", 
#   method.param = list(dtarget   = function(x) dbeta(x, shape1 = beta.shape1, shape2 = beta.shape2), 
#                       dproposal = function(x) dunif(x, min = 0, max = 1), 
#                       rproposal = function(n) runif(n, min = 0, max = 1), 
#                       C = C.thres)) # must be a tight bound for the representation of dtarget/dproposal
# 
# mean(beta.sample.2) / (beta.shape1/(beta.shape1+beta.shape2))
# var(beta.sample.2) / ((beta.shape1*beta.shape2)/((beta.shape1+beta.shape2)^2*(beta.shape1+beta.shape2+1)))
# median(beta.sample.2) / ((beta.shape1-1/3)/(beta.shape1+beta.shape2-2/3))
# 
# # Metropolis-Hastings Sampler
# rayleigh.sigma <- 3
# burn.in <- 1000
# 
# rayleigh.sample.1 <- STAT428.random.number.1D(
#   n = 10000, method = "MHA", 
#   method.param = list(dtarget   = function(x) (x / rayleigh.sigma^2) * exp(-x^2 / (2*rayleigh.sigma^2)), 
#                       dproposal = function(x, param) dchisq(x, df = param), 
#                       rproposal = function(n, param = 1) rchisq(n, df = param))) # no x0, must have default param value
# rayleigh.sample.1 <- rayleigh.sample.1[(burn.in+1):length(rayleigh.sample.1)-burn.in]
# 
# mean(rayleigh.sample.1) / (rayleigh.sigma * sqrt(pi/2))
# var(rayleigh.sample.1) / ((4-pi)*rayleigh.sigma*rayleigh.sigma/2)
# median(rayleigh.sample.1) / (rayleigh.sigma*sqrt(2*log(2)))
# 
# rayleigh.sample.2 <- STAT428.random.number.1D(
#   n = 10000, method = "MHA", 
#   method.param = list(dtarget   = function(x) (x / rayleigh.sigma^2) * exp(-x^2 / (2*rayleigh.sigma^2)), 
#                       dproposal = function(x, param) dchisq(x, df = param), 
#                       rproposal = function(n, param) rchisq(n, df = param), # no need to have default param value
#                       x.init = rayleigh.sigma)) # specify initial point
# rayleigh.sample.2 <- rayleigh.sample.2[(burn.in+1):length(rayleigh.sample.2)-burn.in]
# 
# mean(rayleigh.sample.2) / (rayleigh.sigma * sqrt(pi/2))
# var(rayleigh.sample.2) / ((4-pi)*rayleigh.sigma*rayleigh.sigma/2)
# median(rayleigh.sample.2) / (rayleigh.sigma*sqrt(2*log(2)))

