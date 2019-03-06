# directories
#ts = data.frame(time=eva, timeseries=ev)
ts = data.frame(time=bun_df$Year, timeseries=bun_df$TempJJA)

timeseries_orig = ts$timeseries #* 100000000
time = ts$time

new_dataset <- function(dataset, step_size) {
  data_X = c()
  data_Y = c()
  for(i in 1:(length(dataset)-step_size)) {
    a <- dataset[i:(i+step_size)]
    data_X <- c(data_X, a)
    data_Y <- c(data_Y, dataset[i + step_size])
  }
  return(list(X = data_X, Y = data_Y))
}

dataset <- runif(100)
new_dataset(dataset, 2)

split_into_chunks <- function(data, train, predict, step, binary=TRUE, scale=TRUE) {
  X <- list()
  Y <- list()
  
  n <- 1
  for(i in 1:length(data)) {
    tryCatch({
      x_i = data[i:(i + train-1)]
      y_i = data[(i + train - 1 + predict)]
    }, error = function(cond) {
      print(cond)
    }, warning = function(cond){
      print(cond)
    } )
    
    # Use it only for daily return time series
    if (binary){
      if(y_i > 0.0) {
        y_i <- c(1.0, 0.0)
      }
      else {
        y_i <- c(0.0, 1.0)
      }
      if (scale)
        x_i <- scale(x_i)
    }
    else {
      if(i+train > length(data) || (i+train+predict) > length(data)) {
        break
      }
      timeseries <- c(data[i:(i + train - 1 + predict)])
      if(scale)
        timeseries[i:(i + train - 1)] = scale(timeseries[i : (i + train - 1)])
      
      nt <- length(timeseries)
      x_i = timeseries[-nt]
      y_i = timeseries[nt]
    }
    
    X[[n]]<-x_i
    Y[[n]]<-y_i
    n <- n + 1
    
    i <- i + step
  }
  
  res = list(X=X, Y=Y)
  return(res)
}


shuffle_in_unison<- function(a, b) {
  # courtsey http://stackoverflow.com/users/190280/josh-bleecher-snyder
  if (length(a) != length(b)) {
    print("Error: Length doesn't match for X and Y.")
    return()
  }
  
  shuffled_a = list()
  shuffled_b = list()
  permutation = sample(length(a))
  
  old_index <- 1
  for(new_index in permutation) {
    shuffled_a[[new_index]] = a[[old_index]]
    shuffled_b[[new_index]] = b[[old_index]]
    old_index <- old_index + 1
  } 
  
  res <- list(X = shuffled_a, Y = shuffled_b) 
  return(res)
}

crete_Xt_Yt <- function(X, y, percentage = 0.65, shuffle=TRUE) {
  idx = as.integer(length(X) * percentage)
  X_train = X_train_orig = X[1:idx]
  Y_train = Y_train_orig = y[1:idx]
  
  if (shuffle) {
    sres = shuffle_in_unison(X_train, Y_train)
    X_train = sres$X
    Y_train = sres$Y
  }
  
  N <- length(X)
  X_test = X[(idx+1):N]
  Y_test = y[(idx+1):N]
  
  res <- list(X_train_orig = X_train_orig, X_train = X_train, X_test = X_test, Y_train = Y_train, 
              Y_train_orig = Y_train_orig, Y_test = Y_test)
  return(res)
}

process_time_series_data <- function(timeseries, train_size, target_time, lag_size, binary=FALSE, scale=FALSE, percentage = 0.9) {
  X_Y <- split_into_chunks(timeseries, TRAIN_SIZE, TARGET_TIME, LAG_SIZE, binary=FALSE, scale=FALSE)
  
  X <- X_Y$X
  Y <- X_Y$Y
  
  res <- crete_Xt_Yt(X, Y, percentage = percentage, shuffle = FALSE)
  
  return(res)  
}

train_and_predict <- function(X_train, Y_train, X_test, Y_test, model = 'random_forest', time) {
  ny <- dim(X_train)[2]
  #seed
  nc <- ncol(X_train)
  
  if (model == 'random_forest') {
    vars <- c()
    for(i in 1:nc) {
      cn <- paste("Lag", as.character(nc - i + 1), sep="")
      vars <- c(vars, cn)
    }
    
    X_train_rf <- cbind(X_train, Y_train)
    outcome <- 'Price'
    colnames(X_train_rf) <- c(vars, outcome)
    head(X_train_rf)
    
    (fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))
    
    set.seed(12345)
    (model_rf <- ranger(fmla, # formula 
                        data= data.frame(X_train_rf), # data
                        num.trees = 500, 
                        respect.unordered.factors = "order"))
    print(model_rf)
    
    X_test_rf <- cbind(X_test)
    colnames(X_test_rf) <- c(vars)
    #head(X_test_rf)
    X_pred_rf <- predict(model_rf, X_test_rf)$predictions
    np <- length(X_pred_rf)
    
    nt <- length(time)
    s <- (nt-np + 1)
    e <- s + np -1
    t <- time[s:e]
    length(t)
    
    model <- model_rf
    predict_continuous_rf <- function(model, X_train_orig, nx, ny, vars ) {
      s <- length(X_train_orig)
      X_test_new_list <- X_train_orig[[s]]
      X_test_new <- matrix(unlist(X_test_new_list), nrow=1, ncol=ny)
      colnames(X_test_new) <- c(vars)
      
      X_pred_cont <- c()
      for (i in 1:nx) {
        #print(X_test_new_list)
        pred = predict(model, X_test_new)$predictions
        #print(pred)
        X_pred_cont <- c(X_pred_cont, pred)
        X_test_new_list <- X_test_new_list[2:ny]
        X_test_new_list <- c(X_test_new_list, pred)
        X_test_new <- matrix(unlist(X_test_new_list), nrow=1, ncol=ny)
        colnames(X_test_new) <- c(vars)
      }
      return(X_pred_cont) 
    }
    nx <- dim(X_test)[1]
    ny <- dim(X_test)[2]
    X_pred_cont_rf <- predict_continuous_rf(model_rf, X_train_orig, nx, ny, vars)
  }
  
  res <- list(X_pred_rf = X_pred_rf, X_pred_cont_rf = X_pred_cont_rf)
  return(res)
}


#tL <- length(timeseries_orig)

timeseries = timeseries_orig #[1:as.integer(tL* 0.80)]

#time = dfp$close_time
par(mfrow=c(1,1))
plot(time, timeseries, t='l')

# for binance data
#TRAIN_SIZE =  (30/5)*2
# for climate data
TRAIN_SIZE = 1
TARGET_TIME = 1
LAG_SIZE = 1

res <- process_time_series_data(timeseries, TRAIN_SIZE, TARGET_TIME, LAG_SIZE, scale = TRUE, percentage = 0.97)
X_train <- res$X_train
X_train_orig <- res$X_train_orig
X_test <- res$X_test
Y_train <- res$Y_train
Y_train_orig <- res$Y_train_orig
Y_test <- res$Y_test

X_train <- matrix(as.numeric(unlist(X_train)), ncol=TRAIN_SIZE, byrow=TRUE)
X_test <- matrix(as.numeric(unlist(X_test)), ncol=TRAIN_SIZE, byrow=TRUE)
Y_train <- matrix(as.numeric(unlist(Y_train)), ncol=TARGET_TIME, byrow=TRUE)
Y_test <- matrix(as.numeric(unlist(Y_test)), ncol=TARGET_TIME, byrow=TRUE)

ny <- dim(X_train)[2]


# Model using random forest
#seed
nc <- ncol(X_train)
vars <- c()
for(i in 1:nc) {
  cn <- paste("Lag", as.character(nc - i + 1), sep="")
  vars <- c(vars, cn)
}

X_train_rf <- cbind(X_train, Y_train)
outcome <- 'Outcome'
colnames(X_train_rf) <- c(vars, outcome)
head(X_train_rf)

(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))

library(ranger)
set.seed(12345)
(model_rf <- ranger(fmla, # formula 
                    data= data.frame(X_train_rf), # data
                    num.trees = 500, 
                    respect.unordered.factors = "order"))
model_rf

# check model accuracy
X_valid_rf <- matrix(unlist(X_train_orig), 
                     nrow = length(X_train_orig), 
                     ncol = length(X_train_orig[[1]]),
                     byrow = TRUE)
X_valid_rf <- cbind(X_valid_rf, unlist(Y_train_orig))
colnames(X_valid_rf) <- c(vars, outcome)
head(X_valid_rf)
Y_valid <- predict(model_rf, X_valid_rf)

yt <- unlist(Y_train_orig)
xt <- time[1:length(yt)]
plot_ly(data=data.frame(time=xt, timeseries=yt)) %>%
  add_lines(x=~time, y=~timeseries) %>%
  add_lines(x=xt,y=Y_valid$predictions)


# Using GLM: quassipoisson
head(X_train_rf)
nc <- ncol(X_train)
vars <- c()
for(i in 1:nc) {
  cn <- paste("Lag", as.character(nc - i + 1), sep="")
  vars <- c(vars, cn)
}
(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))
fmla.glm <- as.formula(fmla)
library(mgcv)
model.glm <- glm(fmla.glm, data=data.frame(X_train_rf), family=gaussian)
model.glm
model.glm2 <- glm(fmla.glm, data=data.frame(X_train_rf), family=quasipoisson)
model.glm2

# Check model accuracy with the train data
X_valid_rf <- matrix(unlist(X_train_orig), 
                     nrow = length(X_train_orig), 
                     ncol = length(X_train_orig[[1]]),
                     byrow = TRUE)
X_valid_rf <- cbind(X_valid_rf, unlist(Y_train_orig))
colnames(X_valid_rf) <- c(vars, outcome)
head(X_valid_rf)
Y_valid1 <- predict(model.glm, data.frame(X_valid_rf))
Y_valid2 <- predict(model.glm2, data.frame(X_valid_rf))

yt <- unlist(Y_train_orig)
xt <- time[1:length(yt)]
plot_ly(data=data.frame(time=xt, timeseries=yt)) %>%
  add_lines(x=~time, y=~timeseries) %>%
  add_lines(x=~time,y=Y_valid1) %>%
  add_lines(x=xt,y=Y_valid2)


# Using GAM
head(X_train_rf)
nc <- ncol(X_train)
vars <- c()
for(i in 1:nc) {
  cn <- paste("s(Lag", as.character(nc - i + 1), ")", sep="")
  vars <- c(vars, cn)
}
(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))
fmla.gam <- as.formula(fmla)
library(mgcv)
model.gam <- gam(fmla.gam, data=data.frame(X_train_rf), family=gaussian)
model.gam

# check model accuracy
head(X_valid_rf)
Y_valid_rf <- predict(model_rf, data.frame(X_valid_rf))
Y_valid_gam <- predict(model.gam, data.frame(X_valid_rf))

yt <- unlist(Y_train_orig)
xt <- time[1:length(yt)]
plot_ly(data=data.frame(time=xt, timeseries=yt)) %>%
  add_lines(x=~time, y=~timeseries) %>%
  add_lines(x=xt,y=Y_valid_rf$predictions) %>%
  add_lines(x=xt,y=Y_valid_gam)

# Using xgboost
library(xgboost)
library(dplyr)
cv <- xgb.cv(data=X_train,
             label=Y_train,
             nrounds = 1000,
             nfold=10,
             objective='reg:linear',
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 0
)

# Get the evaluation log 
elog <- cv$evaluation_log

# Determine and print how many trees minimize training and test error
elog %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(test_rmse_mean))   # find the index of min(test_rmse_mean)

ntrees <- which.min(elog$train_rmse_mean)

model_xgb <- xgboost(data = X_train, # training data as matrix
                     label = Y_train,  # column of outcomes
                     nrounds = ntrees,       # number of trees to build
                     objective = "reg:linear", # objective
                     eta = 0.3,
                     depth = 6,
                     verbose = 0  # silent
)

Y_valid_xgb <- predict(model_xgb, X_train)

# check model accuracy
yt <- unlist(Y_train_orig)
xt <- time[1:length(yt)]
plot_ly(data=data.frame(time=xt, timeseries=yt)) %>%
  add_lines(x=~time, y=~timeseries) %>%
  add_lines(x=xt,y=Y_valid_xgb) %>%
  add_lines(x=xt,y=Y_valid_rf$predictions)

#lstm
library(keras)
library(readr)
library(stringr)
library(purrr)

model <- keras_model_sequential()

HIDDEN_RNN <- 2
EMB_SIZE <- 1
model %>%
  layer_lstm(32, input_shape = c(1,TRAIN_SIZE), return_sequences=TRUE) %>%
  layer_lstm(16) %>%
  #layer_activation('relu') %>%
  layer_dense(1) %>%
  layer_activation("linear")

optimizer <- optimizer_rmsprop(lr = 0.01)

model %>% compile(
  loss = 'mean_squared_error',#"categorical_crossentropy", #
  optimizer = optimizer
)

# Training & Results ----------------------------------------------------

sample_mod <- function(preds, temperature = 1){
  preds <- log(preds)/temperature
  exp_preds <- exp(preds)
  preds <- exp_preds/sum(exp(preds))
  
  rmultinom(1, 1, preds) %>% 
    as.integer() %>%
    which.max()
}

on_epoch_end <- function(epoch, logs) {
  
  cat(sprintf("epoch: %02d ---------------\n\n", epoch))
  
}

print_callback <- callback_lambda(on_epoch_end = on_epoch_end)

x <- array(0, dim = c(nrow(X_train), 1, ncol(X_train)))
y <- array(0, dim = c(nrow(Y_train), ncol(Y_train)))
for(i in 1:nrow(X_train)){
  x[i,,] <- X_train[i,]
  y[i,] <- Y_train[i]
}

model %>% 
  fit(
    x, y,
    batch_size = 10,
    epochs = 1000,
    verbose = 2,
    callbacks = print_callback
  )

x_lstm = x
y_lstm = y

# check model accuracy
Y_valid_lstm <- model %>% predict(x)
plot_ly(data=data.frame(time=xt, timeseries=yt)) %>%
  add_lines(x=~time, y=~timeseries) %>%
  add_lines(x=xt,  y=Y_valid_lstm[,1])%>%
  add_lines(x=xt,y=Y_valid_xgb) %>%
  add_lines(x=xt,y=Y_valid_rf$predictions)



# Modeling is done for the timeseries
x = X_valid_rf
# with xgboost
predict(model_xgb, x[,-4])
# with random forest
predict(model_rf, x)$predictions
# with lstm
predict(model_lstm, x_lstm)[,1]

# Now do MCMC
f <- function(x, model_name) {
    if (model_name == 'xgb') { #xgboost
        res <- predict(model_xgb, x)
    } else if (model_name == 'rf') { #random forest
        res <- predict(model_rf, x)$predictions
    } else if (model_name == 'lstm') { 
        res <- predict(model_lstm, x)[,1]
    }
    return(res)
}

q <- function(x) rnorm(1, x, 1)

# Metropolis hastings algorithm
#x: current state
#q: proposal function
#f: distribution of x (known)
step <- function(x, f, q) { 
  ## Pick new point
  xp <- q(x)
  ap <- NA
  # acceptance probability
  if (is.na(f(x)) || f(x) == 0) {
    ap <- runif(1)
  } else {
    ap <- f(xp)/f(x)
  }
  if (is.na(ap)) {
    ap <- 1
  }
  alpha <- min(1, ap)
  u <- runif(1)
  if (u < alpha) {
    x <- xp 
  }
  x
}

run <- function(x, f, q, nsteps) {
  res <- matrix(NA, nsteps, length(x))
  for (i in seq_len(nsteps))
    res[i,] <- x <- step(x, f, q) 
  drop(res)
}

p <- 0.4
mu <- c(-1, 2)
sd <- c(.5, 2)
f <- function(x)
  p     * dnorm(x, mu[1], sd[1]) +
  (1-p) * dnorm(x, mu[2], sd[2])

par(mfrow=c(1,1))
x <- eva #bun_df$Year
y <- ev  #bun_df$TempJJA
plot(x, y, t='l')

cdf <- ecdf(y)
pdf <- density(y)
apdf <- approxfun(density(y))
plot(cdf)
plot(pdf)

q <- function(x) rnorm(1, x, 0.4)
f <- function(x) apdf(x)
#short run
s <- mean(y)
par(mfrow=c(1,1))
set.seed(1)

res <- run(s, f, q, 100000)
#res <- abs(res)

#library(mcmc)
#res <- metrop(f, s, 1000)

layout(matrix(c(1, 2), 1, 2), widths=c(4, 1))
par(mar=c(4.1, .5, .5, .5), oma=c(0, 4.1, 0, 0))
plot(res, type="s", xpd=NA, ylab="Parameter", xlab="Sample", las=1)
usr <- par("usr")
xx <- seq(usr[3], usr[4], length=301)
plot(f(xx), xx, type="l", yaxs="i", axes=FALSE, xlab="")

hist(res, 40, freq=FALSE, main="", 
     #ylim=c(0, .4), 
     las=1,
     xlab="x", ylab="Probability density")
z <- integrate(f, min(res), max(res))$value
curve(f(x) / z, add=TRUE, col="red", n=200)

sf <- approxfun(ev, eva, method='constant')
y <- seq(min(ev), max(ev), length.out = 200)
x <- sf(y)
df <- data.frame(x=x, y=y)
df <- df[order(x, decreasing = F),]
par(mfrow=c(1,1))
plot(df, t='l')
lines(eva, ev, col=2)


# long run
set.seed(1)
res.long <- run(-10, f, q, 50000)
hist(res.long, 100, freq=FALSE, main="", ylim=c(0, .4), las=1,
     xlab="x", ylab="Probability density", col="grey")
z <- integrate(f, -Inf, Inf)$value
curve(f(x) / z, add=TRUE, col="red", n=200)

# different proposal mechanism
res.fast <- run(-10, f, function(x) rnorm(1, x,  33), 1000)
res.slow <- run(-10, f, function(x) rnorm(1, x,  .3), 1000)
res.slow2 <- run(-10, f, function(x) rnorm(1, x,  .25), 1000)


layout(matrix(c(1, 2), 1, 2), widths=c(4, 1))
par(mar=c(4.1, .5, .5, .5), oma=c(0, 4.1, 0, 0))
plot(res, type="s", xpd=NA, ylab="Parameter", xlab="Sample", las=1,
     col="grey")
lines(res.fast, col="red")
lines(res.slow, col="blue")
lines(res.slow2, col="orange")

plot(f(xx), xx, type="l", yaxs="i", axes=FALSE)

x <- seq(0, 2, by=0.005)
f1 <- splinefun(eva, ev)
f2 <- approxfun(eva, ev)
f3 <- loess(ev ~ eva, degree = 2)
y1 <- f1(x)
y2 <- f2(x)
y3 <- predict(f3, x)
plot(eva, ev, lty=2)
lines(eva, ev, lty=2, col='black')
# with splinefun
lines(x, y1, t='l', col='red')
# with approxfun
lines(x, y2, t='l', col='green')
# with loess
lines(x, y3, t='l', col='pink')


tukey.window<-function(n,a) 
{   
  t2 <- seq(0,1,length.out=n);
  per <- a/2; 
  tl <- floor(per*(n-1))+1;
  th <- n-tl+1;
  # Window is defined in three sections: taper, constant, taper
  w <- c( 0.5+0.5*cos( (pi/per)*(t2[1:tl] - per) ) ,  rep(1,th-tl-1), 0.5+0.5*cos((pi/per)*(t2[th:n] - 1 + per))) 
  return(w)
}

hanning.window<-function(n) 
{
  if (n == 1 ) 
    c <- 1
  else {
    n <- n - 1
    c <- 0.5 - 0.5 * cos(2 * pi * (0:n)/n)
  }
  return(c)
}

tri.window<-function(n)
{
  c <- c( 2/n*( n/2-abs( seq(0,n-1)-(n-1)/2 ) ) )
  
  return(c)
}

hamming.window<-function(n) 
{
  if (n == 1) 
    c <- 1
  else {
    n <- n - 1
    c <- 0.54 - 0.46 * cos(2 * pi * (0:n)/n)
  }
  return(c)
}

hanning_ma <- function(df) {
  N <- length(df)
  prev <- NA
  nextt <- NA
  x <- rep(0, N)
  for(n in 1:N) {
    if(n==1) {
      prev <- 0
      nextt <- 2 * df[n+1]
    } else if(n==length(df)) {
      prev <- 2 * df[n-1]
      nextt <- 0
    } else {
      prev <- df[n-1]
      nextt <- df[n+1]
    }
    
    x[n] <- 0.5 * df[n] + 0.25*(prev + nextt)
  }
  return(x)
}

# Moving average is a low pass filter
ma <- function(y, sm=5) {
  n <- length(y)
  
  fw <- rep(1, sm)/sm
  yf <- conv(y, fw)
  yf <- yf[(sm-sm/2):(length(yf)-sm/2)]
  
  return(yf)
}

slide_ma <- function(y, sm=50) {
  n <- length(y)
  res <- NA
  i <- 1
  k <- 1
  while(i <= n){
    s <<- i
    e <- s + sm - 1
    res[k] <- mean(y[s:e])
    k <- k+1
    i <- e + 1
  }
  
  return(res)
}

gaussian_ma <- function(y, mean=0, sigma=3, span=5) {
  # gaussian over range of 5 sigma
  n <- length(y)
  x <- ((0:span) * sigma) - 2.5*sigma
  mu <- rep(mean, length(x))
  fw <- exp(-(x-mu)^2/(2 * sigma^2))
  yf <- conv(y, fw)
  nf <- length(yf)
  
  k <- span/2
  yf <- yf[(k+1):(nf-k)]
  
  return(yf)
}

runRandomSample <- function(y, nsteps, plt=FALSE) {
  res <- matrix(NA, nsteps, length(y))
  y <- as.numeric(y)
  n <- length(y)
  yorig <- y
  for (i in seq_len(nsteps)) {
    ix <- sample(1:n, n)
    y <- y[ix]
    
    if(plt) {
      ymax <- max(max(yorig), max(y))
      ymin <- min(min(yorig), min(y))
      plot(1:length(yorig), yorig, t='l', ylim=c(ymin, ymax))
      lines(1:length(y), y, col=2, lty=2)
      Sys.sleep(2)
    }
    res[i, ] <- y
  }
  drop(res)
}

runApprox <- function(x, y, f, nsteps, plt=FALSE, filter=NA, noise=TRUE, sd=1) {
  res <- matrix(NA, nsteps, length(y))
  y <- as.numeric(y)
  yorig <- y
  for (i in seq_len(nsteps)) {
    # get x2 along x with 2x more point
    xs <- runif(1, min=x[1], max=x[2])
    # get equidistant equal length x
    x2 <- seq(xs, max(x), length.out=length(x))
    # interpolate the value
    y <- f(x2)
    # add gaussian error
    if(noise) {
      ge <- rnorm(1, 0, sd)
      y <- y + ge
    }
    if(!is.na(filter)) {
      if (filter=='ma') 
        yf <- ma(y)
      else if(filter=='hanning')
        yf <- hanning_ma(y) 
      else if(filter=='gaussian')
        yf <- gaussian_ma(y)
    }
    
    if(plt) {
      ymax <- max(max(yorig), max(y))
      ymin <- min(min(yorig), min(y))
      plot(1:length(yorig), yorig, t='l', ylim=c(ymin, ymax))
      lines(1:length(y), y, col=2, lty=2)
      #points(1:length(y), y, col=2, cex=0.5)
      if(!is.na(filter))
        lines(1:length(yf), yf, col=3, lty=2, lwd=2)
      Sys.sleep(2)
    }
    if (!is.na(filter)) {
      y <- yf
    } 
    res[i, ] <- y
  }
  drop(res)
}

runspec <- function(sv, plt=FALSE) {
  splist <- matrix(nrow=nrow(sv), ncol=ncol(sv)/2)
  for(i in 1:nrow(sv)) {
    r <- sv[i,]
    r <- (r-mean(r))/sd(r)
    sp <- spec.pgram(r, demean=T, detrend=T, taper=0.1, 
                     log='no',
                     #spans=c(3,5),
                     plot=F)
    splist[i,] <- sp$spec/sum(sp$spec)
    i <- i+1
    
    if(plt) {
      plot(sp$freq, sp$spec/sum(sp$spec), t='l')
      Sys.sleep(2)
    }
  }
  
  return(splist)
}


#Ex 1
x <- as.numeric(time(ldeaths)) # eva
y <- as.numeric(ldeaths) # ev.tap


# EX 2
xm <- 1000
dx <- 10
x <- seq(0, xm-1, by=dx)
n <- length(x)
y <- rand(1,n)
length(y)

x <- eva
y <- ev
plot(x, y, t='l')

bun <- bun_df[bun_df$Year < 2000,]
x <- bun$Year
y <- bun$TempJJA
n <- length(y)
n
x <- slide_ma(x)[-41]
y <- slide_ma(y)[-41]
plot(x, y, t='l')

y<-as.numeric(y)
yorig <- y
y <- y-mean(y)
lines(x, y, col=2)
y <- y/sd(yorig)
lines(x, y, col=3)
lmf <- lm(y~x)
y <- y.dtrnd <- lmf$residuals
lines(x, y, col=4)

nsim <- 10000

# Approach 1
# generate similar sample using approxfun
sf <- approxfun(x, y) #splinefun
res <- runApprox(x, y, sf, nsim, 
                 #filter='hanning', 
                 #plt=TRUE,
                 noise=TRUE, sd=0.5)

# Approach 2
# generate sample by randomly permuting
#res <- runRandomSample(y, nsim) #, plt=TRUE)

spl <- runspec(res) #, plt=TRUE)
sp_mean <- apply(spl, 2, mean)

Pspec <- spec.pgram(y, demean=T, detrend=T, taper=0.1, 
                    #spans=c(3,5),
                    #log='no', 
                    plot=F)
f = Pspec$freq
sp = Pspec$spec/sum(Pspec$spec)

alpha <- 0.05
pl <- 1 - alpha/2
pu <- alpha/2
dof <- Pspec$df
lchi <- (dof * sp) / qchisq(pl, dof)
uchi <- (dof * sp) / qchisq(pu, dof)
plot(f, sp, t='l', lty=1, xlab='Freq', ylab='Spectral power')
lines(f, uchi, lty=2)
lines(f, lchi, lty=2)

lines(Pspec$freq, sp_mean, lty=2,
      xlim=c(0,0.05))

#plot.show(trajectory=ev.tap, harmonics=c(4,5, 8), scale=5)

# using astrochron
x = eva
y = ev.tap
ev_model <- linterp(data.frame(year=x*1000, event=y))
Mspec <- mtm(ev_model, demean = T, detrend = T, 
             ntap = 5, tbw = 3, #ar1 = T,
             #xmin = 0,
             #xmax = 0.01,
             output = 1, pl=2)

Mspecdf <- data.frame(Mspec)
Mspecdf <- Mspecdf[order(Mspecdf$AR1_CL, decreasing = T),]
head(Mspecdf)
N <- length(ev.tap)
Mspecdf$period <- (1/Mspecdf$Frequency ) #* (time_window / N))
head(Mspecdf, n = 10)

# using redfit
x=as.numeric(ev.tap)
t=as.numeric(eva)
lines(t, x)
library(dplR)
redf.dat.ev <- redfit(x=x, nsim = 1000, mctest = TRUE)
redf.dat <- redf.dat.ev
par(mfrow=c(2,2), mar=c(4,4,4,4))
#par(tcl = 0.5, mar = rep(2.2, 4), mgp = c(1.1, 0.1, 0))
plot(redf.dat[['freq']], redf.dat[['gxx']],
       #xlim = c(0, 0.005),
       #ylim = range(redf.dat[["ci99"]], redf.dat[["gxx"]]),
       type = "n", 
       ylab = "Spectral Power (dB)", 
       xlab = "Frequency (1/yr)",
       axes = FALSE)
grid()
lines(redf.dat[["freq"]], redf.dat[["gxx"]], col = "black", lwd=2)
lines(redf.dat[["freq"]], redf.dat[["ci99"]], col = "red")
lines(redf.dat[["freq"]], redf.dat[["ci95"]], col = "pink")
lines(redf.dat[["freq"]], redf.dat[["ci90"]], col = "green")
lines(redf.dat[["freq"]], redf.dat[["ci80"]], col = "orange")
freqs <- pretty(redf.dat[["freq"]])
pers <- round(2000 / (freqs * length(ev.tap)), 2)
Rspecdf.ev <- data.frame(freq=redf.dat[["freq"]], 
                      spec=redf.dat[["gxx"]],
                      period=2000/(redf.dat[["freq"]] * length(x)))
Rspecdf.ev <- Rspecdf[order(Rspecdf.ev$spec, decreasing = T),]
head(Rspecdf.ev, 20)
axis(1, at = freqs, labels = TRUE)
axis(3, at = freqs, labels = pers)
mtext(text = "Period (yr)", side = 3, line = 2)
axis(2); #axis(4)
legend("topright", c("dat", "CI99", "CI95", "CI90", "CI80"), lwd = 2,
        col = c("black", "red", "pink", "green"),
        bg = "white", cex=0.5)
box()
#par(op)




