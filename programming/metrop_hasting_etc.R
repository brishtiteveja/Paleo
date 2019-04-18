library(mcmc)
data(logit)
out <- glm(y ~ x1 + x2 + x3 + x4, data=logit, family = binomial(), x = TRUE)
summary(out)
plot(out)

x <- out$x
y <- out$y

lupost <- function(beta, x, y) {
       eta <- as.numeric(x %*% beta)
       logp <- ifelse(eta < 0, eta - log1p(exp(eta)), - log1p(exp(- eta)))
       logq <- ifelse(eta < 0, - log1p(exp(eta)), - eta - log1p(exp(- eta)))
       logl <- sum(logp[y == 1]) + sum(logq[y == 0])
       return(logl - sum(beta^2) / 8) 
}


set.seed(42)
beta.init <- as.numeric(coefficients(out))
out <- metrop(lupost, initial = beta.init, nbatch= 1e3, x=x, y=y)
names(out)
out$accept

out <- metrop(out, scale = 0.1, x = x, y = y)
out$accept

out <- metrop(out, scale = 0.3, x = x, y = y)



out$accept

out <- metrop(out, scale = 0.5, x = x, y = y)
out$accept

out <- metrop(out, scale = 0.4, x = x, y = y)
out$accept

out <- metrop(out, nbatch = 1e4, x = x, y = y)
out$accept

out$time
plot(ts(out$batch))

acf(out$batch)



n <- 2e4
rho <- 0.99
x <- arima.sim(model = list(ar=rho), n=n)
x <- ev
plot(x, t='l')

out <-initseq(x)
plot(seq(along=out$Gamma.pos) - 1, out$Gamma.pos, xlab='k', 
     ylab = expression(Gamma[k]), type='l') 
lines(seq(along = out$Gamma.dec) - 1, out$Gamma.dec, lty='dotted')
lines(seq(along = out$Gamma.con) - 1, out$Gamma.con, lty='dashed')

out$var.con

blen <- 5
x.batch <- apply(matrix(x, nrow = blen), 2, mean)
x.batch
head(x.batch)

bout <- initseq(x.batch)
plot(seq(along = bout$Gamma.con) - 1, bout$Gamma.con,
             xlab = "k", ylab = expression(Gamma[k]), type = "l")
mean(x) + c(-1, 1) * qnorm(0.975) * sqrt(out$var.con / length(x))
mean(x.batch) + c(-1, 1) * qnorm(0.975) * sqrt(bout$var.con / length(x.batch))



x <- ev
out <- initseq(x)
metrop(out)


h <- function(x) if (all(x >= 0) && sum(x) <= 1) return(1) else return(-Inf)
out <- metrop(h, ev, 1000)
out$accept
# acceptance rate too low
out <- metrop(out, scale = 0.1)
out$accept
t.test(out$accept.batch)$conf.int
# acceptance rate o. k. (about 25 percent)
plot(out$batch[ , 1])
# but run length too short (few excursions from end to end of range)
out <- metrop(out, nbatch = 1e4)
out$accept
plot(out$batch[ , 1])
hist(out$batch[ , 1])
acf(out$batch[ , 1], lag.max = 250)
# looks like batch length of 250 is perhaps OK
out <- metrop(out, blen = 250, nbatch = 100)
apply(out$batch, 2, mean) # Monte Carlo estimates of means
apply(out$batch, 2, sd) / sqrt(out$nbatch) # Monte Carlo standard errors
t.test(out$accept.batch)$conf.int
acf(out$batch[ , 1]) # appears that blen is long enough



x <- eva
y <- ev
dat <- data.frame(x=eva, y=ev)
models <- list(lm(y ~ x, data = dat), 
               lm(y ~ I(1 / x), data = dat),
               lm(y ~ log(x), data = dat),
               nls(y ~ I(1 / x * a) + b * x, data = dat, start = list(a = 1, b = 1)), 
               nls(y ~ (a + b * log(x)), data = dat, start = setNames(coef(lm(y ~ log(x), data = dat)), c("a", "b"))),
               nls(y ~ I(exp(1) ^ (a + b * x)), data = dat, start = list(a = 0,b = 0)),
               nls(y ~ I(1 / x * a) + b, data = dat, start = list(a = 1,b = 1))
)

m <- lm(ev ~ log(eva))
summary(m)
plot(eva, ev)
lines(eva, m$fitted.values)


fit.lowess <- lowess(eva, ev, f=0.33)
fit.loess <- loess(ev ~ eva, span=0.1, degree=2)
plot(eva, ev)
lines(eva, fit.loess$fitted)

age <- seq(min(eva), max(eva), length.out = 1000)
predict.loess <- predict(fit.loess, age, se=TRUE)
lines(age, predict.loess$fit, col='red')

summary(fit.lowess)
summary(fit.loess)

plot(eva, ev, t='l')
lines(fit.lowess)
plot(eva, fit.loess)
