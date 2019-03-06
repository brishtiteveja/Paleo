# Function 1 ( Bloomfield P. 52 Truncated Sinusoid) - simple sinusoidal with frequency f_0
# Fourier transform vanishes if f neq f_0
fun1 <- function(t, m, n, f_0, phi) {
  x.t <- rep(0, n)
  
  for(i in 1:n) {
    if (i >= 0 && i < m)
      x.t[i] <- cos(2*pi*(f_0 * t[i] + phi))
    else
      x.t[i] <- 0
  }
  
  return(x.t)
}

m <- 100#75
n <- 100
f_0 <- 4 #0.25
phi <- 0
t <- seq(0, 1, by=1/n)[-1]
y <- fun1(t, m, n, f_0, phi)
plot(t, y, t='l')

# Fourier transform
d.f <- fft(y)
# Amplitude
R.f <- Mod(d.f)
# Phase
P.f <- Arg(d.f)
# Periodogram
I.f <- n * R.f^2

# Plot Periodogram
# Non zero at all of the fourier frequencies (f_j = j/n), even if f_0 is itself a fourier frequency 
plot(0:(n-1), R.f, type='h') #, ylim = c(0,1))
plot(0:(n-1), P.f, type='h')

# Function 2 ( Bloomfield P. 52 Quadratic function)
fun2 <- function(t, n) {
  x.t <- rep(0, n)
  
  for(i in 1:n) {
      nN <- 0L:(n-1)
      x.t[i] <- sum((i - (nN-1)/2)^2)
  }
  
  return(x.t)
}

n <- 100
t <- seq(0, 1, by=1/n)[-1]
y <- fun2(t, n)
plot(t, y, t='l')

# Fourier transform
y <- y-mean(y)
d.f <- fft(y)

# Amplitude
R.f <- Mod(d.f)
P.f <- Arg(d.f)
# Periodogram
I.f <- n * R.f^2

# Plot Periodogram
plot(0:(n-1), R.f, type='h') #, ylim = c(0,1))
plot(0:(n-1), P.f, type='h') #, ylim = c(0,1))


# Function 3 ( Bloomfield P. 52 Triangle Wave)
fun3 <- function(t, n) {
  x.t <- rep(0, n)
  
  for(i in 1:n) {
    if (i >= 0 && i <= n/2)
      x.t[i] <- i
    else
      x.t[i] <- n-i
  }
  
  return(x.t)
}

n <- 100
t <- seq(0, 1, by=1/n)[-1]
y <- fun3(t, n)
plot(t, y, t='l')

# Fourier transform
y <- y-mean(y)
d.f <- fft(y)

# Amplitude
R.f <- d.f * Conj(d.f)
# Periodogram
I.f <- n * R.f^2

# Plot Periodogram
plot(0:(n-1), R.f, type='h') #, ylim = c(0,1))

# Function 4 ( Bloomfield P. 47 Single impulse)
fun4 <- function(t, n) {
  x.t <- rep(0, n)
  set.seed(2^10)
  t_0 <- round(runif(1, 0, n), 0)
  for(i in 1:n) {
    if (i == t_0)
      x.t[i] <- 1
  }
  
  return(x.t)
}

n <- 100
t <- seq(0, 1, by=1/n)[-1]
y <- fun4(t, n)
plot(t, y, t='l')

# Fourier transform
y <- y-mean(y)
d.f <- fft(y)

# Amplitude
R.f <- d.f * Conj(d.f)
# Phase
P.f <- Arg(d.f)
# Periodogram
I.f <- n * R.f^2

# Plot Periodogram
plot(0:(n-1), R.f, type='h') #, ylim = c(0,1))
plot(0:(n-1), P.f, type='h') #, ylim = c(0,1))

# Function 5 ( Bloomfield P. 48 Step Function)
set.seed(2^10)
n <- 100
t_0 <- round(runif(1, 0, n), 0)
fun5 <- function(t, n) {
  x.t <- rep(0, n)
  for(i in 1:n) {
    if (i < t_0)
      x.t[i] <- 1
  }
  
  return(x.t)
}

t <- seq(0, 1, by=1/n)[-1]
y <- fun5(t, n)
plot(t, y, t='l')

# Fourier transform
y <- y-mean(y)
d.f <- fft(y)

# Amplitude
R.f <- Mod(d.f)
# Phase
P.f <- Arg(d.f)
# Periodogram
I.f <- n * R.f^2

# Plot Periodogram
plot(0:(n-1), R.f, type='l') #, ylim = c(0,1))
# Amplitude is dirichlet kernel which decays roughly as 1/f
plot(0:(n-1), P.f, type='h') #, ylim = c(0,1))

# Dirichlet kernel
D_n <- function(n, f, plot=FALSE) {
  x <- f
  y <- sin((n+1/2) * x) / (sin(x/2))
  
  if (plot)
    plot(x, y, t='l')

  return(y)
}

m <- 1
f <- 1:(n-1)
D_n(m, f, TRUE)
m <- 10
f <- 1:(n-1)
D_n(m, f, TRUE)

m <- t_0
y <- (m/n) * abs(D_n(m, f))
plot(f, y, t='l')

fun4 <- function(t, n) {
  x.t <- rep(0, n)
  set.seed(2^10)
  t_0 <- round(runif(1, 0, n), 0)
  for(i in 1:n) {
    if (i == t_0)
      x.t[i] <- 1
  }
  
  return(x.t)
}

n <- 100
t <- seq(0, 1, by=1/n)[-1]
y <- fun4(t, n)
plot(t, y, t='l')

# Fourier transform
y <- y-mean(y)
d.f <- fft(y)

# Amplitude
R.f <- d.f * Conj(d.f)
# Phase
P.f <- Arg(d.f)
# Periodogram
I.f <- n * R.f^2

# Plot Periodogram
plot(0:(n-1), R.f, type='h') #, ylim = c(0,1))
plot(0:(n-1), P.f, type='h') #, ylim = c(0,1))

# Function 6 ( Bloomfield P. 48 Straight Line)
fun6 <- function(t, n) {
  x.t <- rep(0, n)
  for(i in 1:n) {
      x.t[i] <- i - (n-1) / 2
  }
  
  return(x.t)
}

n <- 100
t <- seq(0, n-1, by=1)
y <- fun6(t, n)
plot(t, y, t='l')

# Fourier transform
y <- y-mean(y)
d.f <- fft(y)
# Amplitude
R.f <- Mod(d.f)
# Phase
P.f <- Arg(d.f)
# Periodogram
I.f <- n * R.f^2

# Plot Periodogram
plot(0:(n-1), R.f, type='l') #, ylim = c(0,1))
# amplitude decays with 1/f

# Amplitude decays like the following function
ff <- function (x) {
  y <- (pi * x * cos(pi * x) - sin(pi * x))/ (pi * x)^2
  return (y)
}

x <- seq(-4, 4, by=0.001)
y <- ff(x)
plot(x, y, t='l')

plot(0:(n-1), P.f, type='h') #, ylim = c(0,1))


# Dirichlet kernel
D_n <- function(n, f, plot=FALSE) {
  x <- f
  y <- sin((n+1/2) * x) / (sin(x/2))
  
  if (plot)
    plot(x, y, t='l')
  
  return(y)
}

m <- 1
f <- 1:(n-1)
D_n(m, f, TRUE)
m <- 10
f <- 1:(n-1)
D_n(m, f, TRUE)

m <- t_0
y <- (m/n) * abs(D_n(m, f))
plot(f, y, t='l')

# Function 6 ( Bloomfield P. 49 shifted series)

funShift <- function(x, h) {
  n <- length(x)
  y <- rep(0, n)
  for (t in 1L:n) {
    i <- ((t+h) %%n)  + 1
    y[t] <- x[i]
  }
  
  return(y)
}

n <- 100
f <- 1
y <- sin(2*pi*f*x)
plot(x, y, t='l')
y.shifted <- funShift(y, 8)
lines(x, y.shifted, col='red', lty=2)

# Fourier transform
d.f <- fft(y)
d.f.shifted <- fft(y.shifted)

# Amplitude
R.f <- Mod(d.f)
R.f.shifted <- Mod(d.f.shifted)

# Phase
P.f <- Arg(d.f)
P.f.shifted <- Arg(d.f.shifted)

# Periodogram
I.f <- n * R.f^2
I.f.shifted <- n * R.f.shifted^2

# Plot Periodogram
plot(0:(n-1), R.f, type='l') #, ylim = c(0,1))
lines(0:(n-1), R.f.shifted, col='red', lty=2)

# amplitude decays with 1/f
plot(0:(n-1), P.f, type='l') #, ylim = c(0,1))
lines(0:(n-1), P.f.shifted, type='l', col=2, lty=2)

# Function 7 ( Bloomfield P. 49 symmetric series)

funSymmetry <- function(x, anti=FALSE) {
  n <- length(x)
  y <- rep(0, n)
  for (t in 1L:n) {
    if (anti)
      y[t] <- -1 * x[t]
    else
      y[t] <- x[t]
  }
  
  return(y)
}

n <- 100
x <- seq(0, by=1/n, length.out=n)
f1 <- 1
f2 <- 3
y <- 2*sin(2*pi*f1*x) + 0.5 * sin(2*pi*f2*x)
y.sym <- funSymmetry(y)
y.antisym <- funSymmetry(y, TRUE)
plot(x, y, t='l', xlim=c(min(-x), max(x)))
lines(-x, y.sym, col='red', lty=2)
lines(-x, y.antisym, col='green', lty=2)


# Fourier transform
d.f <- fft(y)
round(d.f) # Amplitude purely imaginary

d.f.sym <- fft(y.sym)
round(d.f.sym)

d.f.antisym <- fft(y.antisym)
round(d.f.antisym)

# Amplitude
R.f <- Mod(d.f)
R.f.sym <- Mod(d.f.sym)

# Phase
P.f <- Arg(d.f)
P.f.sym <- Arg(d.f.sym)

# Periodogram
I.f <- n * R.f^2
I.f.sym <- n * R.f.sym^2

# Plot Periodogram
plot(0:(n-1), R.f, type='l') #, ylim = c(0,1))
lines(0:(n-1), R.f.sym, col='red', lty=2)

# amplitude decays with 1/f
plot(0:(n-1), P.f, type='l') #, ylim = c(0,1))
lines(0:(n-1), P.f.sym, type='l', col=2, lty=2)



# Function 8 ( Bloomfield P. 49 periodic series: if more than 1 cycle in t=(0,1))
n <- 100
t <- seq(0, by=1/n, length.out=n)
f <- 1
y <- sin(2*pi*f*t)

plot(t, y, t='l')

# take 3 periodic cycles
t2 <- seq(0, by=1/n, length.out=3*n)
f <- 1
y2 <- sin(2*pi*f*t2)

plot(t2, y2, t='l')

# Fourier transform
d.f <- fft(y)
# Amplitude
R.f <- Mod(d.f)
# Phase
P.f <- Arg(d.f)
# Periodogram
I.f <- n * R.f^2

# Plot Periodogram
plot(0:(n-1), R.f, type='l') #, ylim = c(0,1))
# amplitude decays with 1/f
plot(0:(n-1), P.f, type='h') #, ylim = c(0,1))

# Fourier transform
d.f2 <- fft(y2)
# Amplitude
R.f2 <- Mod(d.f2)
# Phase
P.f2 <- Arg(d.f2)
# Periodogram
I.f2 <- n * R.f2^2

# Plot Periodogram
n2 <- length(d.f2)
plot(0:(n2-1), R.f2, type='l', xlim = c(0,n)) #, ylim = c(0,1))
# Fourier transform of the fundamental frequency is only non-zero.

# amplitude decays with 1/f
plot(0:(n2-1), P.f2, type='h', xlim = c(0, n)) #, ylim = c(0,1))


# 


# White noise and Red noise
----------------------------

#https://atmos.washington.edu/~breth/classes/AM582/lect/lect8-notes.pdf
# White noise has zero mean, constant variance, and is uncorrelated in time

n <- 100
w <- white_noise <- rnorm(n)
y <- w-mean(w)

plot(w, t='l')

# Fourier transform
d.f <- fft(y)

# Amplitude
R.f <- d.f * Conj(d.f)
# Periodogram
I.f <- n * R.f^2

# Plot Periodogram
plot(0:(n-1), R.f, pch=8) #, ylim = c(0,1))

# Red noise has zero mean, constant variance, and is serially correlated in time,
# such that the lag-1 autocorrelation between two successive time samples has
# correlation coefficient 0 < r < 1
red_noise <- function(n, w, cc) { #cc: correlation co-efficient
  r <- rep(0, n)
  r[1] = w[1]
  for (t in 2:n) {
    r[t] = cc * w[t-1] + sqrt(1-cc^2) * w[t]
  }
  
  return(r)
}

cc <- 0.75
r <- red_noise(n, w, cc)

y2 <- r-mean(r)
plot(y2, t='l')
lines(w, col='red')

# Fourier transform
d.f2 <- fft(y2)

# Amplitude
R.f2 <- d.f2 * Conj(d.f2)
# Periodogram
I.f2 <- n * R.f^2

# Plot Periodogram
plot(0:(n-1), R.f2, pch=8) #, ylim = c(0,1))
# low frequency amplitude is higher


