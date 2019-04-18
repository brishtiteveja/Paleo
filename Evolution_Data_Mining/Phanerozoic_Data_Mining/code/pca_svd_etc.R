X0 <- genera_df[genera_df$age > 0 & genera_df$age < 70,]
X0 <- na.trim(X0)
head(X0)
tail(X0)
dim(X0)
#X1 <- FAD_LAD_per_df[FAD_LAD_per_df$age < 70,] # cenozoic planktonic foraminifera
#X1 <- na.trim(X1)
#head(X1)
#tail(X1)
#dim(X1)
X2 <- oxy_18_avg_df[oxy_18_avg_df$age > 0 & oxy_18_avg_df$age < 70, ] # salinity and temperature
X2 <- na.trim(X2)
head(X2)
tail(X2)
dim(X2)
X3 <- carbon_13_avg_dff[carbon_13_avg_dff$age > 0 & carbon_13_avg_dff$age < 70, ] # indicates proportion of carbon of biogenic origin
X3 <- na.trim(X3)
head(X3)
tail(X3)
dim(X3)
X4 <- sr87_86_avg_dff[sr87_86_avg_dff$age > 0 & sr87_86_avg_dff$age < 70, ] # continental erosion, plate tectonics https://www.le.ac.uk/gl/art/gl209/lecture2/lect2-6.html
X4 <- na.trim(X4)
head(X4)
tail(X4)
dim(X4)
X5 <- prokoph_df[prokoph_df$Age.Ma. > 0 & prokoph_df$Age.Ma. < 70, ]
X5 <- na.trim(X5)
head(X5)
tail(X5)
dim(X5)
X6 <- LIP_avg_dff[LIP_avg_dff$age > 0 & LIP_avg_dff$age < 70, ]
X6 <- na.trim(X6)
head(X6)
tail(X6)
dim(X6)
X7 <- impact_avg_dff[impact_avg_dff$age > 0 & impact_avg_dff$age < 70, ] 
X7 <- na.trim(X7)
head(X7)
tail(X7)
dim(X7)
X8 <- passive_margin_avg_dff[passive_margin_avg_dff$age > 0 & passive_margin_avg_dff$age < 70,]
X8 <- na.trim(X8)
head(X8)
tail(X8)
dim(X8)
X9 <- msl_avg_dff[msl_avg_dff$age > 0 & msl_avg_dff$age < 70, ] 
X9 <- na.trim(X9)
head(X9)
tail(X9)
dim(X9)

X <- data.frame(age=X2$age, 
                #ev_marine=X0$freq,
                genera_ts=X0$N.turnover, 
                genera_prokoph=X5$Marine.genera,
                oxy_18=-X2$oxy_18_avg,
                carbon_13=X4$c13_avg,
                sr87_86=X5$sr87_86,
                s34=X5$δ34S,
                LIP=X6$LIP_avg_cnt,
                impact=X7$impact_avg_cnt,
                passive_margin=X8$margin_oc_avg_cnt,
                sl=X3$msl_avg)


head(X)
tail(X)
plot(X)

# compute correlation
cor(X[,-1])
plot(X$age, X$ev, t='l')
points(X$age, X$ev, cex=0.5)
par(new=T)
plot(X$age, -X$oxy_18, t='l', axes=F, xlab="", ylab='', col='red')
points(X$age, -X$oxy_18, col=2, cex=0.5)
par(new=T)
plot(X$age, X$sl, t='l', axes=F, xlab="", ylab='', col='green')
points(X$age, X$sl, col="green", cex=0.5)


# unnormalized data
Xm = as.matrix(X[,2:3])
Xm = t(Xm)

# Y: normalized data
# Y = X_norm
Y = Xm
#Y[1,] = (Y[1,] - mean(Y[1,]))/sd(Y[1,])
#Y[2,] = (Y[2,] - mean(Y[2,]))/sd(Y[2,])

for(i in 1:dim(Xm)[1]) {
  Y[i,] = (Y[i,] - mean(Y[i,]))/sd(Y[i,])
}
hist(Xm[1,])
hist(Y[1,])
hist(Xm[2,])
hist(Y[2,])
plot(Y[1,], Y[2,])
plot(X$age, Y[1,], t='l')
par(new=T)
plot(X$age, Y[2,], t='l', col=2, axes=F, xlab="", ylab="")

XXt = Xm %*% t(Xm)
XtX = t(Xm) %*% Xm

YYt = Y %*% t(Y)
YtY = t(Y) %*% Y

# C: covariance matrix
C = XXt
C_norm = YYt

# Eigen value decomposition
E=eigen(C_norm)
Ev = E$values
Evm = diag(Ev)
round(Ev,5)
# EV : eigen vector ~ empirical orthogal vector
EV = E$vectors

# EtE = I
round(t(EV) %*% EV)


# Z: principal component matrix
# z_ij = principal component
# Xm = EZ
#Z = t(EV) %*% Xm
Z = t(EV) %*% Y
Z_norm = diag(Ev^(-1/2)) %*% Z
round(Z %*% t(Z))
round(Z_norm %*% t(Z_norm))

# Variance explained
l1 = Ev[1]/sum(Ev) 
l1 * 100
l2 = Ev[2]/sum(Ev)
l2 * 100


# SVD : Singular value decomposition
SS = svd(Xm)
# U: left singular vector contains state vectors per time
U = SS$u
S = SS$d
# singular value matrix : singular value contains all amplitude information
Sm = diag(S)
# V: right singular vector contains temporal(time) structure (normalized temporal variation of the amplitude of the state structure function)
V = SS$v
dim(U)
length(S)
dim(V)


# D_EOF
# eigen vectors corresponding to the amplitude in the data
D_EOF = EV %*% sqrt(Evm)

# D_SVD
D_SVD = U %*% diag(S)
D_SVD * N^(-1/2)

# principal component matrix
Z_EOF = t(E) %*% Y
#Z_SVD = Sm %*% 
  

identifyPch <- function(x, y = NULL, n = length(x), plot = FALSE, pch = 19, ...)
{
  xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
  sel <- rep(FALSE, length(x))
  while(sum(sel) < n) {
    ans <- identify(x[!sel], y[!sel], labels = which(!sel), n = 1, plot = plot, ...)
    if(!length(ans)) break
    ans <- which(!sel)[ans]
    points(x[ans], y[ans], pch = pch)
    sel[ans] <- TRUE
  }
  ## return indices of selected points
  print(which(sel))
}

if(dev.interactive()) { ## use it
  x <- seq(1, length(V[,2])); y <- V[,2]
  plot(x,y); identifyPch(x,y) # how fast to get all?
}

dev.off()
