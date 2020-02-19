data_dir <- '/Users/andy/Documents/projects/ML-Data Mining/Evolution_Data_Mining/Astronomical_data'
setwd(data_dir)
dp_fname <- 'milankovitch.xls'
library(readxl)
sheets <- excel_sheets(dp_fname)
sheets
dfxl <- read_excel(dp_fname, sheet=sheets[1])#, range = "A13:F1987")
c <- colnames(dfxl) # column header
c

orbit_df <- data.frame( age = dfxl$time,
                        eccentricity = dfxl$eccentricity,
                        obliquity = dfxl$obliquity,
                        perihelion = dfxl$perihelion,
                        insolation = dfxl$insolation,
                        global.insolation = dfxl$global.insolation)
orbit_dff <- orbit_df[orbit_df$age >= 0,]

plot(orbit_dff$age, orbit_dff$eccentricity, t='l')
plot(orbit_dff$age, orbit_dff$obliquity, t='l')
plot(orbit_dff$age, orbit_dff$global.insolation, t='l')

# tscreator milankovitch datapack
data_dir <- '/Users/andy/Documents/projects/ML-Data Mining/Evolution_Data_Mining/Astronomical_data'
setwd(data_dir)
dp_fname <- 'Astron_250myr_Insol_100myr_Mar10.xlsx'
library(readxl)
sheets <- excel_sheets(dp_fname)
sheets
dfxl <- read_excel(dp_fname, sheet=sheets[1])
c <- colnames(dfxl) # column header
c

row_start <- 7
row_end <- 50007
insolation_age <- as.numeric(dfxl$`1.2`[row_start:row_end])
insolation_65N <- as.numeric(dfxl$X__1[row_start:row_end])

row_start <- row_end+5
row_end <- 174512
eccentricity_age <- as.numeric(dfxl$`1.2`[row_start:row_end])
eccentricity <- as.numeric(dfxl$X__1[row_start:row_end])

row_start <- row_end+5
row_end <- 301059
precession_age <- as.numeric(dfxl$`1.2`[row_start:row_end])
precession <- as.numeric(dfxl$X__1[row_start:row_end])

row_start <- row_end+5
row_end <- 373159
obliquity_age <- as.numeric(dfxl$`1.2`[row_start:row_end])
obliquity <- as.numeric(dfxl$X__1[row_start:row_end])

par(mfrow=c(4,1))
par(mar=c(2,4,2,1))
mx_age <- 34
i_id <- which(insolation_age <= mx_age)
plot(insolation_age[i_id], insolation_65N[i_id], t='l')
e_id <- which(eccentricity_age <= mx_age)
plot(eccentricity_age[e_id], eccentricity[e_id], t='l')
o_id <- which(obliquity_age <= mx_age)
plot(obliquity_age[o_id], obliquity[o_id], t='l')
p_id <- which(precession_age <= mx_age)
plot(precession_age[p_id], precession[p_id], t='l')


obliquity_df <- data.frame(age=obliquity_age[o_id],
                           obliquity=obliquity[o_id])
eccentricity_df <- data.frame(age=eccentricity_age[e_id],
                              eccentricity=eccentricity[e_id])
precession_df <- data.frame(age=precession_age[p_id],
                            precession=precession[p_id])

library(zoo)
on <- rollapply(obliquity_df, width=100, FUN=mean)
a_l <- 0
a_u <- 8
par(mfrow=c(4,1))
plot(obliquity_df, t='hist', xlim=c(a_l, a_u))
par(new=T)
plot(on, t='l', col='red',axes=F, xlim=c(a_l, a_u))



# eccentricity
par(mfrow=c(1,1))
plot(eccentricity_df, t='l')
#upper cover
library(astrochron)
pd <- peak(eccentricity_df, genplot=F)
e_up_cov_orig <- data.frame(age=pd$Location, e_val=pd$Peak_Value)
lines(e_up_cov_orig, col='black', lwd=3)
e_up_cov <- rollapply(e_up_cov_orig, width=3, by=1, FUN=mean)
lines(e_up_cov, col='red', lwd=5)
e_t <- linterp(eccentricity_df, dt=0.001)
plot(eccentricity_df, cex=0.025)
mtm(e_t)
e_up_cov_t <- linterp(e_up_cov_orig, dt=0.001)
plot(e_up_cov_t, cex=0.05)
mtm(e_up_cov_t)
mtm(eccentricity_df)
#lower cover
pd2 <- trough(eccentricity_df, genplot=F)
e_down_cov_orig <- data.frame(age=pd2$Location, e_val=pd2$Trough_Value)
lines(e_down_cov_orig, col='black', lwd=3)
e_down_cov <- rollapply(e_down_cov_orig, width=3, by=1, FUN=mean)
lines(e_down_cov, col='red', lwd=5)
e_down_cov_t <- linterp(e_down_cov_orig, dt=0.001)
mtm(e_down_cov_t)


band


#obliquity
par(mfrow=c(1,1))
plot(obliquity_df, t='l')
#upper cover
pd <- peak(obliquity_df, genplot = F)
o_up_cov_orig <- data.frame(age=pd$Location, o_val=pd$Peak_Value)
lines(o_up_cov_orig, col='red', lwd=3)
o_up_cov <- rollapply(o_up_cov_orig, width=3, by=1, FUN=mean)
lines(o_up_cov, col='blue', lwd=2)
o_t <- linterp(obliquity_df)
mtm(obliquity_df)
o_up_cov_t <- linterp(o_up_cov_orig, dt=0.001)
head(o_up_cov_t)
mtm(o_up_cov_t)
#lower cover
pd2 <- trough(obliquity_df, genplot=F)
o_down_cov_orig <- data.frame(age=pd2$Location, o_val=pd2$Trough_Value)
lines(o_down_cov_orig, col='red', lwd=3)
o_down_cov <- rollapply(o_down_cov_orig, width=3, by=1, FUN=mean)
lines(o_down_cov, col='blue', lwd=2)
o_down_cov_t <- linterp(o_down_cov_orig, dt=0.001)
mtm(o_down_cov_t)

#precession
par(mfrow=c(1,1))
plot(precession_df, t='l')
#upper cover
pd <- peak(precession_df, genplot = F)
p_up_cov_orig <- data.frame(age=pd$Location, o_val=pd$Peak_Value)
lines(p_up_cov_orig, col='red', lwd=3)
p_up_cov <- rollapply(p_up_cov_orig, width=3, by=2, FUN=mean)
lines(p_up_cov, col='blue', lwd=2)
p_t <- linterp(precession_df)
mtm(precession_df)
p_up_cov_t <- linterp(p_up_cov_orig, dt=0.001)
head(p_up_cov_t)
mtm(p_up_cov_t)
#lower cover
pd2 <- trough(precession_df, genplot=F)
p_down_cov_orig <- data.frame(age=pd2$Location, o_val=pd2$Trough_Value)
lines(p_down_cov_orig, col='red', lwd=3)
p_down_cov <- rollapply(p_down_cov_orig, width=3, by=1, FUN=mean)
lines(p_down_cov, col='blue', lwd=2)
p_down_cov_t <- linterp(p_down_cov_orig, dt=0.001)
mtm(p_down_cov_t)

#insolation
par(mfrow=c(1,1))
insolation_df <- insolation_65N
plot(insolation_df, t='l')
#upper cover
pd <- peak(insolation_df, genplot = F)
i_up_cov_orig <- data.frame(age=pd$ID, o_val=pd$Peak_Value)
#lines(i_up_cov_orig, col='red', lwd=1)
i_up_cov <- rollapply(i_up_cov_orig, width=10, by=5, FUN=mean)
lines(i_up_cov, col='blue', lwd=2)
i_t <- linterp(insolation_df)
mtm(insolation_df)
i_up_cov_t <- linterp(i_up_cov_orig, dt=0.001)
head(i_up_cov_t)
mtm(i_up_cov_t)
#lower cover
pd2 <- trough(insolation_df, genplot=F)
i_down_cov_orig <- data.frame(age=pd2$Location, i_val=pd2$Trough_Value)
lines(i_down_cov_orig, col='red', lwd=3)
i_down_cov <- rollapply(i_down_cov_orig, width=3, by=1, FUN=mean)
lines(i_down_cov, col='blue', lwd=2)
i_down_cov_t <- linterp(i_down_cov_orig, dt=0.001)
mtm(i_down_cov_t)



y <- obliquity_df$obliquity
y <- y - mean(y)

plot(y, t='l')
N <- length(y) * 0.05
win <- N
taper <- 0.5

library(spectral)

w <- win.tukey(y, a=0.025)
plot(y, t='l')
lines(w, col=3)

r <- acf(y, lag.max = N-1, plot=TRUE)
r$acf

library(ssa)

BT <- btpsd(y, type="Tukey", win=N, taper=0.5)

plot(BT, t='l')
