# Missing value/imputation related package: Amelia
# Causal Impact: Causal Impact (https://www.youtube.com/watch?v=GTgZfCltMm8)
# Granger causality: 
# Pearl's structural causation model R package(dagitty)
# lavaan (latent variable analysis)
# factanal

library(zoo)
library(stats)

a = seq(1:20)
a = sample(1:1000, 10)
# length = n
# k
# n - k + 1

k = 3
a_r = rollmean(x=a, k, align='center')
length(a_r)

#Using stats filter
slide_mean <- function(x, k) {
  filt = rep(1,k)/k
  x = stats::filter(x, filt)
  x = na.trim(x)
  x = as.numeric(as.character(x))
  x
}
 
a_f = slide_mean(a, k)
a_r 
a_f

a = sample(1:10, 20, replace = T)
a_r2 = rollapply(zoo(a), width=c(5,3,2), FUN=sum, na.rm=TRUE, 
                         by=c(5,3,2), 
                 align='left')
a
a_r2
#----------------------------------------------------------------------------------

# Cenozoic-Campanian Marine Oxygen-18 Composite (per-mil PDB)
data_dir <- '/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Datapacks'
setwd(data_dir)
dp_fname <- 'Phan_GTS2016_for_7.1_HaqJur_ForamMikrotax_28July2017.xls'
library(readxl)
sheets <- excel_sheets(dp_fname)
sheets
dfxl <- read_excel(dp_fname) #, sheet=sheets[1], range = "A13:F1987")
c <- colnames(dfxl) # column header
c

# cenozoic oxy-18 row 
ages <- dfxl$`1.3`
oxy_18 <- dfxl$X__1
start_r <- 28227
end_r <- 31270
oxy_ages <- as.numeric(ages[start_r:end_r])
oxy_18 <- as.numeric(oxy_18[start_r:end_r])

plot(oxy_ages, -oxy_18, 
     t='l', yaxt='n',
     xlab='age (Ma)',
     ylab='Oxygen-18 (per-mil PDB)',
     xlim=c(0, max(FAD_LAD_per_df_ap$age)),
     main = 'Benthic oxygen-18 isotope curve during Cenozoic era'
)
yv <- pretty(-oxy_18)
ylbls <- -yv
axis(2, at=yv, labels = ylbls)
arrows(10, -1, 10, 0.5, col=2, lwd=2)
mtext("High Temp",  side = 3, at = c(16), line= -3.5, col=2)
arrows(10, -3, 10, -4.5, col='blue', lwd=2)
mtext("Low Temp",  side = 3, at = c(16), line= -10, col='blue', lwd=2)

cenozoic_oxy18_df <- data.frame(age=oxy_ages, oxy_18=oxy_18)

library(zoo)
cenozoic_oxy18_df <- na.trim(cenozoic_oxy18_df)
head(cenozoic_oxy18_df)
tail(cenozoic_oxy18_df)

min_age <- min(cenozoic_oxy18_df$age, na.rm = T)
mn_a <- floor(min_age) - 0.5
max_age <- max(cenozoic_oxy18_df$age)
mx_a <- ceiling(max_age) + 0.5

slide_window <- 1
age_seq <- seq(mn_a, mx_a, by=slide_window)
ages <- list()
oxy_18_avg <- list()

i <- 1
for(a in age_seq) {
  age <- (age_seq[i] + age_seq[i+1])/2
  ix <- which(cenozoic_oxy18_df$age >= age_seq[i] & cenozoic_oxy18_df$age < age_seq[i+1])
  nix <- length(ix)
  
  ages[i] <- age
  oxy_18_avg[i] <- sum(cenozoic_oxy18_df$oxy_18[ix])/nix
  
  i <- i+1
}

oxy_18_avg_df <- data.frame(age=unlist(ages),
                            oxy_18_avg=unlist(oxy_18_avg))

head(oxy_18_avg_df)
tail(oxy_18_avg_df)

plot(cenozoic_oxy18_df$age, cenozoic_oxy18_df$oxy_18, t='l')
par(new=T)
plot(ages, oxy_18_avg, col=2, axes = F, xlab='', ylab='')

# Phanerozoic C-13 data extraction
ages <- dfxl$`1.3`
carbon_13 <- dfxl$X__1
carbon_13_info <- dfxl$X__2
start_r <- 32996
end_r <- 38049
carbon_ages <- as.numeric(ages[start_r:end_r])
carbon_13 <- as.numeric(carbon_13[start_r:end_r])


carbon_13_df <- data.frame(age=carbon_ages, c13=carbon_13)

min_age <- min(carbon_13_df$age, na.rm = T)
mn_a <- floor(min_age) - 0.5
max_age <- max(carbon_13_df$age)
mx_a <- ceiling(max_age) + 0.5

slide_window <- 1
age_seq <- seq(mn_a, mx_a, by=slide_window)
ages <- list()
carbon_13_avg <- list()

i <- 1
for(a in age_seq) {
  age <- (age_seq[i] + age_seq[i+1])/2
  ix <- which(carbon_13_df$age >= age_seq[i] & carbon_13_df$age < age_seq[i+1])
  nix <- length(ix)
  
  ages[i] <- age
  carbon_13_avg[i] <- sum(carbon_13_df$c13[ix])/nix
  
  i <- i+1
}

carbon_13_avg_df <- data.frame(age=unlist(ages),
                            c13_avg=unlist(carbon_13_avg))
carbon_13_avg_dff <- data.frame(na.approx(carbon_13_avg_df))

head(carbon_13_avg_df)
plot(carbon_ages, carbon_13, 
     t='l', yaxt='n',
     xlab='age (Ma)',
     ylab='C-13 (per-mil PDB)',
     main = 'Phanerozoic C-13'
)
par(new=T)
plot(carbon_13_avg_df$age, carbon_13_avg_df$c13_avg, t='l', col=2, axes=F, xlab="", ylab="")

# Phanerozoic Sr87/86 ratio data extraction
ages <- dfxl$`1.3`
sr87_86 <- dfxl$X__1
sr87_86_info <- dfxl$X__2
start_r <- 38874
end_r <- 39214
sr_ages <- as.numeric(ages[start_r:end_r])
sr87_86 <- as.numeric(sr87_86[start_r:end_r])
sr87_86_info <- sr87_86_info[start_r:end_r]

plot(sr_ages, sr87_86, 
     t='l', yaxt='n',
     xlab='age (Ma)',
     ylab='sr87/86 (ratio)',
     main = 'Phanerozoic Sr 87/86 ratio'
)
points(sr_ages, sr87_86, cex=0.5)
sr87_86_df <- data.frame(age=sr_ages, sr87_86=sr87_86)
head(sr87_86_df)
tail(sr87_86_df)

min_age <- min(sr87_86_df$age, na.rm = T)
mn_a <- floor(min_age) - 0.5
max_age <- max(sr87_86_df$age)
mx_a <- ceiling(max_age) + 0.5

slide_window <- 1
age_seq <- seq(mn_a, mx_a, by=slide_window)
ages <- list()
sr87_86_avg <- list()

i <- 1
for(a in age_seq) {
  age <- (age_seq[i] + age_seq[i+1])/2
  ix <- which(sr87_86_df$age >= age_seq[i] & sr87_86_df$age < age_seq[i+1])
  nix <- length(ix)
  
  ages[i] <- age
  sr87_86_avg[i] <- sum(sr87_86_df$sr87_86[ix])/nix
  
  i <- i+1
}

sr87_86_avg_df <- data.frame(age=unlist(ages),
                             sr87_86=unlist(sr87_86_avg))
sr87_86_avg_dff <- data.frame(na.approx(sr87_86_avg_df))

head(sr87_86_avg_dff)
tail(sr87_86_avg_dff)

plot(sr87_86_avg_df$age, sr87_86_avg_df$sr87_86, 
     t='l', yaxt='n',
     xlab='age (Ma)',
     ylab='Sr 87/86 ratio',
     main = 'Phanerozoic Sr 87/86'
)
par(new=T)
plot(sr87_86_avg_dff$age, sr87_86_avg_dff$sr87_86, t='l', col=2, axes=F, xlab="", ylab="")

# Sea Level Data extraction
dir <- ("~/Dropbox/TSCreator/TSCreator development/Developers/Andy/Datapacks/")
dp_fname <- paste(dir, "Phan_GTS2016_for_7.1_HaqJur_ForamMikrotax_28July2017.xls", sep="")
library(readxl)
dfxl <- read_excel(dp_fname)

#Long-Term Phanerozoic (SEPM98-Haq'08) data
#Long-Term Phanerozoic (SEPM98-Haq'08) data
# ages
lta <- as.numeric(dfxl$`1.3`[25253:26029])
# sea level
ltsl <- as.numeric(dfxl$X__1[25253:26029])

ltsl_df <- data.frame(Age=lta, SL=ltsl)

plot(ltsl_df, t='l')
points(ltsl_df, cex=0.25)

# Mean Sea Level (intermediate term; SEPM-Haq'08 synthetic)
# age
msla <- as.numeric(dfxl$`1.3`[24873:25246])
# sea level
msl <- as.numeric(dfxl$X__1[24873:25246])

msl_df <- data.frame(Age=msla, SL=msl)

plot(msl_df, t='l')
points(msl_df, cex=0.25)
par(new=T)
plot(msl_avg_dff$age, msl_avg_dff$msl_avg, t='l', axes=F, xlab="", ylab='', col='green')
points(msl_avg_dff$age, msl_avg_dff$msl_avg, col="green", cex=0.5)

# Short-Term Phanerozoic
# ages
sta <- as.numeric(dfxl$`1.3`[24220:24867])
# sea level
stsl <- as.numeric(dfxl$X__1[24220:24867])

rm(stsl_df)
stsl_df <- data.frame(Age=sta, SL=stsl)

plot(stsl_df$Age, stsl_df$SL, t='l')
points(stsl_df$Age, stsl_df$SL, cex=0.15)

# create cenozoic sea level
head(msl_df)
min_age <- min(msl_df$Age, na.rm = T)
mn_a <- floor(min_age) - 0.5
max_age <- max(msl_df$Age)
mx_a <- ceiling(max_age) + 0.5

slide_window <- 1
age_seq <- seq(mn_a, mx_a, by=slide_window)
ages <- list()
msl_avg <- list()

i <- 1
no_data_ix <- c()
for(a in age_seq) {
  age <- (age_seq[i] + age_seq[i+1])/2
  ix <- which(msl_df$Age >= age_seq[i] & msl_df$Age < age_seq[i+1])
  nix <- length(ix)
  if (nix == 0)
    no_data_ix <- c(no_data_ix, i)
  
  ages[i] <- age
  msl_avg[i] <- sum(msl_df$SL[ix])/nix
  
  i <- i+1
}

msl_avg_df <- data.frame(age=unlist(ages),
                            msl_avg=unlist(msl_avg))
# The above approach still produces some NAN values as for some year range
# there is no data
# use approx
msl_avg_dff <- data.frame(na.approx(msl_avg_df))


