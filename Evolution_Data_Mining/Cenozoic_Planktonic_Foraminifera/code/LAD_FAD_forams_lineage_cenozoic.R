data_dir <- '/Users/andy/Dropbox/TSCreator/TSCreator_development/Developers/Andy/Projects/EvolutionaryTree/Fordham and Zehady shared/180724'
data_dir <- '/Users/andy/Documents/TSCreator/EvolutionaryTree/Fordham and Zehady shared/180724'
setwd(data_dir)
dp_fname <- 'qryTSCAze_BiospeciesAze_ColourMorphogroup.xls'
library(readxl)
# extract the FAD and LAD points from the tscreator datapack in excel format
sheets <- excel_sheets(dp_fname)
sheets
dfxl <- read_excel(dp_fname) #, sheet=sheets[1], range = "A13:F1987")
c <- colnames(dfxl) # column header
c

name_col <- dfxl$`1.3`
name_col
age_col <- dfxl$X__1
age_col
type_col <- dfxl$X__2
type_col
branch_to_col <- dfxl$X__3
popup <- dfxl$X__4
#branch_to_col
LAD_ix <- which(type_col == 'TOP')
LAD_ix
FAD_ix <- which(type_col == 'branch')
FAD_ix

pat<-'Provisional Binomial'
lineage_names <- c()
for (i in LAD_ix) {
  pos <- regexpr(pat, branch_to_col[i])
  start <- pos[1]
  end <- start + attr(pos, "match.length") + 100
  ns <- substr(a[i], start, end)
  x <- strsplit(ns, "<")[[1]]
  
  if (length(x) >= 2) {
    print(x[2])
    name <- strsplit(x[2], ">")[[1]]
    name <- name[2]
    #print(name)
  } else {
    name <- name_col[i]
    #print(name)
  }
  
  lineage_names <- c(lineage_names, name)
}

# creating data frame with LAD points per pseudolevel
lad_df <- data.frame(LAD=age_col[LAD_ix], name=name_col[LAD_ix])
lad_df <- lad_df[order(lad_df$LAD, decreasing = F),]
lad_df

# creating data frame for FAD points per pseudolevel
fad_df <- data.frame(FAD=age_col[FAD_ix], name=branch_to_col[FAD_ix])
fad_df <- fad_df[order(fad_df$FAD, decreasing = F),]
fad_df

# creating data frame to obtain FAD and LAD for every foram species
fad_lad_df <- fad_df
fad_lad_df$LAD <- rep(-1, dim(fad_lad_df)[1])
i <- 1
for(n in fad_lad_df$name) {
  ix <- which(lad_df$name == n)
  fad_lad_df$LAD[i] = lad_df$LAD[ix] 
  i <- i+1
}
head(fad_lad_df)
dim(fad_lad_df)

# Obtain the living species (LAD = 0)
living_ix <- which(fad_lad_df$LAD == 0)
living_df <- fad_lad_df[living_ix,]
living_df
dim(living_df)

# organisms in first pseudolevel
f_ix <- which(fad_lad_df$LAD > 0 & fad_lad_df$LAD <0.1)
length(f_ix)

# determine minimum and maximum age 
min_age <- min(fad_lad_df$LAD, fad_lad_df$FAD)
mn_a <- floor(min_age)
max_age <- max(fad_lad_df$LAD, fad_lad_df$FAD)
mx_a <- ceiling(max_age) + 1
mx_as <- 34


# Extract number of speciation and extinction per pseudolevel

# set the myr bin window (pseudolevel)
window_size <- 0.1
slide_window <- 0.1
age_seq <- seq(mn_a-window_size, mx_a+window_size, by=slide_window)
ages <- list()
LAD_cnts <- list()
FAD_cnts <- list()
LAD_names <- list()
FAD_names <- list()
n <- length(age_seq) - 1
fad <- round(as.numeric(fad_lad_df$FAD), 5)
lad <- round(as.numeric(fad_lad_df$LAD), 5)
for(k in 1:n) {
  start_age <- round(age_seq[k], 5)
  end_age <- round(start_age + window_size, 5)
  mid_age <- (start_age + end_age)/2
  ixl <- which(lad >= start_age 
              & lad < end_age & lad != 0) # also excludes the currently existing species

  ixf <- which(fad >= start_age 
               & fad < end_age)
  
  cnt_l = length(ixl)
  cnt_f = length(ixf)
  ln_l = list(levels(droplevels(fad_lad_df$name[ixl])))
  ln_f = list(levels(droplevels(fad_lad_df$name[ixf])))
  
  ages[[k]] <- mid_age
  LAD_cnts[[k]] <- cnt_l
  FAD_cnts[[k]] <- cnt_f
  LAD_names[[k]] <- ln_l
  FAD_names[[k]] <- ln_f
 }

FAD_LAD_per_df <- data.frame(age=unlist(ages), 
                         FAD_cnt = unlist(FAD_cnts), LAD_cnt = unlist(LAD_cnts))
FAD_LAD_per_df_orig <- FAD_LAD_per_df
FAD_LAD_per_df <- FAD_LAD_per_df[FAD_LAD_per_df$age>=mn_a & FAD_LAD_per_df$age <= mx_a,]
head(FAD_LAD_per_df)
tail(FAD_LAD_per_df)


library(zoo)
sdf <- data.frame(rollapply(FAD_LAD_per_df, width=10, by=0.1, FUN="mean"))
head(sdf)
plot(sdf, t='l')

dim(lad_df)

fad_lad_df <- fad_df
fad_lad_df$LAD <- rep(-1, dim(fad_lad_df)[1])
i <- 1
for(n in fad_lad_df$name) {
  ix <- which(lad_df$name == n)
  fad_lad_df$LAD[i] = lad_df$LAD[ix] 
  i <- i+1
}
# calculate Lifespan for each foram species
fad_lad_df$Lifespan <- fad_lad_df$FAD - fad_lad_df$LAD
head(fad_lad_df)
dim(fad_lad_df)


# calculate mean life span
# mean life span is defined by the average lifespan of species existing in each pseudolevel 
#fn <- sink('tmp.txt')
n <- length(age_seq)-1
rs <- rev(seq(1:n))
existing_s <- c()
existing_n <- data.frame()
existing_sp <- list()
mean_lifespan_per_level <- data.frame()
for(k in rs) {
  age <- as.numeric(unlist(ages[[k]]))
  FAD_s <- unlist(FAD_names[[k]])
  N_FAD_s <- length(FAD_s)
 
  existing_s <- unique(c(existing_s, FAD_s))
  
  LAD_s <- unlist(LAD_names[[k]])
  N_LAD_s <- length(LAD_s)
  
  
  id <- which(existing_s %in% LAD_s)
  
  n_b <- length(existing_s)
  
  if (length(id) != 0) {
    existing_s <- existing_s[-c(id)]
    
  }
  
  n_a <- length(existing_s)
  existing_sp[[k]] <- list()
  existing_sp[[k]]['age'] <- age
  existing_sp[[k]]['existing_species'] <- list(existing_s)
  fams <- c()
  fads <- c()
  for (s in existing_s) {
    ids <- which(fad_lad_df$name == s)
    fam <- fad_lad_df[ids,]$Family
    fams <- c(fams, fam)
    fad_s <- as.numeric(fad_lad_df[ids,]$FAD)
    fads <- c(fads, fad_s)
  }
  existing_sp[[k]]['FAD'] <- list(fads)
  existing_sp[[k]]['families'] <- list(fams)
  existing_n <- rbind(existing_n, data.frame(age,n_a))
  
  #get the mean lifespan for the existing species in the current period
  if (age > 0 & length(fads) != 0) {
    cur_age <- (age - window_size/2)
    mspl <- sum(fads - cur_age)/length(fads)
    mean_lifespan_per_level <- rbind(mean_lifespan_per_level,
                                     data.frame(age, mspl))
  } else {
    mspl <- 0
    mean_lifespan_per_level <- rbind(mean_lifespan_per_level,
                                     data.frame(age, mspl))
  }
}

existing_n <- data.frame(age=rev(existing_n$age), n_existing=rev(existing_n$n_a))
existing_n <- existing_n[existing_n$age>=mn_a & existing_n$age <= mx_a,]
head(existing_n)
tail(existing_n)

FAD_LAD_per_df$total <- FAD_LAD_per_df$LAD_cnt + FAD_LAD_per_df$FAD_cnt

library(DT)
datatable(FAD_LAD_per_df)

mean_lifespan_per_level <- mean_lifespan_per_level[order(mean_lifespan_per_level$age),] 
head(mean_lifespan_per_level)

df = data.frame(FAD_LAD_per_df)

# At each pseudolevel, we count the number of speciations or
# extinctions—encoded as counts in the code (Dataset S2)—and
# the total number of species extant. For extinction calculations,
# the count of extant species includes the extinguishers (which
# form part of the pool of species that are exposed to extinction
# risk) but not originators. Conversely, for speciation calculations,
# the count of extant species includes the originators but not the
# extinguishers. 
# N_FAD[i] : Number of species exists through speciation at period i 
# subtract the number of extinctions happened in current period i
# Add the number of speciations happened in current period i
# N_FAD[i] = N_FAD[i-1] - LAD[i] + FAD[i]

N_FAD <- rev(df$FAD_cnt) # rev(df1$`N speciations`)  
r_LAD <- rev(df$LAD_cnt) # rev(df1$`N extinctions`) 
r_FAD <- rev(df$FAD_cnt) # rev(df1$`N speciations`)

#N_FAD[1] <- 0
for(i in 2:length(N_FAD)) {
  N_FAD[i] = N_FAD[i-1] - r_LAD[i] + r_FAD[i]
}

# N_LAD[i] : Number of species exists at period i after extinction events in period (i-1)
# subtract the number of extinctions happened in previous period (i-1)
# Add the number of speciations happened in previous period (i-1)
# N_LAD[i] = N_LAD[i-1] - LAD[i-1] + FAD[i-1]

N_LAD <- rev(df$FAD_cnt) # rev(df1$`N extinctions`)
r_LAD <- rev(df$LAD_cnt) # rev(df1$`N extinctions`) 
r_FAD <- rev(df$FAD_cnt) # rev(df1$`N speciations`)  

#N_LAD[1] <- 0
for(i in 2:length(N_FAD)) {
  N_LAD[i] = N_LAD[i-1] - r_LAD[i-1] + r_FAD[i-1]
}

N_FAD_LAD = data.frame(N_FAD=rev(N_FAD), N_LAD=rev(N_LAD))

head(N_FAD_LAD)
tail(N_FAD_LAD)

df <- cbind(df, N_FAD_LAD)
#df[is.na(df)] <- 0
head(df)
tail(df)

# changing column names
df$`N.speciations` <- df$FAD_cnt
df$`N.extinctions` <- df$LAD_cnt
df$`N.turnover` <- df$FAD_cnt + df$LAD_cnt
df$`N.species.speciation` <- df$N_FAD
df$`N.species.extinction` <- df$N_LAD
df$`raw.speciation.probability` <- df$FAD_cnt/df$N_FAD
df$`raw.extinction.probability` <- df$LAD_cnt/df$N_LAD
#df[is.na(df)] <- 0

library(DT)
datatable(df)

# Extract only Cenozoic
CENOZOIC_BASE <- 80
df_C <- df[df$age > 0 & df$age <= CENOZOIC_BASE, ]
tail(df_C)

# Change the dff here depending on the time period being focused on
dff <- df_C

dff <- dff[,-c(2,3,4,5,6)]
dff$`raw.turnover.probability` <- dff$`raw.speciation.probability` + dff$`raw.extinction.probability`
datatable(dff)
which(is.na(dff))
head(dff)
tail(dff)

datatable(dff)

nn<- dim(mean_lifespan_per_level)[1]-1
dff$`mean.lifespan` <- mean_lifespan_per_level$mspl[2:nn]
library(DT)
datatable(dff)
write.table(fad_lad_df, "~/desktop/lineage_fad_lad.csv", sep=",")
write.table(dff, "~/desktop/foram_lineage_turnover_cenozoic.csv", sep=",")

write.table(dff[,c("age","mean.lifespan")], "a.csv", sep=",")

pf_diversity_per_level <- dff[, c("age", "N.species.speciation")] 
head(pf_diversity_per_level)

PFL_dff <- dff
datatable(dff)

dev.off()
plot.new()
par(mfrow=c(1,1))
par(mar=c(3,3,3,3))
plot(dff$age, dff$mean.lifespan, t='l',axes=F)
points(dff$age, dff$mean.lifespan, cex=0.2)
axis(1)
axis(2)
box()
par(new=T)
plot(dff$age, dff$N.species.speciation, t='l', col='green', axes=F)
points(dff$age, dff$N.species.speciation, cex=0.2, col='green')

sddf <- merge(mean_lifespan_per_level, pf_diversity_per_level, by="age")
datatable(sddf)

# draw foraminifer ranges
# plot by first occurrence
df <- fad_lad_df[order(-fad_lad_df$FAD),]
n <- dim(fad_lad_df)[1]
rdf <- data.frame(x0=seq(1, n),
                  y0=-df$FAD,
                  x1=seq(1, n),
                  y1=-df$LAD
                  )
rdf_orig <- rdf


m <- matrix(c(1,2,
              1,2), 2, 2, byrow = TRUE)
m
layout(m, widths=c(5,1))
layout.show(2)

par(mar=c(3,4,1,1))
x_m=max(rdf$x0)
y_m=min(rdf$y0, rdf$y1)
plot(0,0,cex=0.005, xlim=c(0,x_m), ylim=c(y_m, 0), xaxt='n', ylab='Age (Myr)')
segments(rdf$x0, rdf$y0, rdf$x1, rdf$y1) #, col=rdf$Family)

x<--FAD_LAD_per_df$age
y<-FAD_LAD_per_df$FAD_cnt
z <- rep(0,length(y))
par(mar=c(3,0,1,2))
x_m= max(y)
plot(0,0,cex=0.005,yaxt='n', xlim=c(0,x_m), ylim=c(y_m, 0))
segments(z,x,y,x)

# plot by last occurrence
df <- fad_lad_df[order(-fad_lad_df$LAD),]
n <- dim(fad_lad_df)[1]
rdf <- data.frame(x0=seq(1, n),
                  y0=-df$FAD,
                  x1=seq(1, n),
                  y1=-df$LAD,
                  Family=fad_lad_df$Family
)
head(rdf)

# plot by last occurrence
m <- matrix(c(1,2,
              1,2), 2, 2, byrow = TRUE)
m
layout(m, widths=c(5,1))
layout.show(2)

par(mar=c(3,4,1,1))
x_m=max(rdf$x0)
y_m=min(rdf$y0, rdf$y1)
plot(0,0,cex=0.005, xlim=c(0,x_m), ylim=c(y_m,0), xaxt='n', ylab='Age (Myr)')
segments(rdf$x0, rdf$y0, rdf$x1, rdf$y1, col=rdf$Family)

x<--FAD_LAD_per_df$age
y<-FAD_LAD_per_df$LAD_cnt
z <- rep(0,length(y))
par(mar=c(3,0,1,2))
x_m= max(y)
plot(0,0,cex=0.005,yaxt='n', xlim=c(0,x_m), ylim=c(y_m, 0))
segments(z,x,y,x)


#-------------

head(FAD_LAD_per_df)

N <- dim(FAD_LAD_per_df)[1]
#FAD_LAD_per_df_ap <- FAD_LAD_per_df[FAD_LAD_per_df$age >=0.15,]
PFL_FAD_LAD_per_df_ap <- FAD_LAD_per_df
head(PFL_FAD_LAD_per_df_ap)

par(mfrow=c(2,1))
par(mar=c(1,4,1,4))
width <- 10 # to obtain every million year
by <- 3
x <- rollapply(-PFL_FAD_LAD_per_df_ap$age, width=width, by=by, FUN='mean')
y <- rollapply(PFL_FAD_LAD_per_df_ap$LAD_cnt + PFL_FAD_LAD_per_df_ap$FAD_cnt,
               width=width, by=by, FUN='mean')
#x <- -PFL_FAD_LAD_per_df_ap$age
#y <- PFL_FAD_LAD_per_df_ap$LAD_cnt + PFL_FAD_LAD_per_df_ap$FAD_cnt
plot(x, 
     y, 
     t='l', 
     lwd=2,
     col=1, 
     #lty=2,
     xlim=c(-35,0),
     ylab='Number of events', #'event rate(%)',
     #ylim=c(0,max(FAD_LAD_per_df_ap$total) + 1),
     # main=paste('Speciation and extinction events of nannofossils\n',
     #            '(window size = ', window_size, 'Ma,\n',
     #            'slide =', slide_window , 'Ma)' ) 
)
points(x,y, cex=0.5)

y1 <- rollapply(PFL_FAD_LAD_per_df_ap$LAD_cnt,
                width=width, by=by, FUN='mean')
#y1 <- PFL_FAD_LAD_per_df_ap$LAD_cnt
lines(x, y1, #ages, extinction_rate, 
      lwd=2,
      col=2)
points(x, y1, cex=0.5, col='red')

y2 <- rollapply(PFL_FAD_LAD_per_df_ap$FAD_cnt,
                width=width, by=by, FUN='mean')
#y2 <- PFL_FAD_LAD_per_df_ap$FAD_cnt
lines(x, y2, #ages, speciation_rate, 
      lwd=2,
      col=3)
points(x, y2, cex=0.5, col='green')

legend(x=-30,y=2.3,
       legend=c('turnover(speciation + extinction)', 'extinction', 'speciation'),
       col=c(1,2,3), lty=c(1,1,1),
       cex=0.7)
#par(mfrow=c(1,1))
par(mar=c(3,4,1,4))
x <- rollapply(-mean_lifespan_per_level$age,
               width=width, by=by, FUN='mean')
y <- rollapply(mean_lifespan_per_level$mspl,
               width=width, by=by, FUN='mean')
x <- -PFL_dff$age
y <- PFL_dff$mean.lifespan
plot(x, y, 
     t='l', xlim=c(-35,0), 
     axes=F, lwd=2, col='black', 
     xlab="Age (Myr)", ylab="Mean Lifespan (Myr)")
#points(x, y, cex=0.5)
axis(1)
axis(2)
box()
xd <- rollapply(-pf_diversity_per_level$age,
               width=width, by=by, FUN='mean')
yd <- rollapply(pf_diversity_per_level$N.species.speciation,
               width=width, by=by, FUN='mean')
xd <- -PFL_dff$age
yd <- PFL_dff$N.species.speciation
par(new=T)
plot(xd, yd, t='l', col='orange',
     axes=F, xlab='', ylab='', lwd=2, xlim=c(-35,0))
#points(xd, yd, lwd=2, col='orange', cex=0.5)
axis(4)
mtext("Age (Myr)",side=1, line=2)
mtext("Number of living species",side=4, line=2)
legend(x=-30,y=20,
       legend=c('mean lifespan', 'number of living species'),
       col=c('black','orange'), lty=c(1,1), lwd=c(2,2),
       cex=0.8)


# write in to the datapack
save_datapack_dir <- '/Users/andy/Documents/projects/ML-Data Mining/Evolution_Data_Mining/Cenozoic_Planktonic_Foraminifera/datapack/'
organism_type <- 'Planktonic_foraminifer_morphospecies'
datapack_file_name <- paste(organism_type, '_frequency_window_size_', window_size, 
                            'myr_slide_window_', slide_window,'_myr_datapack.txt', sep="")
fn <- paste(save_datapack_dir, datapack_file_name, sep="")
msg <- paste("writing datapack ", fn, sep="")
print(msg)

sink(fn)
str <- 'format version:	1.3\nage units:	ma\n'
writeLines(str)

# write fad frequencies
event_type <- 'FAD'
str <- paste(organism_type, ' ', event_type, ' frequency for window size ', window_size, 'Ma slide window ',
             slide_window, 'Ma\tpoint\t150\t255/245/230', sep="")
writeLines(str)
nf <- dim(FAD_LAD_per_df)[1]
for (l in 1:nf) {
  a <- FAD_LAD_per_df$age[l]
  f <- FAD_LAD_per_df$FAD_cnt[l]
  ln <- paste("\t", a, "\t", f, sep='')
  writeLines(ln)
}

# write lad frequencies
str="\n"
writeLines(str)
event_type <- 'LAD'
str <- paste(organism_type, ' ',event_type, ' frequency for window size ', window_size, 'Ma slide window ',
             slide_window, 'Ma\tpoint\t150\t255/245/230', sep="")

writeLines(str)
nf <- dim(FAD_LAD_per_df)[1]
for (l in 1:nf) {
  a <- FAD_LAD_per_df$age[l]
  f <- FAD_LAD_per_df$LAD_cnt[l]
  ln <- paste("\t", a, "\t", f, sep='')
  writeLines(ln)
}

# write FAD + LAD frequencies
str="\n"
writeLines(str)
event_type <- 'FAD+LAD'
str <- paste(organism_type, ' ',event_type, ' frequency for window size ', window_size, 'Ma slide window ',
             slide_window, 'Ma\tpoint\t150\t255/245/230', sep="")

writeLines(str)
nf <- dim(FAD_LAD_per_df)[1]
for (l in 1:nf) {
  a <- FAD_LAD_per_df$age[l]
  f <- FAD_LAD_per_df$FAD_cnt[l] + FAD_LAD_per_df$LAD_cnt[l]
  ln <- paste("\t", a, "\t", f, sep='')
  writeLines(ln)
}

# write FAD - LAD frequencies
str="\n"
writeLines(str)
event_type <- 'FAD-LAD'
str <- paste(organism_type, ' ',event_type, ' frequency for window size ', window_size, 'Ma slide window ',
             slide_window, 'Ma\tpoint\t150\t255/245/230', sep="")

writeLines(str)
nf <- dim(FAD_LAD_per_df)[1]
for (l in 1:nf) {
  a <- FAD_LAD_per_df$age[l]
  f <- FAD_LAD_per_df$FAD_cnt[l] - FAD_LAD_per_df$LAD_cnt[l]
  ln <- paste("\t", a, "\t", f, sep='')
  writeLines(ln)
}
sink()

# Using multitaper
library(astrochron)
#chr_model <- linterp(chr_df, dt=1)
x <- FAD_LAD_per_df_ap$age
y <- FAD_LAD_per_df_ap$total
evolution_model <- linterp(data.frame(year=x, evolution=y), dt=0.1)
Mspec <- mtm(evolution_model, demean = T, detrend = T, 
             #ntap = 5, tbw = 3, #ar1 = T,
             #xmin = 0,
             #xmax = 0.1,
             output = 1, pl=2)
Mspecdf$period <- (1/Mspecdf$Frequency) # * (time_window / N))
Mspecdf <- Mspecdf[order(Mspecdf$period, decreasing = T),]
Mspecdf2 <- subset(Mspecdf, (Mspecdf$Harmonic_CL >=80 & Mspecdf$AR1_CL >= 80) | 
                     (Mspecdf$Harmonic_CL >=95) )
datatable(Mspecdf2[,c(1:4, 9)])

# Cenozoic-Campanian Marine Oxygen-18 Composite (per-mil PDB)
data_dir <- '/Users/andy/Dropbox/TSCreator/TSCreator_development/Developers/Andy/Projects/Datapacks'
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
     ylab='Oxygen-18 ()',
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
cenozoic_oxy18_df <- cenozoic_oxy18_df[cenozoic_oxy18_df$age <=max(LAD_per_df$age),]

# Sea level: short term Phanerozoic 
ages <- dfxl$`1.3`
sl_short <- dfxl$X__1
start_r <- 24220
end_r <- 24867
sl_ages <- as.numeric(ages[start_r:end_r])
sl_short <- as.numeric(oxy_18[start_r:end_r])

cenozoic_sl_df <- data.frame(age=sl_ages, short_sea_level=sl_short)
cenozoic_sl_df <- cenozoic_sl_df[cenozoic_sl_df$age <= max(LAD_per_df$age),]



plot(cenozoic_sl_df$age,
     cenozoic_sl_df$short_sea_level,
     t='l', lwd=2,
     #yaxt='n',
     xlab='age (Ma)',
     ylab='Sea level (m)',
     main = 'Sea level in meters above present day during Cenozoic era'
)
abline(h=0, lty=2)

data_dir <- '/Users/andy/Documents/projects/ML-Data Mining/evolution/data'
setwd(data_dir)
write.csv(FAD_LAD_per_df_ap, 
          file='cenozoic_extinction_speciation_after_present.csv',
          col.names = T,
          row.names = F)
write.csv(FAD_LAD_per_df, 
          file='cenozoic_extinction_speciation.csv',
          col.names = T,
          row.names = F)
write.csv(cenozoic_oxy18_df,
          file='cenozoic_oxy_18.csv',
          col.names = T,
          row.names = F)
write.csv(cenozoic_sl_df,
          file='cenozoic_sea_level.csv',
          col.names = T,
          row.names = F)


# find peaks
peak_id <- peaks(PF_FAD_LAD_per_df_ap$total)
peak_df <- PF_FAD_LAD_per_df_ap[peak_id,]
valley_id <- peaks(-PF_FAD_LAD_per_df_ap$total)
valley_df <- PF_FAD_LAD_per_df_ap[valley_id,]

pf_nn_df <- cbind(PF_FAD_LAD_per_df_ap[2:4], NN_FAD_LAD_per_df_ap[2:4])
colnames(pf_nn_df) <- c('PF_FAD', 'PF_LAD', 'PF_turnover', 'NN_FAD', 'NN_LAD', 'NN_turnover')
head(pf_nn_df)
corr <- round(cor(pf_nn_df), 3)
corr
datatable(corr)
plot(pf_nn_df, t='l')


a <- pf_nn_df$PF_turnover
b <- pf_nn_df$NN_turnover

z1 <- (a - mean(a))/sd(a)
z2 <- (b - mean(b))/sd(b)
plot(z1, z2)
cor(z1, z2)
cor(a, b)
cor(pf_nn_df)


library(WaveletComp)
my.w <- analyze.wavelet(pf_nn_df, "PF_turnover",
                loess.span = 0, 
                dt = 0.1, 
                #dj = 1/50, 
                #lowerPeriod = 2, upperPeriod = 256, 
                make.pval = TRUE, n.sim = 50)
par(mfrow=c(1,1))
wt.image(my.w, color.key = "interval", 
         n.levels = 250, legend.params = list(lab = "wavelet power levels"), 
         periodlab = "period (myr)", show.date = TRUE, 
         date.format = "%F", timelab = "")
