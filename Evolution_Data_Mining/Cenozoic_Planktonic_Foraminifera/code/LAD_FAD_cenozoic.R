data_dir <- '/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/EvolutionaryTree/Fordham and Zehady shared/180724'
setwd(data_dir)
dp_fname <- 'qryTSCAze_MorphospeciesAzeTableS3_ColourMorphogroup.xls'
library(readxl)
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
branch_to_col
LAD_ix <- which(type_col == 'TOP')
LAD_ix
FAD_ix <- which(type_col == 'branch')
FAD_ix

lad_df <- data.frame(LAD=age_col[LAD_ix], name=name_col[LAD_ix])
lad_df <- lad_df[order(lad_df$LAD, decreasing = F),]
lad_df
fad_df <- data.frame(FAD=age_col[FAD_ix], name=branch_to_col[FAD_ix])
fad_df <- fad_df[order(fad_df$FAD, decreasing = F),]
fad_df

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

living_ix <- which(fad_lad_df$LAD == 0)
living_df <- fad_lad_df[living_ix,]
living_df
dim(living_df)


min_age <- min(fad_lad_df$LAD, fad_lad_df$FAD)
mn_a <- floor(min_age)
max_age <- max(fad_lad_df$LAD, fad_lad_df$FAD)
mx_a <- ceiling(max_age)

slide_window <- 1
age_seq <- seq(mn_a, mx_a, by=slide_window)
ages <- list()
LAD_cnts <- list()
FAD_cnts <- list()
LAD_names <- list()
FAD_names <- list()
n <- length(age_seq) - 1
for(k in 1:n) {
  start_age <- age_seq[k]
  end_age <- age_seq[k+1]
  mid_age <- (start_age + end_age)/2
  ixl <- which(fad_lad_df$LAD >= start_age 
              & fad_lad_df$LAD < end_age)

  ixf <- which(fad_lad_df$FAD >= start_age 
               & fad_lad_df$FAD < end_age)
  
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

FAD_LAD_per_df$total <- FAD_LAD_per_df$LAD_cnt + FAD_LAD_per_df$FAD_cnt

head(FAD_LAD_per_df)

N <- dim(FAD_LAD_per_df)[1]
FAD_LAD_per_df_ap <- FAD_LAD_per_df[2:N,]
head(FAD_LAD_per_df_ap)

ages_ap <- FAD_LAD_per_df$age[2:N]
extinction_rate <- FAD_LAD_per_df_ap$LAD/sum(FAD_LAD_per_df_ap$LAD) * 100
speciation_rate <- FAD_LAD_per_df_ap$FAD/sum(FAD_LAD_per_df_ap$FAD) * 100

plot(FAD_LAD_per_df_ap$age, 
     FAD_LAD_per_df_ap$LAD_cnt + FAD_LAD_per_df_ap$FAD_cnt, 
     t='l', lwd=1,
     col=1, 
     lty=2,
     xlab='Age (Ma)',
     ylab='Number of events', #'event rate(%)',
     ylim=c(0,42),
     main='Speciation and extinction events of planktonic foraminifera during Cenozoic era')

lines(FAD_LAD_per_df_ap$age, FAD_LAD_per_df_ap$LAD_cnt, #ages, extinction_rate, 
      lwd=2,
      col=2)

lines(FAD_LAD_per_df_ap$age, FAD_LAD_per_df_ap$FAD_cnt, #ages, speciation_rate, 
      lwd=2,
      col=3)
legend('topright', legend=c('speciation + extinction', 'extinction', 'speciation'), col=c(1,2,3), lty=c(2,1,1))


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
