data_dir <- '/Users/andy/Documents/projects/ML-Data Mining/Evolution_Data_Mining/Cenozoic_Nannofossils/data'
setwd(data_dir)
dp_fname <- 'Bergen-etal_GSA2018_TableS1 theme-sorted.xlsx'
library(readxl)
sheets <- excel_sheets(dp_fname)
sheets
dfxl <- read_excel(dp_fname, sheet=sheets[1])#, range = "A13:F1987")
c <- colnames(dfxl) # column header
c

for_ordering_col <- dfxl$`For ordering`
horizon_col <- dfxl$HORIZON
bp_historical_name_col <- dfxl$`BP HISTORICAL NAME`
chart_name_col <- dfxl$`2018 CHART NAME`
published_name_col <- dfxl$`PUBLISHED NAME (IF DIFFERENT)`
nn_col <- dfxl$NN
cnm_col <- dfxl$CNM
cn_col <- dfxl$CN
comments_col <- dfxl$COMMENTS
flag_col <- dfxl$Flag
nannos_col <- dfxl$Nannos
n_zone_col <- dfxl$`N-zone`
wade_zone_col <- dfxl$`Wade-zone`
subzone_col <- dfxl$Subzone
planktonic_forams_col <- dfxl$`Planktonic Forams`
benthic_forams_col <- dfxl$`Benthic Forams`
group_col <- dfxl$Group
gom_event_col <- dfxl$`GoM Event type`
odp_event_col <- dfxl$`ODP Event type`
chart_age_col <- dfxl$`2018 CHART AGE (Ma)`
calibration_col <- dfxl$`CALIBRATED TO`

nannos_row_id <- which(!is.na(nannos_col))
nannos_df_full <- data.frame(nannos=as.character(nannos_col[nannos_row_id]),
                        age=as.numeric(as.character(chart_age_col[nannos_row_id])),
                        bp_historical_name=bp_historical_name_col[nannos_row_id],
                        chart_name=chart_name_col[nannos_row_id],
                        published_name=published_name_col[nannos_row_id],
                        flag=flag_col[nannos_row_id],
                        nn=nn_col[nannos_row_id],
                        cn=cn_col[nannos_row_id],
                        cnm=cnm_col[nannos_row_id],
                        gom_event_type=as.character(gom_event_col[nannos_row_id]),
                        odp_event_type=as.character(odp_event_col[nannos_row_id]),
                        comments=as.character(comments_col[nannos_row_id]))

nannos_df <- nannos_df_full[1:462,]

head(nannos_df)
library(DT)
datatable(nannos_df)

name_col <- nannos_df$nannos
name_col
age_col <- nannos_df$age
age_col
type_col <- nannos_df$gom_event_type
type_col
as.character(unique(type_col))
#branch_to_col <- dfxl$X__3
#branch_to_col
LAD_cond <- (type_col == 'HO' 
  | type_col == 'HRO' 
  #| type_col == 'HIO'
  #| type_col == 'HFO'
  #| type_col == 'HCO'
  #| type_col == 'HO Acme'
  #| type_col == 'HOAcme'
  )
LAD_ix <- which(LAD_cond)

LAD_ix

FAD_cond <- (type_col == 'LO' 
  | type_col == 'LRO' 
  #| type_col == 'LIO'
  #| type_col == 'LFO' 
  #| type_col == 'LCO'
  #| type_col == 'LO Acme'
  #| type_col == 'LOAcme'
  )

FAD_ix <- which(FAD_cond)

FAD_ix

lad_df <- data.frame(LAD=age_col[LAD_ix], name=name_col[LAD_ix], event_type=type_col[LAD_ix])
lad_df <- lad_df[order(lad_df$LAD, decreasing = F),]
lad_df
fad_df <- data.frame(FAD=age_col[FAD_ix], name=name_col[FAD_ix], event_type=type_col[FAD_ix])
fad_df <- fad_df[order(fad_df$FAD, decreasing = F),]
fad_df

LAD_type <- c()
LAD <- c()
name <- c()
FAD <- c()
FAD_type <- c()

missing_fad <- which(!(fad_df$name %in% lad_df$name))
for(r in missing_fad) {
  FAD <- c(FAD, fad_df$FAD[r])
  FAD_type <- c(FAD_type, as.character(fad_df$event_type[r]))
  name <- c(name, as.character(fad_df$name[r]))
  LAD_type <- c(LAD_type, "NA")
  LAD <- c(LAD, 0)
}

i <- 1
n <- dim(lad_df)[1]
for(i in 1:n) {
  name1 <- as.character(lad_df$name[i])
  j <- 1
  n2 <- dim(fad_df)[1]
  found <- FALSE
  for(j in 1:n2) {
    name2 <- as.character(fad_df$name[j])
    if (name2 == name1) {
      FAD <- c(FAD, fad_df$FAD[j])
      FAD_type <- c(FAD_type, as.character(fad_df$event_type[j]))
      name <- c(name, name1)
      LAD_type <- c(LAD_type, as.character(lad_df$event_type[i]))
      LAD <- c(LAD, lad_df$LAD[i])
      found <- TRUE
    }
  }
  if (found == FALSE) {
    FAD <- c(FAD, 50)
    FAD_type <- c(FAD_type, 'NA')
    name <- c(name, name1)
    LAD_type <- c(LAD_type, as.character(lad_df$event_type[i]))
    LAD <- c(LAD, lad_df$LAD[i])
    found <- TRUE  
  }
}

fad_lad_df <- data.frame(FAD=FAD,
                         FAD_type=FAD_type,
                         name=name,
                         LAD_type=LAD_type,
                         LAD=LAD)


datatable(lad_df)
datatable(fad_df)
datatable(fad_lad_df)

# load data from excel diractly
updated_table_dir <- '/Users/andy/Documents/projects/ML-Data Mining/Evolution_Data_Mining/Cenozoic_Nannofossils/data/'
fn1 <- 'Nannos_fad_lad PrelimBeforeRemovingDuplicates.xlsx'
fn2 <- 'Nannos_fad_lad_latest_Andy_24Sept2019.xlsx' #'Nannos_fad_lad JimBergenEnhancing 26Aug2019.xlsx'
updated_table_fn <- paste(updated_table_dir, fn2, sep="")
library(readxl)
sheets <- excel_sheets(updated_table_fn)
sheets
dfxl <- read_excel(updated_table_fn, sheet=sheets[1])#, range = "A13:F1987")
df <- dfxl[,c(2:6)]
#cond <- !(df$FAD_type == 'LCO' | df$LAD_type == 'LCO')
#df <- df[cond, ]

FAD <- as.numeric(df$FAD)
FAD <- replace(FAD, is.na(FAD), 50)
FAD_type <- df$FAD_type
name <- df$name
LAD <- as.numeric(df$LAD)
LAD <- replace(LAD, is.na(LAD), 0)
LAD_type <- df$LAD_type

# creating FAD LAD data frame for nannos
fad_lad_df <- data.frame(FAD=FAD,
                         FAD_type=FAD_type,
                         name=name,
                         LAD_type=LAD_type,
                         LAD=LAD)
head(fad_lad_df)
datatable(fad_lad_df)

dim(fad_lad_df)

fad_lad_df$FAD <- as.numeric(fad_lad_df$FAD)
fad_lad_df$name <- as.character(fad_lad_df$name)
fad_lad_df$LAD <- as.numeric(fad_lad_df$LAD)

# unique rows and columns in fad and lad df
u_fad_df <- aggregate(fad_lad_df[,c(1,3)], by=list(fad_lad_df$name), min)
u_fad_df <- u_fad_df[,c(2,3)]
u_fad_df <- u_fad_df[order(u_fad_df$name),]
u_fad_df

u_lad_df <- aggregate(fad_lad_df[,c(3,5)], by=list(fad_lad_df$name), max)
u_lad_df <- u_lad_df[, c(2,3)]
u_lad_df <- u_lad_df[order(u_lad_df$name),]
u_lad_df

final_fad_lad_df <- merge(u_fad_df, u_lad_df)
#final_fad_lad_df$name[1] <- "Dictyococcites filewiczii"
final_fad_lad_df <- final_fad_lad_df[order(final_fad_lad_df$name),]
final_fad_lad_df
datatable(final_fad_lad_df)

living_ix <- which(final_fad_lad_df$LAD == 0)
living_df <- final_fad_lad_df[living_ix,]
living_df
dim(living_df)

# determine minimum and maximum age 
min_age <- min(final_fad_lad_df$LAD, final_fad_lad_df$FAD)
mn_a <- 0# floor(min_age)
max_age <- max(final_fad_lad_df$LAD, final_fad_lad_df$FAD)
mx_a <- ceiling(max_age)
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
fad <- round(as.numeric(final_fad_lad_df$FAD), 5)
lad <- round(as.numeric(final_fad_lad_df$LAD), 5)
for(k in 1:n) {
  start_age <- round(age_seq[k],5)
  end_age <- round(start_age + window_size,5)
  mid_age <- (start_age + end_age)/2
  ixl <- which(lad >= start_age 
               & lad < end_age & lad != 0)
  
  ixf <- which(fad >= start_age 
               & fad < end_age)
  
  cnt_l = length(ixl)
  cnt_f = length(ixf)
  ln_l = as.character(final_fad_lad_df$name[ixl])
  ln_f = as.character(final_fad_lad_df$name[ixf])
  
  ages[[k]] <- mid_age
  LAD_cnts[[k]] <- cnt_l
  FAD_cnts[[k]] <- cnt_f
  LAD_names[[k]] <- ln_l
  FAD_names[[k]] <- ln_f
}

FAD_LAD_per_df <- data.frame(age=unlist(ages), 
                         FAD_cnt = unlist(FAD_cnts), LAD_cnt = unlist(LAD_cnts))
FAD_LAD_per_df <- FAD_LAD_per_df[FAD_LAD_per_df$age >= mn_a & FAD_LAD_per_df$age <= mx_a,]

FAD_LAD_per_df$total <- FAD_LAD_per_df$LAD_cnt + FAD_LAD_per_df$FAD_cnt
plot(FAD_LAD_per_df, t='l')
datatable(FAD_LAD_per_df)
head(FAD_LAD_per_df)
tail(FAD_LAD_per_df)

existing_n <- data.frame(age=rev(existing_n$age), n_existing=rev(existing_n$n_existing))
existing_n <- existing_n[existing_n$age>=mn_a & existing_n$age <= mx_a,]
existing_n <- existing_n[order(existing_n$age, decreasing = F),]
head(existing_n)
tail(existing_n)

# Extract information about genera and families for the forams
unique_nannos <- sort(unique(as.character(final_fad_lad_df$name)))
length(unique_nannos)
splitted_str <- strsplit(unique_nannos, " ")
genera <- c()
species <- c()
for(s in splitted_str) {
  genera <- c(genera, s[1])
  species <- c(species, s[2])
}
unique_genera <- unique(genera)
# number of unique generas
length(unique_genera)
species

# list of families for the generas 
families <- list()
families[["Amaurolithus"]] <- "Ceratolithaceae"
families[["Calcidiscus"]] <- "Calcidiscaceae"
families[["Camuralithus"]] <- "-"  # "-
families[["Catinaster"]] <- "Discoasteraceae"
families[["Ceratolithus"]] <- "Ceratolithaceae"
families[["Clausicoccus"]] <- "Coccolithaceae"
families[["Coccolithus"]] <- "Coccolithaceae" # https://species.wikimedia.org/wiki/Coccolithus
families[["Coronocyclus"]] <- "Coccolithaceae"
families[["Crenalithus"]] <- "Noelaerhabdaceae" #Gephyrocapsaceae #https://species.wikimedia.org/wiki/Crenalithus
families[["Cryptococcolithus"]] <- "Calcidiscaceae" #http://www.mikrotax.org/Nannotax3/index.php?taxon=Calcidiscaceae&module=Coccolithophores&dpage=1
families[["Cyclicargolithus"]] <- "Noelaerhabdaceae" 
families[["Dictyococcites"]] <- "Noelaerhabdaceae" 
families[["Discoaster"]] <- "Discoasteraceae" 
families[["Emiliania"]] <- "Noelaerhabdaceae"
families[["Gephyrocapsa"]] <- "Noelaerhabdaceae"
families[["Helicosphaera"]] <- "Helicosphaeraceae" # https://species.wikimedia.org/wiki/Helicosphaera
families[["Ilselithina"]] <- "-" #Coccolith families inc sed -> Coccolith genera inc sed -> Ilselithina http://www.mikrotax.org/system/index.php?taxon=Ilselithina&module=Coccolithophores
families[["Minylitha"]] <- "-" #Coccolithophores -> Nannolith families inc sed -> Nannolith genera inc sed -> Minylitha http://www.mikrotax.org/Nannotax3/index.php?taxon=Nannolith%20genera%20inc%20sed&module=Coccolithophores
families[["Pontosphaera"]] <- "Pontosphaeraceae" #https://species.wikimedia.org/wiki/Pontosphaera
families[["Pseudoemiliania"]] <-"Noelaerhabdaceae"
families[["Reticulofenestra"]] <- "Noelaerhabdaceae"
families[["Schyphosphaera"]] <- "Pontosphaeraceae"
families[["Sphenolithus"]] <- "Sphenolithaceae" 
families[["Triquetrorhabdulus"]] <- "Ceratolithaceae"

final_fad_lad_df$Lifespan <- final_fad_lad_df$FAD - final_fad_lad_df$LAD
families_v <- c()

for(g in final_fad_lad_df$name) {
  gs <- strsplit(g, " ")[[1]][1]
  fam <- families[[gs]]
  if (is.null(fam)) {
    print(g)
    print(gs)
    print(fam)
    fam <- "-"
  }
  families_v <- c(families_v, fam)
}
final_fad_lad_df$Family <- families_v

final_fad_lad_df <- final_fad_lad_df[order(final_fad_lad_df$Family),]
datatable(final_fad_lad_df)

# check the species for which FAD is not known exactly
include_rows <- which(final_fad_lad_df$FAD == 50)
include_rows
final_fad_lad_df[include_rows,]
final_fad_lad_df2 <- final_fad_lad_df[include_rows,]

living_df

fam_df <- data.frame(genus=genera)
fam_v <- c()
for (g in fam_df$genus) {
  fam <- families[[g]]
  if (is.null(fam))
    fam <- "-"
  fam_v <- c(fam_v, fam)
}
fam_df$family <- fam_v

fad_lad_df <- final_fad_lad_df2

# Create a list such that we can search family name by genus as key
fam_by_genus_list <- list()
c <- 1
for (g in fam_df$genus) {
  fam_by_genus_list[[g]] <- as.character(fam_df$family[c])
  c = c + 1 
}
fam_by_genus_list

fam_by_genus <- c()
for (f in fad_lad_df$name) {
  genus <- strsplit(as.character(f), " ")[[1]][1]
  fam <- fam_by_genus_list[[genus]]
  fam_by_genus <- c(fam_by_genus, fam)
}
# Add family name to the fad_lad data frame
fad_lad_df$Family <- fam_by_genus

# number of unique families
unique_families <- unique(as.character(fam_df$family))
as.character(unique_families)
length(unique_families)


fad_lad_df_by_lifespan<-fad_lad_df[order(fad_lad_df$Lifespan, decreasing = F),]
fad_lad_df_by_lifespan <- fad_lad_df_by_lifespan[order(fad_lad_df_by_lifespan$Family),]
library(DT)
datatable(fad_lad_df_by_lifespan)

n <- aggregate(fad_lad_df_by_lifespan[,c(2,5)], by=list(fad_lad_df$Family), length)
fam_species_cnt_df <- n[,c(1,3)]
fam_species_cnt_df <- fam_species_cnt_df[order(fam_species_cnt_df$Family),]
colnames(fam_species_cnt_df) <- c("Family", "# of Species")
fam_species_cnt_df
datatable(fam_species_cnt_df)

m <- aggregate(fad_lad_df_by_lifespan[,c(4,5)], by=list(fad_lad_df$Family), mean)
m <- m[, c(1,2)]
fam_by_mean_lifespan <- m[order(m$Lifespan),]
colnames(fam_by_mean_lifespan) <- c("Family", "Mean Lifespan")
fam_by_mean_lifespan$`Mean Lifespan` <- round(fam_by_mean_lifespan$`Mean Lifespan`,2)
datatable(fam_by_mean_lifespan)

m <- aggregate(fad_lad_df_by_lifespan[,c(4,5)], by=list(fad_lad_df$Family), min)
m <- m[, c(1,2)]
fam_by_start_lifespan <- m[order(m$Lifespan),]
colnames(fam_by_start_lifespan) <- c("Family", "Start of Lifespan")
fam_by_start_lifespan$`Start of Lifespan` <- round(fam_by_start_lifespan$`Start of Lifespan`,2)
datatable(fam_by_start_lifespan)

m <- aggregate(fad_lad_df_by_lifespan[,c(4,5)], by=list(fad_lad_df$Family), max)
m <- m[, c(1,2)]
fam_by_end_lifespan <- m[order(m$Lifespan),]
colnames(fam_by_end_lifespan) <- c("Family", "End of Lifespan")
fam_by_end_lifespan$`End of Lifespan` <- round(fam_by_end_lifespan$`End of Lifespan`,2)
datatable(fam_by_end_lifespan)

fam_by_species_df <- fam_species_cnt_df
fam_by_species_df$`Mean Lifespan` <- fam_by_mean_lifespan$`Mean Lifespan`
fam_by_species_df$`Start of Lifespan` <- fam_by_start_lifespan$`Start of Lifespan`
fam_by_species_df$`End of Lifespan` <- fam_by_end_lifespan$`End of Lifespan`
datatable(fam_by_species_df)

# calculate mean life span
# mean life span is defined by the average lifespan of species existing in each pseudolevel 
#fn <- sink('tmp.txt')
n <- length(age_seq) - 1
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
    ids <- which(final_fad_lad_df$name == s)
    fam <- fad_lad_df[ids,]$Family
    fams <- c(fams, fam)
    fad_s <- as.numeric(final_fad_lad_df[ids,]$FAD)
    fads <- c(fads, fad_s)
  }
  existing_sp[[k]]['families'] <- list(fams)
  existing_n <- rbind(existing_n, data.frame(age,n_a))
  
  #get the mean lifespan for the existing species in the current period
  if (age > 0 & length(fads) != 0) {
    cur_age <- (age - window_size/2)
    mspl <- sum(fads-cur_age)/length(fads)
    mean_lifespan_per_level <- rbind(mean_lifespan_per_level,
                                     data.frame(age, mspl))
  } else {
    mspl <- 0
    mean_lifespan_per_level <- rbind(mean_lifespan_per_level,
                                     data.frame(age, mspl))
  }
}


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
df[is.na(df)] <- 0

#datatable(df)

# Extract only Cenozoic
CENOZOIC_BASE <- 80
df_C <- df[df$age > 0 & df$age <= CENOZOIC_BASE, ]
tail(df_C)

# Change the dff here depending on the time period being focused on
dff <- df
dff <- df_C

dff <- dff[,-c(2,3,4,5,6)]
dff$`raw.turnover.probability` <- dff$`raw.speciation.probability` + dff$`raw.extinction.probability`
datatable(dff)
which(is.na(dff))
head(dff)
tail(dff)

nn_diversity_per_level <- dff[, c("age", "N.species.speciation")] 
head(nn_diversity_per_level)


nn<- dim(mean_lifespan_per_level)[1]-1
dff$`mean.lifespan` <- mean_lifespan_per_level$mspl[2:nn]
write.table(dff, "nannos_cenozoic.csv", sep=",")
#write.table(dff[,c("age","mean.lifespan")], "b.csv", sep=",")
NN_dff <- dff


par(mfrow=c(1,1))
par(mar=c(3,3,3,3))
plot(dff$age, dff$mean.lifespan, t='l',axes=F)
axis(1)
axis(2)
box()
par(new=T)
plot(dff$age, dff$N.species.speciation, t='l', col='green', axes=F)

#histogram
par(mfrow=c(2,1))
hist(final_fad_lad_df$FAD, breaks = 70*10)
boxplot(final_fad_lad_df$FAD)

hist(fad_lad_df$LAD, breaks = 70 * 10)
boxplot(fad_lad_df$LAD)

par(mfrow=c(2,1))
hist(final_fad_lad_df$Lifespan, breaks=50)
boxplot(final_fad_lad_df$Lifespan)

quantile(fad_lad_df$Lifespan)


# max life span
max_id <- which.max(fad_lad_df$Lifespan)
max(fad_lad_df$Lifespan)
fad_lad_df[max_id,]

# min life span
min_id <- which.min(fad_lad_df$Lifespan)
min(fad_lad_df$Lifespan)
fad_lad_df[min_id,]

# mean life span
mean(fad_lad_df$Lifespan)
#median
median(fad_lad_df$Lifespan)

head(FAD_LAD_per_df)
N <- dim(FAD_LAD_per_df)[1]
#FAD_LAD_per_df_ap <- FAD_LAD_per_df[2:N,]
#head(FAD_LAD_per_df_ap)

#ages_ap <- FAD_LAD_per_df$age[2:N]

#NN_FAD_LAD_per_df_ap <- FAD_LAD_per_df_ap

NN_FAD_LAD_per_df_ap <- FAD_LAD_per_df
head(NN_FAD_LAD_per_df_ap)

par(mfrow=c(2,1))
par(mar=c(1,4,1,4))
width <- 10 # to obtain every million year
by <- 3
x <- rollapply(-NN_FAD_LAD_per_df_ap$age, width=width, by=by, FUN='mean')
y <- rollapply(NN_FAD_LAD_per_df_ap$LAD_cnt + NN_FAD_LAD_per_df_ap$FAD_cnt,
               width=width, by=by, FUN='mean')
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

y1 <- rollapply(NN_FAD_LAD_per_df_ap$LAD_cnt,
                width=width, by=by, FUN='mean')
lines(x, y1, #ages, extinction_rate, 
      lwd=2,
      col=2)
points(x, y1, cex=0.5, col='red')

y2 <- rollapply(NN_FAD_LAD_per_df_ap$FAD_cnt,
                width=width, by=by, FUN='mean')
lines(x, y2, #ages, speciation_rate, 
      lwd=2,
      col=3)
points(x, y2, cex=0.5, col='green')

legend(x=-30,y=3,
       legend=c('turnover(speciation + extinction)', 'extinction', 'speciation'),
       col=c(1,2,3), lty=c(1,1,1),
       cex=0.8)

#par(mfrow=c(1,1))
par(mar=c(3,4,1,4))
x <- rollapply(-mean_lifespan_per_level$age,
               width=width, by=by, FUN='mean')
y <- rollapply(mean_lifespan_per_level$mspl,
               width=width, by=by, FUN='mean')
x <- -NN_dff$age
y <- NN_dff$mean.lifespan
length(x)
length(y)
plot(x, y, 
     t='l', xlim=c(-35,0), 
     axes=F, lwd=2, col='black', 
     xlab="Age (Myr)", ylab="Mean lifespan (Myr)")
#points(x, y, cex=0.5)
axis(1)
axis(2)
box()
xd <- rollapply(-pf_diversity_per_level$age,
                width=width, by=by, FUN='mean')
yd <- rollapply(pf_diversity_per_level$N.species.speciation,
                width=width, by=by, FUN='mean')
xd <- -NN_dff$age
yd <- NN_dff$N.species.speciation
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


sddf <- merge(mean_lifespan_per_level, nn_diversity_per_level, by="age")
datatable(sddf)

# draw nannos ranges
# plot by first occurrence
df <- fad_lad_df[order(-fad_lad_df$FAD),]
n <- dim(fad_lad_df)[1]
rdf <- data.frame(x0=seq(1, n),
                  y0=-df$FAD,
                  x1=seq(1, n),
                  y1=-df$LAD,
                  Family=fad_lad_df$Family
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
segments(rdf$x0, rdf$y0, rdf$x1, rdf$y1, col=rdf$Family)

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


# write in to the datapack
save_datapack_dir <- '/Users/andy/Documents/projects/ML-Data Mining/Evolution_Data_Mining/Cenozoic_Planktonic_Foraminifera/datapack/'
organism_type <- 'Calcerous_nannofossils'
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
Mspecdf <- data.frame(Mspec)
Mspecdf <- Mspecdf[order(Mspecdf$Harmonic_CL, decreasing = T),]
time_window <- max(x) - min(x)
N <- length(evolution_model$evolution)
Mspecdf$period <- (1/Mspecdf$Frequency) # * (time_window / N))
Mspecdf <- Mspecdf[order(Mspecdf$period, decreasing = T),]
Mspecdf2 <- subset(Mspecdf, (Mspecdf$Harmonic_CL >=80 & Mspecdf$AR1_CL >= 80) | 
                     (Mspecdf$Harmonic_CL >=95) )
datatable(Mspecdf2[,c(1:4, 9)])


# plot pf and nn together
plot(NN_FAD_LAD_per_df_ap$age, 
     #PF_FAD_LAD_per_df_ap$FAD_cnt , #+ 
     NN_FAD_LAD_per_df_ap$LAD_cnt, 
     t='l', lwd=2,
     col=2, 
     #lty=2,
     xlab='Age (Ma)',
     ylab='Number of events', #'event rate(%)',
     #ylim=c(0,max(FAD_LAD_per_df_ap$total) + 1),
     main=paste('Speciation and extinction events of PF and NN\n',
                '(window size = ', window_size, 'Ma,\n',
                'slide =', slide_window , 'Ma)' ) )

lines(PF_FAD_LAD_per_df_ap$age, 
     #NN_FAD_LAD_per_df_ap$FAD_cnt , #+ 
     PF_FAD_LAD_per_df_ap$LAD_cnt, 
     t='l', lwd=2, col=3)
legend('topright', legend=c('Planktonic Foraminifera', 'Calcerous Nannofossils'), 
       col=c(2,3), lty=c(1,1),
       cex=0.75)

# 
par(mfrow=c(1,1))
plot(PF_FAD_LAD_per_df_ap$age, 
     PF_FAD_LAD_per_df_ap$FAD_cnt , #+ 
     #PF_FAD_LAD_per_df_ap$LAD_cnt, 
     t='l', lwd=2,
     col=3, 
     #lty=2,
     xlab='Age (Ma)',
     ylab='Number of events', #'event rate(%)',
     ylim=c(-25,25),
     main=paste('Speciation and extinction events of PF \n',
                '(window size = ', window_size, 'Ma,\n',
                'slide =', slide_window , 'Ma)' ) )
abline(h=0, lty=2, col=1)
lines(PF_FAD_LAD_per_df_ap$age, 
      #NN_FAD_LAD_per_df_ap$FAD_cnt , #+ 
      -PF_FAD_LAD_per_df_ap$LAD_cnt, 
      t='l', lwd=2, col=2)
lines(PF_FAD_LAD_per_df_ap$age, 
      #NN_FAD_LAD_per_df_ap$FAD_cnt , #+ 
      PF_FAD_LAD_per_df_ap$total, 
      t='l', lwd=2, lty=2, col=1)
# temperature
oxy_idx<-which(oxy_ages<=25)
m <- 10
x <- rollmean(oxy_ages[oxy_idx], k=m)
y <- rollmean(oxy_18[oxy_idx], k=m)
par(new=T)
plot(x, -y, t='l', col='blue', xlim=c(0,25), axes=F, xlab='', ylab='')
yv <- pretty(-y)
ylbls <- -yv
axis(4, at=yv, labels = ylbls)
#mtext("Temp (°C)", side=4, line=3)
mtext("Oxy-18", side=4, line=3)

par(mar=c(4,4,4,8))
plot(NN_FAD_LAD_per_df_ap$age, 
     NN_FAD_LAD_per_df_ap$FAD_cnt , #+ 
     #PF_FAD_LAD_per_df_ap$LAD_cnt, 
     t='l', lwd=2,
     col=3, 
     #lty=2,
     xlab='Age (Ma)',
     ylab='Number of events', #'event rate(%)',
     ylim=c(-25,25),
     xlim=c(0,25),
     main=paste('Speciation and extinction events of NN \n',
                '(window size = ', window_size, 'Ma,\n',
                'slide =', slide_window , 'Ma)' ) )
abline(h=0, lty=2, col=1)
lines(NN_FAD_LAD_per_df_ap$age, 
      #NN_FAD_LAD_per_df_ap$FAD_cnt , #+ 
      -NN_FAD_LAD_per_df_ap$LAD_cnt, 
      t='l', lwd=2, col=2)
lines(NN_FAD_LAD_per_df_ap$age, 
      #NN_FAD_LAD_per_df_ap$FAD_cnt , #+ 
      NN_FAD_LAD_per_df_ap$total, 
      t='l', lwd=2, lty=2, col=1)


# temperature
oxy_idx<-which(oxy_ages<=25)
m <- 20
x <- rollmean(oxy_ages[oxy_idx], k=m)
y <- rollmean(oxy_18[oxy_idx], k=m)
par(new=T)
plot(x, -y, t='l', col='blue', xlim=c(0,25), axes=F, xlab='', ylab='')
yv <- pretty(-y)
ylbls <- -yv
axis(4, at=yv, labels = ylbls)
#mtext("Temp (°C)", side=4, line=3)
mtext("Oxy-18", side=4, line=3)
#arrows(10, -1, 10, 0.5, col=2, lwd=2)
#mtext("High Temp",  side = 3, at = c(16), line= -3.5, col=2)
#arrows(10, -3, 10, -4.5, col='blue', lwd=2)
#mtext("Low Temp",  side = 3, at = c(16), line= -10, col='blue', lwd=2)

