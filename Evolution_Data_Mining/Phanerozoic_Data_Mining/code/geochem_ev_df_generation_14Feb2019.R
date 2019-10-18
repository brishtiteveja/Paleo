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
a
a_r2 = rollapply(zoo(a), width=c(5,3,2), FUN=sum, na.rm=TRUE, 
                         by=c(5,3,2), 
                 align='left')
a
a_r2

length(a)
a_r2 = rollapply(zoo(a), width=c(4), FUN=sum, #na.rm=TRUE, 
                 by=c(1), 
                 align='left')
a
a_r2
#----------------------------------------------------------------------------------

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
head(carbon_13)
carbon_13_info <- dfxl$X__2
start_r <- 32996
end_r <- 38049
carbon_ages <- as.numeric(ages[start_r:end_r])
carbon_13 <- as.numeric(carbon_13[start_r:end_r])
head(carbon_ages)

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
     t='l', #yaxt='n',
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
     t='l', #yaxt='n',
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
     t='l', #yaxt='n',
     xlab='age (Ma)',
     ylab='Sr 87/86 ratio',
     main = 'Phanerozoic Sr 87/86'
)
par(new=T)
plot(sr87_86_avg_dff$age, sr87_86_avg_dff$sr87_86, t='l', col=2, axes=F, xlab="", ylab="")

# Sea Level Data extraction
dir <- ("~/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/Datapacks/")
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

plot(msl_df, t='l', xlab='age (Ma)', ylab='Sea Level (m)', main='Phanerozoic Sea Level')
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


# Prokoph data
# Cenozoic-Campanian Marine Oxygen-18 Composite (per-mil PDB)
data_dir <- '/Users/andy/Documents/projects/ML-Data Mining/Evolution_Data_Mining/Phanerozoic_Data_Mining/Datapack'
setwd(data_dir)
dp_fname <- 'prokoph_data_table_2.xlsx'
library(readxl)
sheets <- excel_sheets(dp_fname)
sheets
dfxl <- read_excel(dp_fname) #, sheet=sheets[1], range = "A13:F1987")
c <- colnames(dfxl) # column header
c
prokoph_df <- data.frame(dfxl)
head(prokoph_df)
tail(prokoph_df)

plot(prokoph_df, t='l')
plot(prokoph_df$Age.Ma., prokoph_df$Marine.genera, 
     t='l', lwd=2,
     xlab='age (Ma)', ylab='Number of genera',
     main='Marine genera data from Prokoph et al')
plot(prokoph_df$Age.Ma., prokoph_df$LIP.volumes.vs.1.103.km3.,
     t='l', lwd=2, ylab="", #yaxt='n',
     xlab='age (Ma)', 
     main='LIP volume'
     )
lines(prokoph_df$Age.Ma., prokoph_df$LIP.volumes.vs.1.103.km3.__1,
     t='l', lwd=2, ylab="", lty=2, col='green'
     )
legend('topleft', legend=c('LIP volume vs#1', 'LIP volume vs#2'),
       lty=c(1,2), col=c('black', 'green'))


plot(prokoph_df$Age.Ma., prokoph_df$δ34S, 
     t='l', lwd=2,
     xlab='age (Ma)', ylab='Sulphur isotope (S-34)',
     main='Sulphur isotope curve')
# Passive margin data extraction
data_dir <- '/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/Datapacks'
setwd(data_dir)
dp_fname <- 'Phan_GTS2016_for_7.1_HaqJur_ForamMikrotax_28July2017.xls'
library(readxl)
sheets <- excel_sheets(dp_fname)
sheets
dfxl <- read_excel(dp_fname) #, sheet=sheets[1], range = "A13:F1987")
c <- colnames(dfxl) # column header
df2c <- dfxl[[c[2]]]

column_type <- c('event')

columns <- list()
for (ct in column_type) {
  msg <- paste("processing ", ct, " column.\n",
               "----------------------------------------------------------------------",
               sep="")
  print(msg)
  columns[[ct]] <- list() 
  
  # get excel row num for each column
  rows <- which(df2c == ct) + 1
  
  
  cols <- list()
  if (ct == 'event') {
    cnum <- 1
    for (r in rows) {
      cols[[cnum]] <- list(name = c(), content=list())
      colname <- dfxl[[c[1]]][r-1]
      cols[[cnum]]$name <- colname
      
      r_i <- r
      column <- list()
      type <- NA
      while(TRUE) {
        age <- dfxl[[c[3]]][r_i]
        if (is.na(age)) {
          if (is.na(dfxl[[c[1]]][r_i]))
            break
          else {
            type <- dfxl[[c[1]]][r_i]
            r_i <- r_i + 1
            next
          }
        }
        name <- dfxl[[c[2]]][r_i]
        if (is.na(name) && !is.na(age))
          name <- ""
        line_type <- dfxl[[c[4]]][r_i]
        popup <- dfxl[[c[5]]][r_i]
        
        column$name <- c(column$name, name)
        column$age <- c(column$age, as.numeric(age))
        if (!is.na(type))
          column$type <- c(column$type, type)
        column$line_type <- c(column$line_type, line_type)
        column$popup <- c(column$popup, popup)
        r_i <- r_i + 1
      }
      cols[[cnum]]$content <- column
      cnum <- cnum + 1
    }
  } 
  
  columns[[ct]] <- cols
}

get_parent_hierarchy <- function(col_name) {
  parent_columns <- list()
  if (col_name %in% main_columns) {
    parent_columns[["main column"]] = main_column
    return(parent_columns)
  } else {
    for(main_column in main_columns) {
      # number of sub-columns
      sub_column_num <- length(main_column_list[[main_column]])
      if (sub_column_num == 0)
        next
      sub_columns <- names(main_column_list[[main_column]][1:sub_column_num])
      if (col_name %in% sub_columns) {
        parent_columns[["main column"]] = main_column
        isx <- which(col_name == sub_columns)
        parent_columns[["sub column"]] = sub_columns[[isx]]
        return(parent_columns)
      }
      else {
        for(sub_column_n in sub_columns) {
          sub_column <- main_column_list[[main_column]][[sub_column_n]]
          sub_sub_columns <- names(sub_column)
          sub_sub_column_num <- length(sub_sub_columns)
          if (sub_sub_column_num == 0)
            next
          if (col_name %in% sub_sub_columns) {
            parent_columns[["main column"]] = main_column
            isx <- which(sub_column_n == sub_columns)
            parent_columns[["sub column"]] = sub_columns[[isx]]
            issx <- which(col_name == sub_sub_columns)
            parent_columns[["sub sub column"]] = sub_sub_columns[[issx]]
            return(parent_columns)
          }
          for(sub_sub_column_n in sub_sub_columns) {
            sub_sub_sub_columns <- main_column_list[[main_column]][[sub_column_n]][[sub_sub_column_n]]
            sub_sub_sub_column_num <- length(sub_sub_sub_columns)
            if (sub_sub_sub_column_num == 0)
              next
            if (col_name %in% sub_sub_sub_columns) {
              parent_columns[["main column"]] = main_column
              isx <- which(sub_column_n == sub_columns)
              parent_columns[["sub column"]] = sub_columns[[isx]]
              issx <- which(sub_sub_column_n == sub_sub_columns)
              parent_columns[["sub sub column"]] = sub_sub_columns[[issx]]
              isssx <- which(col_name == sub_sub_sub_columns)
              parent_columns[["sub sub sub column"]] = sub_sub_sub_columns[[isssx]]
              return(parent_columns)
            }
          }
        }
      }
    }
  }
  
  return(NA)
}

get_main_column_name <- function(col_name) {
  parent = get_parent_hierarchy(col_name)
  
  if(is.na(parent) || is.na(parent[["main column"]])) {
    return(NA)
  }
  
  mc = parent[["main column"]]
  
  return(mc)
}

get_sub_column_name <- function(col_name) {
  parent = get_parent_hierarchy(col_name)
  
  if(is.na(parent) || is.na(parent[["sub column"]])) {
    return(NA)
  }
  
  sc = parent[["sub column"]]
  
  return(sc)
}

get_sub_sub_column_name <- function(col_name) {
  parent = get_parent_hierarchy(col_name)
  if(is.na(parent) || is.na(parent[["sub sub column"]])) {
    return(NA)
  }
  
  ssc = parent[["sub sub column"]]
  
  return(ssc)
}

not_column <- function(col_name) {
  if (col_name == "_metacolumn_off" || col_name == "_metacolumn_on" || 
      col_name == "_METACOLUMN_OFF" || col_name == "_METACOLUMN_ON" 
      || col_name == "pass" ||
      substring(col_name, 2, 18) == "for details click")
    return(TRUE)
  else if (col_name == 'off' || col_name == 'on') {
    return(TRUE)
  } else {
    # # column width
    m <- regexec("^[0-9]+$",col_name)
    ml <- regmatches(col_name, m)
    if (length(ml[[1]]) == 1) {
      return(TRUE)
    }
    
    # color code
    m <- regexec("([0-9])+/([0-9])+/([0-9])+",col_name)
    ml <- regmatches(col_name, m)
    
    if (length(ml[[1]]) != 0) {
      return(TRUE)  
    }
    
    return(FALSE)
  }
}

# main columns
# 

column_names_by_category <- list()
extract_column_names_by_category <- function() {
  main_columns <- c()
  sub_columns <- c()
  
  rix <- which(df2c == ":")
  for (r in rix) {
    cn1 <- as.character(dfxl[r, 1])
    if (cn1 %in% sub_columns) {
      
    } else {
      main_columns <- c(main_columns, cn1)
      nc <- length(dfxl[r,])
      sub_col_ns <- as.character(dfxl[r, 3 : nc])
      naix <- which(is.na(sub_col_ns))
      naix <- c(naix, which(sub_col_ns == "na"))
      
      for(n in 1:nc) {
        if (n %in% naix) {
          
        } else {
          sub_col_name <- sub_col_ns[n]
          if(!is.na(sub_col_name) && 
             (sub_col_name != "_metacolumn_off" || sub_col_name != "_metacolumn_on" || 
              sub_col_name != "pass")) {
            sub_columns <- c(sub_columns, sub_col_name)
          }
        }
      }
    }
  }
  column_names_by_category[["main column"]] <- main_columns <- unique(na.omit(main_columns))
  column_names_by_category[["sub column"]] <- sub_columns <- unique(na.omit(sub_columns))
  
  return(column_names_by_category)
}

# extract column names
# main_column ~ group in marine genera
# sub_column ~ sub group
# sub_sub_column ~ column
column_names <- extract_column_names_by_category()
main_column_names <- main_columns <- column_names[["main column"]]
sub_column_names <- column_names[["sub column"]]
n_sub_sub_column <- length(columns$event)
sub_sub_column_names <- c()
for (nsc in 1:n_sub_sub_column) {
  sub_sub_column_names <- c(sub_sub_column_names, columns[["event"]][[nsc]][["name"]])
}

modern_passive_margin_col_names <- c('General (major modern basins)',
                                     'Arctic basins',
                                     'Atlantic basins',
                                     'Pacific basins',
                                     'Indian basins',
                                     'Southern basins'
                                     )
regional_margin_col_names <- c('North American margin history',
                              'European margin history',
                              'Middle East and South Asia margin history',
                              'Russian-Chinese margin history',
                              'South American margin history',
                              'African margin history',
                              'Australian-Indonesian margin history')

passive_margin_col_names <- c(modern_passive_margin_col_names[-1], regional_margin_col_names)

ages_from_col3 <- na.omit(as.numeric(as.character(na.omit(dfxl[[c[3]]]))))
age_slide <- 0.5 # every 500K years 
#starting_age <- floor(min(ages_from_col3)) - age_slide/2
starting_age <- 0 - age_slide/2
cenozoic_base <- 66.04
ending_age <- 600 #ceiling(max(ages_from_col3)) + age_slide/2
#ending_age <- ceiling(cenozoic_base)
ending_age

events_by_col <- list()
event_names_by_col <- list()
ev_df_by_col <- list()
events_by_col_by_main_columns <- list()
event_names_by_col_by_main_columns <- list()
ev_df_by_main_columns <- list()

column_type <- c('event')
for(col_type in column_type) {
  msg <- paste("processing ", col_type, " column events.\n",
               "----------------------------------------------------------------------",
               sep="")
  print(msg)
  ncol <- length(columns[[col_type]])
  
  msg <-paste("total number of " , col_type , " columns = ", ncol, sep ="") 
  print(msg)
  
  if (ncol == 0) {
    msg <- paste("no column data or not extracted yet.")
    next
  }
  
  start_age <- starting_age
  end_age <- ending_age
  age_diff <- age_slide
  
  events <- list()
  event_names <- list()
  event_types <- list()
  events_by_main_columns <- list()
  event_names_by_main_columns <- list()
  
  while(start_age <= end_age) {
    next_age <- start_age + age_diff
    mid_age <- (start_age + next_age) / 2
    print("")
    print(paste("Processing age = ", mid_age, sep=""))
    print("------------------------------------")
    
    key <- as.character(mid_age)
    evs <- 0
    ev_names <- c()
    ev_types <- c()
    
    # column wise events
    evs_by_main_columns <- list()
    ev_names_by_main_columns <- list()
    for(main_column in main_columns) {
      evs_by_main_columns[[main_column]] <- 0
      ev_names_by_main_columns[[main_column]] <- c()
    }
    
    for(col in columns[[col_type]]){
      # Here only process for the margin columns
      if (col$name %in% passive_margin_col_names) {
        print(col$name)
      }
      else{ 
        
        next 
      }
      
      cn <- length(col$content$age)
      for (k in 1:cn) {
        if (length(col$content) == 0)
          next
        a <- col$content$age[k]
        n <- col$content$name[k]
        t <- col$content$type[k]
        if (a >= start_age && a < next_age) {
          evs <- as.numeric(evs) + 1
          ev_names <- c(ev_names, n)
          ev_types <- c(ev_types, t)
          
          # adding to the main_column 
          main_column <- get_main_column_name(col$name)
          if(is.na(main_column)) {
            #print(paste("something is wrong with column ", col, " for main_column ", main_column, sep=""))
          } else {
            evs_by_main_columns[[main_column]] <- as.numeric(evs_by_main_columns[[main_column]]) + 1
            ev_names_by_main_columns[[main_column]] <- c(ev_names_by_main_columns[[main_column]], n)
          }
        }
        k <- k+1
      }
    }
    events[[key]] <- evs
    event_names[[key]] <- ev_names
    if(col_type == 'event')
      event_types[[key]] <- ev_types
    
    events_by_main_columns[[key]] <- list()
    event_names_by_main_columns[[key]] <- list()
    for (main_column in main_columns) {
      events_by_main_columns[[key]][[main_column]] <- evs_by_main_columns[[main_column]]
      event_names_by_main_columns[[key]][[main_column]] <- ev_names_by_main_columns[[main_column]]
    }
    start_age <- next_age
  }
  
  age <- as.numeric(names(events))
  freq <- as.numeric(unlist(events))
  freq_lad <- c()
  freq_fad <- c()
  for(key in names(events)) {
    et <- event_types[[key]]
    if(is.null(et)) {
      fad = 0
      lad = 0
    } else {
      fad = length(et[et=="FAD"])
      lad = length(et[et=="LAD"])
    }
    freq_fad <- c(freq_fad, fad)
    freq_lad <- c(freq_lad, lad)
  }
  
  ev_df <- data.frame(age = age, freq = freq, freq_fad = freq_fad, freq_lad = freq_lad)
  
  data_mining_dir <- '/Users/andy/Documents/projects/ML-Data Mining/Evolution_Data_Mining/Phanerozoic_Data_Mining'
  data_dir <- paste(data_mining_dir, '/Datapack/passive_margin_event_extraction/', sep="")
  event_names_fn <- paste(col_type, '_column_', age_slide, '_mil_event_names.txt', sep="")
  fn0 <- paste(data_dir, event_names_fn, sep="")
  msg <- paste("writing event names in ", fn0, sep="")
  print(msg)
  
  # printing event names to file
  sink(fn0)
  print(event_names)
  sink()
  
  event_freq_datapack_file_name <- paste(col_type, '_column_every_', age_slide,'_mil_event_frequencies_datapack.txt', sep="")
  fn <- paste(data_dir, event_freq_datapack_file_name, sep="")
  msg <- paste("writing datapack ", fn, sep="")
  print(msg)
  
  sink(fn)
  str <- 'format version:	1.3\nage units:	ma\n'
  writeLines(str)
  str <- paste(col_type, ' column ', age_slide, ' million year event frequencies\tpoint\t150\t255/245/230', sep="")
  writeLines(str)
  nf <- length(freq)
  for (l in 1:nf) {
    a <- age[l]
    f <- freq[l]
    ln <- paste("\t", a, "\t", f, sep='')
    writeLines(ln)
  }
  
  # write fad frequencies
  str="\n"
  writeLines(str)
  str <- paste(col_type, ' column ', age_slide, ' million year fad frequencies\tpoint\t150\t255/245/230', sep="")
  writeLines(str)
  nf <- length(freq_fad)
  for (l in 1:nf) {
    a <- age[l]
    f <- freq_fad[l]
    ln <- paste("\t", a, "\t", f, sep='')
    writeLines(ln)
  }
  
  # write lad frequencies
  str="\n"
  writeLines(str)
  str <- paste(col_type, ' column ', age_slide, ' million year lad frequencies\tpoint\t150\t255/245/230', sep="")
  writeLines(str)
  nf <- length(freq_lad)
  for (l in 1:nf) {
    a <- age[l]
    f <- freq_lad[l]
    ln <- paste("\t", a, "\t", f, sep='')
    writeLines(ln)
  }
  sink()
  #file.show(fn)
  
  event_names_by_col[[col_type]] <- event_names
  events_by_col[[col_type]] <- events
  ev_df_by_col[[col_type]] <- ev_df
  
  events_by_col_by_main_columns[[col_type]] <- list()
  event_names_by_col_by_main_columns[[col_type]] <- list()
  ev_df_by_main_columns[[col_type]] <- list()
  for (main_column in main_columns) {
    age_r <- c()
    freq_r <- c()
    events_r <- c()
    event_names_r <- list()
    for(age in names(events_by_main_columns)) {
      events_r <- c(events_r, events_by_main_columns[[age]][[main_column]])
      event_names_r[[age]] <- event_names_by_main_columns[[age]][[main_column]]
      age_r <- c(age_r, as.numeric(age))
      freq_r <- c(freq_r, as.numeric(events_by_main_columns[[age]][[main_column]]))
    }
    events_by_col_by_main_columns[[col_type]][[main_column]] <- events_r
    event_names_by_col_by_main_columns[[col_type]][[main_column]] <- event_names_r
    ev_df_by_main_columns[[col_type]][[main_column]] <- data.frame(age = age_r, freq = freq_r)
  }
}

df = data.frame(ev_df_by_col$event)
library(DT)
datatable(df)

plot(df$age, df$freq, 
     t='l', yaxt='n',
     xlab='age (Ma)',
     ylab='Number of opening and closing event',
     main = 'Passive margin opening and closing event'
)
points(df$age, df$freq, cex=0.5)
lines(df$age, df$freq_fad, t='l') # margin start/opening
lines(df$age, df$freq_lad, t='l') # marging end/ending
passive_margin_df <- data.frame(age=df$age,
                                margin_oc_cnt=df$freq,
                                margin_opening_cnt=df$freq_fad,
                                margin_closing_cnt=df$freq_lad
                                )
head(passive_margin_df)
tail(passive_margin_df)

min_age <- min(passive_margin_df$age, na.rm = T)
mn_a <- floor(min_age) - 0.5
max_age <- max(passive_margin_df$age)
mx_a <- ceiling(max_age) + 0.5

slide_window <- 1
age_seq <- seq(mn_a, mx_a, by=slide_window)
ages <- list()
passive_margin_oc_avg <- list()
passive_margin_opening_avg <- list()
passive_margin_closing_avg <- list()

i <- 1
for(a in age_seq) {
  age <- (age_seq[i] + age_seq[i+1])/2
  ix <- which(passive_margin_df$age >= age_seq[i] & passive_margin_df$age < age_seq[i+1])
  nix <- length(ix)
  
  ages[i] <- age
  passive_margin_oc_avg[i] <- sum(passive_margin_df$margin_oc_cnt[ix])/nix
  passive_margin_opening_avg[i] <- sum(passive_margin_df$margin_opening_cnt[ix])/nix
  passive_margin_closing_avg[i] <- sum(passive_margin_df$margin_closing_cnt[ix])/nix
  
  i <- i+1
}

passive_margin_avg_df <- data.frame(age=unlist(ages),
                             margin_oc_avg_cnt=unlist(passive_margin_oc_avg),
                             margin_opening_avg_cnt=unlist(passive_margin_opening_avg),
                             margin_closing_avg_cnt=unlist(passive_margin_closing_avg))
passive_margin_avg_dff <- data.frame(na.approx(passive_margin_avg_df))

head(passive_margin_avg_dff)
tail(passive_margin_avg_dff)

plot(passive_margin_avg_dff$age, 
     passive_margin_avg_dff$margin_oc_avg_cnt,
     t='l', #yaxt='n',
     xlab='age (Ma)',
     ylab='Number of opening and closing event',
     main = 'Passive margine opening and closing events',
     lty=1, lwd=2
)
lines(passive_margin_avg_dff$age, 
      passive_margin_avg_dff$margin_opening_avg_cnt, 
      col='green', lty=2, lwd=2)
lines(passive_margin_avg_dff$age, 
      passive_margin_avg_dff$margin_closing_avg_cnt, 
      col='red', lty=3, lwd=2)
legend('topleft', 
       legend = c('total count', 
                             'number of margin opening',
                             'number of margin closing'), 
       lty=c(1,2,3), col=c('black', 'green', 'red'),
       cex=0.75)

