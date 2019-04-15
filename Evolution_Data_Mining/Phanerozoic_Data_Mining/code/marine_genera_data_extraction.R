## spectrum analysis
# demean and detrend data
#```{r}

library(readxl)
library(plotly)
library(dplyr)

period_names <- c('cambrian',      'ordovician',   'silurian',  'devonian', 
                  'carboniferous', 'permian',      'triassic',  'jurassic', 
                  'cretaceous',    'paleogene',    'neogene',   'quaternary')

period_ages  <- c(541.00,           485.37,         443.83,      419.20,
                  358.94,           298.88,         251.90,      201.36,
                  145.01,            66.04,          23.03,        2.58 )


# project directory
proj_dir <- '/users/andy/dropbox/tscreator/tscreator development/developers/andy/projects/phanerozoic_data_mining'
# datapack directory
dp_dir <- '/users/andy/dropbox/tscreator/tscreator development/developers/andy/projects/phanerozoic_data_mining/datapack/'
setwd(dp_dir)
dp_fname <- 'marinegenera_13jan13.xls'

dfxl <- read_excel(dp_fname)
c <- colnames(dfxl) # column header
df2c <- dfxl[[c[2]]] # second column

ages_from_col3 <- na.omit(as.numeric(as.character(na.omit(dfxl[[c[3]]]))))
age_slide <- 0.1 #1 #0.050 # every 50,000 years # fraction of 1 myr
starting_age <- floor(min(ages_from_col3)) - age_slide/2
cenozoic_base <- 66.04
ending_age <- ceiling(max(ages_from_col3)) + age_slide/2
#ending_age <- ceiling(cenozoic_base)
ending_age

# second column
c <- list()
c <- colnames(dfxl)
df2c <- dfxl[[c[2]]]

# check column formats on : https://engineering.purdue.edu/stratigraphy/tscreator/download/tsc_columnformats_examples_sept2017.txt
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


# create column hierarchy. put the sub and sub sub columns under
main_columns <- main_column_names
main_column_list <- list()
main_column_info_rows <- which(dfxl[[c[2]]] == ':')

for (r in main_column_info_rows) {
  main_column <- dfxl[[c[1]]][r]
  if (main_column %in% main_columns) {
    main_column_list[[main_column]] <- list()
  } else {
    next
  }
  sub_columns <- list()
  for (rc in c[3:12]) {
    sub_column_n <- dfxl[[rc]][r]
    if(!is.null(sub_column_n) && !is.na(sub_column_n) && 
       !not_column(sub_column_n)) {
      sub_columns[[sub_column_n]] <- list()
      
      # get sub sub column row number
      sr <- which(dfxl[[c[1]]] == sub_column_n)
      if (length(sr) == 0 && dfxl[[c[2]]][sr] != ":") {
        sub_columns[[sub_column_n]] <- sub_column_n
        next
      } else {
        sub_sub_columns <- list()
      }
      
      for (src in c[3:12]) {
       for(ssr in sr) {
        sub_sub_column_n <- dfxl[[src]][ssr]
        if(!is.null(sub_sub_column_n) && !is.na(sub_sub_column_n) &&
           !not_column(sub_sub_column_n)) {
          sub_sub_columns[[sub_sub_column_n]] <- list()
          
          # get sub sub sub column row number
          ssr <- which(dfxl[[c[1]]] == sub_sub_column_n)
          if (length(ssr) == 0 || dfxl[[c[2]]][ssr] != ":") {
            sub_sub_columns[[sub_sub_column_n]] <- sub_sub_column_n
            next
          } else {
            sub_sub_sub_columns <- list()
          }
          
          # for(ssrc in c[3:12]) {
          #  for(sssr in ssr) {
          #   sub_sub_sub_column_n <- dfxl[[ssrc]][sssr]
          #   if (!is.null(sub_sub_sub_column_n) && !is.na(sub_sub_sub_column_n) &&
          #       !not_column((sub_sub_sub_column_n))) {
          #     print(sub_sub_sub_column_n)
          #     # assuming there's no more level
          #     sub_sub_sub_columns[[sub_sub_sub_column_n]] <- sub_sub_sub_column_n
          #   }
          #  }
          # }
          
          sub_sub_columns[[sub_sub_column_n]] <- sub_sub_sub_columns
        }
      }
      sub_columns[[sub_column_n]] <- sub_sub_columns
     }
    }
  }
  main_column_list[[main_column]] <- sub_columns
}


# get column wise event numbers
dir <- proj_dir 

events_by_col <- list()
event_names_by_col <- list()
ev_df_by_col <- list()
events_by_col_by_main_columns <- list()
event_names_by_col_by_main_columns <- list()
ev_df_by_main_columns <- list()
    
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
  
  data_dir <- paste(dir, '/datapack/marine_genera_event_extraction/', sep="")
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

# at each pseudolevel, we count the number of speciations or
# extinctions—encoded as counts in the code (dataset s2)—and
# the total number of species extant. for extinction calculations,
# the count of extant species includes the extinguishers (which
# form part of the pool of species that are exposed to extinction
# risk) but not originators. conversely, for speciation calculations,
# the count of extant species includes the originators but not the
# extinguishers. 
# n_fad[i] : number of species exists through speciation at period i 
# subtract the number of extinctions happened in current period i
# add the number of speciations happened in current period i
# n_fad[i] = n_fad[i-1] - lad[i] + fad[i]

n_fad <- rev(df$freq_fad) # rev(df1$`n speciations`)  
r_lad <- rev(df$freq_lad) # rev(df1$`n extinctions`) 
r_fad <- rev(df$freq_fad) # rev(df1$`n speciations`)

#n_fad[1] <- 0
for(i in 2:length(n_fad)) {
  n_fad[i] = n_fad[i-1] - r_lad[i] + r_fad[i]
}

# n_lad[i] : number of species exists at period i after extinction events in period (i-1)
# subtract the number of extinctions happened in previous period (i-1)
# add the number of speciations happened in previous period (i-1)
# n_lad[i] = n_lad[i-1] - lad[i-1] + fad[i-1]

n_lad <- rev(df$freq_lad) # rev(df1$`n extinctions`)
r_lad <- rev(df$freq_lad) # rev(df1$`n extinctions`) 
r_fad <- rev(df$freq_fad) # rev(df1$`n speciations`)  

#n_lad[1] <- 0
for(i in 2:length(n_fad)) {
  n_lad[i] = n_lad[i-1] - r_lad[i-1] + r_fad[i-1]
}

n_fad_lad = data.frame(n_fad=rev(n_fad), n_lad=rev(n_lad))
tail(n_fad_lad)

df <- cbind(df, n_fad_lad)
#df[is.na(df)] <- 0




# changing column names
df$`N.speciations` <- df$freq_fad
df$`N.extinctions` <- df$freq_lad
df$`N.turnover` <- df$freq_fad + df$freq_lad
df$`N.species.speciation` <- df$n_fad
df$`N.species.extinction` <- df$n_lad
df$`raw.speciation.probability` <- df$freq_fad/df$n_fad
df$`raw.extinction.probability` <- df$freq_lad/df$n_lad
df[is.na(df)] <- 0

# Extract only Phanerozoic
df_P <- df[df$age > 0 & df$age <= 541.5, ]

# Extract only Cenozoic
CENOZOIC_BASE = 67
df_C <- df[df$age > 0 & df$age <= CENOZOIC_BASE, ]
tail(df_C)

# Change the dff here depending on the time period being focused on
dff <- df
dff <- df_P
dff <- df_C

dff <- dff[,-c(2,3,4,5,6)]
colnames(dff)
dff$`raw.turnover.probability` <- dff$`raw.speciation.probability` + dff$`raw.extinction.probability`
which(is.na(dff))
head(dff)
tail(dff)

AGE_SLIDE = age_slide
AGE_DIFF = NA
if (floor(AGE_SLIDE) == 0) {
  AGE_DIFF = paste(AGE_SLIDE * 1000, "K", sep="")
} else {
  AGE_DIFF = paste(AGE_SLIDE, "M", sep="")
}

pfname <- paste(getwd(),"/marine_genera_event_extraction", "/marine_genera_speciation_extinction_age_slide_", AGE_DIFF, ".csv", sep="")
pfname
write.csv(dff, file=pfname, col.names = TRUE)


# Read the marine genera bin wise evolution frequency/turnover data
dff <- read.csv(file=pfname)
head(dff)

library(DT)
datatable(dff)

# plotting
plot(-dff$age, dff$`N.turnover`, t='l')
plot(-dff$age, dff$N.species.speciation, t='l')
plot(-dff$age, dff$N.species.extinction, t='l')
plot(-dff$age, dff$`raw.speciation.probability`, t='l')
plot(-dff$age, dff$`raw.extinction.probability`, t='l')
plot(-dff$age, dff$HMM.speciation.state.probability, t='l')
plot(-dff$age, dff$HMM.extinction.state.probability, t='l')
plot(-dff$age, dff$HMM.turnover.probability, t='l')

# correlation
cor(dff$raw.speciation.probability, dff$HMM.speciation.state.probability)
cor(dff$raw.extinction.probability, dff$HMM.extinction.state.probability)
cor(dff$raw.turnover.probability, dff$HMM.turnover.probability)
cor(dff$N.turnover, dff$HMM.turnover.probability)

# every 0.05 myr 
# 0.25 myr moving average
par(mfrow=c(1,1))
library(zoo)
AGE_SLIDE <- age_slide
m=0.25/AGE_SLIDE
plot(rollmean(-dff$age,m), rollmean(dff$`raw.turnover.probability`,m), t='l')
# 0.50 myr moving average
m=0.50/AGE_SLIDE
plot(rollmean(-dff$age,m), rollmean(dff$`raw.turnover.probability`,m), t='l')
# 1 myr moving average
m=1.00/AGE_SLIDE
plot(rollmean(-dff$age,m), rollmean(dff$`raw.turnover.probability`,m), t='l')
# 5 myr moving average
m=5.00/AGE_SLIDE
plot(rollmean(-dff$age,m), rollmean(dff$`raw.turnover.probability`,m), t='l')
# 10 myr moving average
m=10.00/AGE_SLIDE
plot(rollmean(-dff$age,m), rollmean(dff$`raw.turnover.probability`,m), t='l')
# 25 myr moving average
m=25.00/AGE_SLIDE
plot(rollmean(-dff$age,m), rollmean(dff$`raw.turnover.probability`,m), t='l')

#png('turnover.png')
par(mfrow=c(9,1))
par(mar=c(1,4,1,1))
plot(-dff$age, dff$N.speciations, t='l', col='green', 
     xlab='Ma', ylab='N speciation')
plot(-dff$age, dff$raw.speciation.probability, t='l', col='green',
     xlab='Ma', ylab='Raw spec. prob')
plot(-dff$age, dff$HMM.speciation.state.probability, t='l', col='green',
     xlab='Ma', ylab='HMM spec. prob')
plot(-dff$age, dff$N.extinctions, t='l', col='red', 
     xlab='Ma', ylab='N extinction')
plot(-dff$age, dff$raw.extinction.probability, t='l', col='red',
     xlab='Ma', ylab='Raw exti. prob')
plot(-dff$age, dff$HMM.extinction.state.probability, t='l', col='red',
     xlab='Ma', ylab='HMM exti. prob')
plot(-dff$age, dff$N.turnover, t='l', col='black',
     xlab='Ma', ylab='N turnover')
plot(-dff$age, dff$raw.turnover.probability, t='l', col='black',
     xlab='Ma', ylab='Raw turn. prob')
plot(-dff$age, dff$HMM.turnover.probability, t='l', col='black',
     xlab='Ma', ylab='HMM turn. prob')


# spectral
library(astrochron)
mtm(data.frame(age = dff$age, turnover = dff$`N.turnover`), detrend = T)

dat = data.frame(age = dff$age, turnover = dff$`raw.turnover.probability`)
# a) mtm - conventional mtm-AR1 approach
mtm(dat, detrend = T)
# b) lowspec ar1 - lowspec analysis of ar1 surrogate needed
lowspec(dat, detrend=T) #, tbw=2, padfac=1, pl=2, sigID=F, output=1)

# c) periodogram - conventional ar1 with 25% cosine tapered periodogram, 
# background : 1=ar1 , 2=pwrlaw
periodogram(cosTaper(dat, demean=T, detrend=T), demean=F, 
                    background=1, fNyq=F, padfac=1, output=1)

# d) mtmML96 - robust red noise mtm
mtmML96(dat)

# e) periodogram power law 25% cosine tapered periodogram, background 2
periodogram(cosTaper(dat, demean=T, detrend=T), demean=F, 
                    background=2, fNyq=F, padfac=1, output=1)


# f) mtmPL - power law fit to mtm spectrum
mtmPL(dat)

# g) lowspec - power law 


eha(data.frame(age = dff$age, turnover = dff$`N turnover`), win=20.001, step=.1, pad=4000, genplot=4, palette=5, pl=2, ydir=-1, 
    tbw=2, output=2)


head(df)
library(DT)
datatable(round(df,6))

plot(-df$age, df$`N speciations` + df$`N extinctions`, t='l')
par(mfrow=c(3,1))
plot(-df$age, df$`raw speciation probability`, t='l', col='green')
plot(-df$age, df$`raw extinction probability`, t='l', col='red')
plot(-df$age, df$`raw turnover probability`, t='l')
par(mfrow=c(3,1))
plot(-df$age, df$`HMM speciation state probability`, t='l', col='green')
plot(-df$age, df$`HMM extinction state probability`, t='l', col='red')
plot(-df$age, df$`HMM turnover probability`, t='l')

# Group wise
ev_c_r <- events_by_col_by_main_columns[['event']]
events_by_main_columns_mat <- matrix(unlist(ev_c_r), byrow = FALSE, 
                                      nrow=length(names(events_by_main_columns)), 
                                      ncol=length(main_columns), 
                                      dimnames=list(names(events_by_main_columns), 
                                                    main_columns))

# plot by columns
for(i in 1:length(main_columns)) {
  #i = 2
  Xage <- as.numeric(rownames(events_by_main_columns_mat)) * 1000
  E <- events_by_main_columns_mat
  s <- 4
  range <- c(s:length(Xage))
  A <- Xage[range]
  A <- A/1000 # converting to AD from Ka
  y_min = 0
  y_max = 25
  x_min = min(A)
  x_max = max(A)
  EV = E[,i][range]

  msg <- paste("i= ", i, " Saving age vs Phanerozoic evolution turnover plot for", main_column_names[i])
  print(msg)
  im_str <- paste(main_column_names[i], '_Phenerozoic_turnover_', AGE_SLIDE, '_mil_event_frequency', sep="")
  image_fn <- paste(dir, '/images/', im_str, ".png", sep="")
  print(image_fn)
  png(image_fn)
  plot(A, EV,  type='h', lwd=3, col=i, 
     ylim=c(y_min, y_max), xlim=c(x_min, x_max),
     xlab="Year(AD)", 
     ylab="Event count",
     xaxt='n',
     main=paste(main_columns[i])) #" turnover per million yr during Phanerozoic"))
  axis(1, at=pretty(A), labels = pretty(A))
  dev.off()
}
head(events_by_regions_mat)

m <- data.frame(events_by_regions_mat)
m$DominantRegion <- as.integer(apply(m, 1,which.max))

# There's multiple dominant regions that I am ignoring
for(r in 1:nrow(m)) {
  print(sort(m[r,]))
}
head(m)

# principal component analysis
# another example using the iris
ncol <- ncol(m)
library(kernlab)
kpc <- kpca(~.,data=m[,1:(ncol-1)], #iris[-test, -5],
            kernel="rbfdot",
            kpar=list(sigma=0.2),features=2)

#print the principal component vectors
pcv(kpc)

#plot the data projection on the components
par(mar=c(4,4,4,12))

x <- as.double(rotated(kpc)[,1])
y <- as.double(rotated(kpc)[,2])

par(xpd=FALSE)
plot(x, y, 
     #type='n',
     col=as.integer(m[,ncol]), 
     pch=as.integer(m[,ncol]),
     xlab="1st Principal Component",ylab="2nd Principal Component")
arrows(0, 0, x, y, lty=1, lwd=0.5,
       col=as.integer(m[,ncol]))
abline(h=0, lty=2)
abline(v=0, lty=2)

par(xpd=TRUE)
legend(4.725, 2.5, 
       legend = as.character(regional_columns), 
       col=1:10,
       pch=1:10)
par(xpd=FALSE)

#embed remaining points 
#emb <- predict(kpc,iris[test,-5])
#points(emb,col=as.integer(iris[test,5]))

# Number of events every 25 year
#data_dir <- ('/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/ML-Data Mining/programming/')
#setwd(data_dir)
#fn_50 <- paste(data_dir, 'event_frequency_per_50_yr_dat.txt', sep='')
#ev_f_50 <- read.csv(fn_50, sep=' ')
a <- ev_df_by_col[['block']]$age
f <- ev_df_by_col[['block']]$freq + ev_df_by_col[['event']]$freq
ev_f <- data.frame(ages = a, events = f)

#time_interval <- 2.0
#ev_f <- subset(ev_f, ev_f$ages <= time_interval)

eva <- ev_f$ages
ev <- ev_f$events

# There is one "big peak" at 500 years ago in the bins.  
#I wonder if that is affecting your "main cycle periods".  
#What would happen if you artificially reduce its magnitude by Half; 
#or simply remove that "single point"?  
#If the spectra results of "main peaks" are significantly affected by the single point, then one must wonder about the significance.
midx.ev <- which.max(ev_f$events)
mev.a <- eva[midx.ev]

# Keep a copy of original
# eva.orig <- eva
# ev.orig <- ev
# 
# ev[midx.ev] <- ev[midx.ev]/2 # reduce the magnitude in half

# plot
fn <- paste('Global_event_numbers_',
            AGE_SLIDE, '_year_bin_from_', round(STARTING_AGE), 
            '_to_', ENDING_AGE, '_ka', sep="")
im_fn <- paste(proj_dir, 'images/', fn, '.png', sep="")
png(im_fn)

# Vertical Stack plotting with R
# https://stats.stackexchange.com/questions/1838/how-do-i-vertically-stack-two-graphs-with-the-same-x-scale-but-a-different-y-sc
dev.off()
plot.new()
x_min = -2000
x_max = 5
y_min = -10
y_max = 65
# First plot the event counts

par(mar=c(6,4,6,8))
title = 'Cultural turnovers compared with Temperature data'
title = ''
plot(-eva * 1000, ev, xlab='Age(AD)', 
     ylab='', xaxt = 'n', yaxt='n', type='l', cex=0.25, col='white', lwd=2,
     xlim=c(x_min, x_max), ylim=c(y_min, y_max), 
     main=paste(title))
abline(h=0, lty=2)
#points(-eva * 1000, ev, col='red', pch=20)
axis(side=2, at=seq(-5, 40, by=5), 
     col='black', col.axis='black',lwd=2)
mtext(side=2,text='Number of cultural turnovers',line=2, lwd=3, col='black', 
      adj=0.25)
xlabels <- seq(25, 2050, by = 100)
axis(side=1, at=seq(-1975, 25, by=100), labels = xlabels,
     col='black', col.axis='black',lwd=2)
#mtext(side=2,text='#dynastic transitions',line=2, lwd=2, col='black', 
#      adj=0.25)





# **********************************----------------************************************




DACP <- c(20, 950) # 600
DACP <- DACP -2000
MWP <- c(950, 1300)
MWP <- MWP - 2000
LIA <- c(1300, 1850)
LIA <- LIA - 2000
CWP <- c(1850,2020)
CWP <- CWP - 2000

yh <-y_max-27
arrowsize <- 0.10
arrows(DACP[1],yh, DACP[2], yh, arrowsize, col='blue')
arrows(DACP[2],yh, DACP[1], yh, 0, col='blue')
x <- mean(DACP)
text(x+100,yh+15, labels = c("DACP"), col='blue')
text(x+100,yh+5, labels = c("(20-950AD)"), col='blue', cex=0.75)

arrows(MWP[1],yh, MWP[2], yh, arrowsize, col='red')
arrows(MWP[2],yh, MWP[1], yh, arrowsize, col='red')
x <- mean(MWP)
text(x,yh+15, labels = c("MWP"), col='red')
text(x,yh+5, labels = c("(950-1300AD)"), col='red', cex=0.75)

arrows(LIA[1],yh, LIA[2], yh, arrowsize, col='blue')
arrows(LIA[2],yh, LIA[1], yh, arrowsize, col='blue')
x <- mean(LIA)
text(x,yh+15, labels = c("LIA"), col='blue')
text(x,yh+5, labels = c("(1300-1850AD)"), col='blue', cex=0.75)


arrows(CWP[1],yh, CWP[2], yh, 0, col='red')
arrows(CWP[2],yh, CWP[1], yh, arrowsize, col='red')
x <- mean(CWP)
text(x,yh+15, labels = c("CWP"), col='red')
text(x,yh+5, labels = c("(1850AD-)"), col='red', cex=0.75)

# # using ggplot2
# library(ggplot2)
# df1 = data.frame(year=-eva*1000, event=ev)
# p1 = ggplot(data=df1, aes(x=year, y=event)) +
#   geom_line() +
#   geom_hline(yintercept = 0, linetype='dashed') +
#   xlab("Year(Ka)") +
#   ylab("Event count") +
#   theme(plot.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())+
#   theme(panel.border= element_blank(), 
#         panel.background = element_blank()) +
#   theme(axis.line.x.bottom = element_line(color="black"),
#         axis.line.y.left = element_line(color="black"),
#         axis.line.y.right = element_line(color="black"))
# p1

# Buntgen Central Europe temperature
# Central Europe 2500 Year Tree Ring Summer Climate Reconstructions 
library(zoo)
ys <- seq(0, 2000, by=50)
ysm <- rollmean(ys, 2)
tm <- rollmean(bun_df$TempJJA, fill="extend", 25)
t <- tm[c(ysm)]
t <- t - mean(t)
t <- rev(t)
length(ysm)
length(t)

par(new=T)
y_min2 = min(t) 
y_max2 = max(t) + 1.5
plot(-ysm, t, 
     type='l', col='blue', xlab="", ylab="", 
     axes = F, yaxt='n', 
     lwd=3,
     ylim = c(y_min2-0.5, y_max2+0.5),
     cex=0.10)
#points(-ysm, t, pch=20, col='black')
axis(2,col='black', col.axis = 'black', lwd=2, line=0)
mtext(2,text='T. Ann (°C)', col='black',
      line=2, lwd=2)
abline(h=0, lty=2, lwd=0.5, col='blue')

df2 = data.frame(year=-ysm, event=t)
# p2 = ggplot(data=df2, aes(x=year, y=event)) +
#   geom_line(color='blue') +
#   geom_hline(yintercept = 0, linetype='dashed') +
#   xlab("") +
#   ylab("C.Europe T. Ann (°C)") +
#   theme(plot.margin= margin(b=25, l=25, t=25, r=25),
#         panel.border= element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), 
#         axis.line.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.line.y.left = element_line(color="black"),
#         axis.line.y.right = element_line(color="black"))
# p2

# Stack of Northern hemisphere temperature
ys3 <- seq(0, 1973, by=50)
ysm3 <- rollmean(ys3, 2)
tm3 <- rollmean(temp_50y, fill="extend", 25)
t3 <- tm3[c(ysm3)]
t3 <- t3 - mean(t3)
t3 <- rev(t3)
length(ysm3)
length(t3)

mn <- round(min(t3),2)
mx <- round(max(t3) + 0.3, 2)
#par(new=T)
lines(-ysm3, t3, t='l', lwd=3, col='green')
#, axes=F, xlab="", ylab="", cex=0.5,
#ylim =c (mn, mx), lwd=1)
#points(-ysm, t2, pch=20, col='blue')

#axis(4, col='green', col.axis='green',lwd=1)
#mtext(4,text='NH T. Ann (°C)',line=2, lwd=2, col='green')

df3 = data.frame(year=-ysm3, event=t3)
# p3 = ggplot(data=df3, aes(x=year, y=event)) +
#   geom_line(color='red') +
#   geom_hline(yintercept = 0, linetype='dashed') +
#   xlab("") +
#   ylab("NH T. Ann (°C)") +
#   theme(plot.margin= margin(b=50, l=40, t=40, r=40),
#         panel.border= element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), 
#         axis.line.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.line.y.left = element_line(color="black"),
#         axis.line.y.right = element_line(color="black"))
# p3

#Previous Tree-Ring-Based Northern Hemisphere Temperature Reconstructions Portray A Varying Amplitude Range Between The "Medieval Warm Period'' (MWP), "Little Ice Age'' (LIA) And Present. We Describe A New Reconstruction, Developed Using Largely Different Methodologies And Additional New Data Compared To Previous Efforts. Unlike Earlier Studies, We Quantify Differences Between More Traditional ( STD) And Regional Curve Standardization (RCS) Methodologies
yr4 <- nh_arrigo_df$year
yrseq4 <- seq(min(yr4), max(yr4), by=50)
ysm4 <- rollmean(yrseq4, fill='extend', 2)
ysm4 <- ysm4 - min(ysm4) + 1
tm4 <- rollmean(nh_arrigo_df$rcs_t, fill="extend", 25)
t4 <- tm4[c(ysm4)]
t4 <- t4 - mean(t4)
t4 <- rev(t4)

length(ysm4)
length(t4)

lines(-ysm4, t4, t='l', lwd=3, col='red')

# NGRIP oxy-18 data, code below for getting the data
par(new=T)
x = -rollmean(ngrip_age, 3)
y = rollmean(ngrip_oxy - mean(ngrip_oxy), 3)
y_min2 = min(y) 
y_max2 = max(y) + 1.5
plot(x, y, 
     type='l', col='orange', xlab="", ylab="", lwd =2,
     axes = F, yaxt='n', 
     ylim = c(y_min2-0.5, y_max2+0.5),
     cex=0.10)
#points(-ysm, t, pch=20, col='black')
yat = seq(y_min2, y_max2)
ylabels = yat + mean(ngrip_oxy) 
axis(4,at = yat, labels = ylabels, col='orange', col.axis = 'orange', 
     tick=T, lwd=2, line=2)
mtext(4,text='NGRIP Oxy-18', col='orange',
      line=4, lwd=2)


legend('topleft',legend=c('C. Europe T. Ann', 'Stacked NH T. Ann', 'Tree ring NH RCS T. Ann', 'NGRIP Oxy-18 T. proxy', 'Number of dynastic transitions'),
       col=c('blue','green', 'red', 'orange', 'black'), lty=1, cex=0.620)



library(grid)
dev.off()
plot.new()
grid.newpage()
grid.draw(rbind(ggplotGrob(p3), 
                ggplotGrob(p2),
                ggplotGrob(p1),
                size = "first"))


dev.off()



# demean event data
eva <- orig_eva
ev <- orig_ev

orig_eva <- eva
orig_ev <- ev

eva <- eva[1:76]
ev <- ev[1:76]

Pspec<- spec.pgram(ev, demean = F, detrend = F, 
                   taper = 0, pad=0, log='no', plot=T)
par(new=T)
plot(Pspec$freq, Pspec$spec, t='l', col='red', lty=2)
dfPspec <- data.frame(Pspec)
dfPspec <- dfPspec[order(dfPspec$spec, decreasing = T),]
time_window <- max(eva) - min(eva)
dfPspec$period <- (time_window / (dfPspec$freq * length(ev))) * 1000
head(dfPspec, n=15)
dfPspecp <- dfPspec[order(dfPspec$period, decreasing = F),]
head(dfPspecp, n=10)
par(mfrow=c(1,1))
plot(dfPspecp$period, dfPspecp$spec/sum(dfPspecp$spec), t= 'l', 
     xlim=c(0,550), ylim=c(0, 0.20), xlab='year', ylab='spectral power')

ev.dm <- ev - mean(ev)

# plot
plot(eva, ev.dm, xlab='Age(Ka)', 
     ylab='Number of Events', type='p', cex=0.25, col='blue', lwd=2,
     main='Demeaned Event Number Data', ylim=c(-10,40)
)
lines(eva, ev.dm, col='blue')
abline(h=0, lty=2, col='blue')

hanning_ma <- function(df, n) {
  prev <- NA
  nextt <- NA
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
  x <- 0.5 * df[n] + 0.25*(prev + nextt)
  return(x)
}
ev.filt <- ev.dm
# filter event data
ev.filt <- c()
i <- 1
nr <- length(eva)
for (n in 1:(nr)) {
  h <- hanning_ma(ev.dm, n)
  ev.filt <- c(ev.filt, h)
}

nr <- length(ev_f$ages)
plot(eva, ev.filt, t='l', col='black', lwd=2)
lines(eva, ev.dm, t='l', col='blue')
abline(h=0, lty=2, col='black')



# detrend
#pev <- prewhiten(ev.filt)
#ev.dtrend <- pev[['prew_lm']]
nr <- length(eva)
lmfit <- lm(ev.filt ~ eva)
ev.dtrend <- lmfit$residuals
plot(eva, ev.dtrend, t='l', col='black', lwd=2, main='Demeaned + Detrended')
lines(eva, ev.dm)
lines(eva, ev.filt, t='l', col='blue', lty=2)
abline(h=0, lty=2, col='black')

# taper event data
ev.tap <- ev.dtrend
i <- 1
nr <- length(as.numeric(eva))
n <- length(ev.filt)

tap_perc_n <- n * 10/100 # 10% tapering to reduce the effect of the terminations of data at the ends of the window

for (j in 1:tap_perc_n) {
  w = j * pi / tap_perc_n
  ev.tap[j] = ev.tap[j] * 0.5 * (1-cos(w))
}
for (j in (n-tap_perc_n):n) {
  w = (512-j) * pi / tap_perc_n
  ev.tap[j] = ev.filt[j] * 0.5 * (1-cos(w))
}

# plot
plot(as.numeric(eva), ev.tap, col='black', 
     lty=1, t='l', lwd=2,
     main='Demeaned + Detrended + Tapered')
lines(eva, ev.filt, t='l', col='blue', lty=2)
abline(h=0, lty=2)

Pspec<- spec.pgram(ev.dtrend, demean = T, detrend = T, 
                   taper = 0.20, pad=0.1, log='no', plot=F)
plot(Pspec$freq, Pspec$spec, t='l', col='red', lty=2)
dfPspec <- data.frame(Pspec)
dfPspec <- dfPspec[order(dfPspec$spec, decreasing = T),]
time_window <- max(eva) - min(eva) + eva[2]-eva[1]
dfPspec$period <- (time_window / (dfPspec$freq * length(ev.tap))) * 1000
head(dfPspec, n=15)
dfPspecp <- dfPspec[order(dfPspec$period, decreasing = F),]
head(dfPspecp, n=10)
par(mfrow=c(2,2))
plot(dfPspecp$period, dfPspecp$spec/sum(dfPspecp$spec), t= 'l', 
     xlim=c(0,550), ylim=c(0, 0.20), xlab='year', ylab='spectral power')

# Spectral Analysis
# FFT
ev.fft <- fft(ev.tap)

# Calculate Amplitude
Amp <- Mod(ev.fft)

# Calculate Power Spectrum
PowerSpec <- Amp^2

# Take half of it , coz mirrored
PowerSpec <- PowerSpec[1:(length(Amp)/2)]

PowerSpec.han <- c()
i <- 1
nr <- length(PowerSpec)
for (n in 2:(nr-1)) {
  h <- hanning_ma(PowerSpec, n)
  PowerSpec.han <- c(PowerSpec.han, h)
}

t <- as.numeric(ev_f$ages)
window_width <- max(t) - min(t)
freq <- 1:(length(PowerSpec)-2)
period <- window_width/freq 

TotPowerSpec <- sum(PowerSpec)
RelPowerSpec <- PowerSpec.han/TotPowerSpec

# Charts of Wavenumber vs. Smoothed Power 
plot(freq, PowerSpec.han, t='l', 
     xlab='Frequency (Cycles / 2Ka)', 
     ylab='Power Spectra [Amplitude]^2',
     lwd=2
)


plot(period*1000, PowerSpec.han, t='l', 
     xlab='Time (Yr)', 
     ylab='Power',
     xlim=c(0,1000),
     lwd=2,
     main="Spectral Power vs Period"
)
library(stepR)
stat <- monteCarloSimulation(n=length(ev.tap))
identical(critVal(n = length(ev.tap), alpha = 0.5, stat = stat),
          critVal(n = length(ev.tap), alpha = 0.5,
                  options = list(load = list(), simulation = "matrix")))

dfpc <- data.frame(cycle = period*1000, power = PowerSpec.han)
dfpc <- dfpc[order(dfpc$power, decreasing = T),]
head(dfpc, 15)

np <- 10
periodl <- round(dfpc$cycle[1:np], 2)
abline(v=periodl, lty=2, col=1:np)
periodl <- paste(periodl, 'yr')
legend('topright', legend=periodl, lty=2, col=1:np, cex=0.5)



# )Wavelength vs. Relative Power Spectra
plot(RelPowerSpec, t='l', 
     xlab='Year (Ka)', 
     ylab='Relative Power Spectra [Amplitude]^2',
     main='Spectral analysis using single split cosine taper method',
     xaxt='n'
)
idx <- seq(1,max(freq),by=12)
axis(side=1, at=idx, labels=FALSE)
mtext(side=1, padj = 1, text=round(period[idx],2), at=freq[idx])

# Use the custom frequency spectrum
# on the real data
plot.spectrum(ev)
# on the demeaned data
plot.spectrum(ev.dm, power=TRUE, xlab='Frequency(cycles/2ka)', 
              main = 'Spectral power vs frequency on demeaned event data')
# on the hanning filtered data
plot.spectrum(ev.filt, power=TRUE, xlab='Frequency(cycles/2ka)',
              main = 'Spectral power vs frequency on demeaned and filtered event data')
# on the detrended data
plot.spectrum(ev.dtrend, power=TRUE, xlab='Frequency(cycles/2ka)',
              main = 'Spectral power vs frequency on detrended event data')
# on the tapered data
plot.spectrum(ev.tap, power=TRUE, xlab='Frequency(cycles/2ka)',
              main = 'Spectral power vs frequency on detrended and tapered event data')

# smooth the psd
res <- plot.spectrum(ev.tap, power=TRUE, smoothing=TRUE,
                     xlab='Frequency(cycles/2ka)',
                     main = 'Smoothed Spectral power vs frequency')


time_window <- max(ev_f$ages) - min(ev_f$ages)
N <- length(ev.tap)
res$period <- (time_window / (res$freq * N)) * 1000
head(res, 10)

plot(period*1000, PowerSpec.han, t='l', 
     xlab='Time (Yr)', 
     ylab='Power',
     xlim=c(0,1000),
     lwd=2,
     main="Spectral Power vs Period"
)
periodl <- c(400, 154, 250, 333.33, 100, 166.67)
abline(v=periodl, lty=2, col=2:7)
periodl <- paste(periodl, 'yr')
legend('topright', legend=periodl, lty=2, col=2:7)

# freq       amp     power    period
# 6  0.06329114 122.99625 158.83075  400.0000 Yes
# 14 0.16455696 107.57012  99.50760  153.8462 Yes
# 5  0.05063291 107.54739 131.11470  500.0000
# 9  0.10126582  96.29151  63.44699  250.0000 Yes
# 7  0.07594937  91.47636 104.76960  333.3333 Yes
# 21 0.25316456  89.00611  56.19316  100.0000 Yes
# 17 0.20253165  78.75561  42.57242  125.0000
# 3  0.02531646  72.24159  45.31505 1000.0000
# 19 0.22784810  67.90576  30.50830  111.1111 
# 13 0.15189873  67.54380  66.50646  166.6667 Yes


hm <- c(6, 14, 9, 7, 17)
# Reconstruct the plot with harmonics
plot.show(ev.tap, plot.freq = F, harmonics = hm, plot.harmnic=F, scale=3)
abline(h=0, lty=2)
legend('topright', legend=c(periodl, 'Combined', 'Event Number'), 
       lty=1, col=c(1:6, 'darkblue', 'black'), cex=0.65)

# Using periodogram
Pspec<- spec.pgram(ev.tap, demean = TRUE, detrend = TRUE, taper = 0.5, log='no')
Pspec<- spec.pgram(ev.tap, demean = TRUE, detrend = TRUE, taper = 0.1, log='no')
Pspec<- spec.pgram(ev.filt, demean = TRUE, detrend = TRUE, taper = 0, pad=0, log='no')
par(new=T)
plot(Pspec$freq, Pspec$spec, t='l', col='red', lty=2)
dfPspec <- data.frame(Pspec)
dfPspec <- dfPspec[order(dfPspec$spec, decreasing = T),]
time_window <- max(eva) - min(eva) + eva[2] - eva[1]
dfPspec$period <- (time_window / (dfPspec$freq * length(ev.tap))) * 1000
head(dfPspec, n=10)
plot(dfPspec$period, dfPspec$spec, t= 'l', xlim=c(0,550))
#     freq     spec  taper    period
# 5  0.0625 45.40252   0.1  405.0633
# 4  0.0500 35.09184   0.1  506.3291
# 14 0.1750 31.08893   0.1  144.6655
# 8  0.1000 29.57111   0.1  253.1646
# 20 0.2500 27.10280   0.1  101.2658
# 6  0.0750 24.99689   0.1  337.5527
# 2  0.0250 21.18668   0.1 1012.6582
# 18 0.2250 19.44715   0.1  112.5176
# 13 0.1625 17.98304   0.1  155.7936
# 12 0.1500 12.35700   0.1  168.7764

# Using blackman-tuckey
BTspec <- spec.BT(ev.tap,dt = 1/50, lag=1/3, unit=' 2Ka')
dfBTspec <- data.frame(BTspec) 
dfBTspec <- dfBTspec[order(dfBTspec$spec, decreasing = T), ]
dfBTspec$period <- (time_window / dfBTspec$freq) * 1000
head(dfBTspec, n=10)

#     # cycles       spec    period
# 3   3.846154 2.81984692  520.0000
# 2   1.923077 2.80302898 1040.0000
# 5   7.692308 1.60448186  260.0000
# 4   5.769231 0.86358185  346.6667
# 6   9.615385 0.59535222  208.0000
# 7  11.538462 0.46948344  173.3333
# 1   0.000000 0.36902539       Inf
# 8  13.461538 0.29816814  148.5714
# 9  15.384615 0.09969757  130.0000
# 10 17.307692 0.03152900  115.5556

# Using psd 
## Not run: #REX
library(psd)
##
## Using prewhiten to improve spectral estimates
##
# Prewhiten by removing mean+trend, and
# AR model; fit truncates the series by
# a few terms, so zero pad
mts <- prewhiten(ev.tap, AR.max=5, zero.pad="rear")
mts.p <- mts[['prew_lm']]
mts.par <- mts[['prew_ar']]
# uniformly-tapered spectral estimates
PSD <- psdcore(mts.p, ntaper=20)
plot(PSD, log = 'no')
PSD.ar <- psdcore(mts.par, ntaper=20)
# remove the effect of AR model
PSD.ar[['spec']] <- PSD.ar[['spec']] / mean(PSD.ar[['spec']])
PSD[['spec']] <- PSD[['spec']] / PSD.ar[['spec']]
plot(PSD, log='no', lwd=2)
#plot(PSD, log='dB', add=TRUE, lwd=2, col="red")
#plot(PSD.ar, log='no', add=TRUE, col="blue", lwd=2)
plot(PSD.ar$freq, PSD.ar$spec, t='l')
PSDdf <- data.frame(PSD)
PSDdf <- PSDdf[order(PSDdf$spec, decreasing = T),]
PSDdf$period <- (time_window /(PSDdf$freq * length(ev.tap)))*1000
head(PSDdf, 10)
# Not very effective for frequency resolution, 

# Using multitaper
library(multitaper)
Mspec <- spec.mtm(ev.dtrend, deltat=1,
                  nw=4, k=7, 
                  log='no',
                  sineSmoothFact = 0.02,
                  xlab='Frequency',
                  ylab='Power',
                  Ftest = TRUE
                  #,jackknife=TRUE
)
plot(Mspec$freq, Mspec$spec, t='l', lwd=2)
Mspecdf <- data.frame(freq=Mspec$freq, spec=Mspec$spec)
Mspecdf <- Mspecdf[order(Mspecdf$spec, decreasing = T),]
Mspecdf$period <- (time_window /(Mspecdf$freq * length(ev.tap)))*1000
head(Mspecdf, 20)

abline(v=0.05859375, col='red') # 432 yr
abline(v=0.07031250, col='green') # 360 yr
abline(v=0.08984375, col='blue') #281

### With spec.pgram --- ambiguous
N <- length(ev.tap)
ev.mtmdf <- data.frame(1:N, ev.tap)
ev_model <- linterp(ev.mtmdf, 
                    dt=1)
eh <- eha(ev_model,
          tbw=7, 
          win=20,  # Make less than 50 for evolutive 
          pl=2, output=2, genplot = 4)

# Make win > 100
ehdf <- data.frame(eh)
ehdf <- ehdf[order(ehdf$Harmonic_CL, decreasing = T),]
ehdf$period <- (time_window/(ehdf$Frequency * N)) * 1000
head(ehdf, n = 10)

# Frequency Harmonic_CL   period
# 4 0.1679688   0.9899981 150.7212
# 2 0.0625000   0.9803090 405.0633
# 5 0.2500000   0.9685964 101.2658
# 3 0.0781250   0.9509427 324.0506
# 1 0.0468750   0.9086676 540.0844

library(astrochron)
Mspec <- mtm(ev.mtmdf, demean = T, 
             ntap = 5, tbw = 7, ar1 = T,
             output = 3, pl=2)
Mspecdf <- data.frame(Mspec)
Mspecdf <- ehdf[order(Mspecdf$Harmonic_CL, decreasing = T),]
Mspecdf$period <- (time_window/(Mspecdf$Frequency * N)) * 1000
head(Mspecdf, n = 10)

# Frequency Harmonic_CL   period
# 2 0.0625000   0.9803090 405.0633
# 4 0.1679688   0.9899981 150.7212
# 5 0.2500000   0.9685964 101.2658

x=as.numeric(y) #temp_sliding_50y) #(temp_recon)
t=as.numeric(yr) #yr_sliding_50y) #(yr)
redf.dat.chr <- redfit(x=x, nsim = 1000, mctest=TRUE)
redf.dat <- redf.dat.chr
#par(tcl = 0.5, mar = rep(3, 4), mgp = c(1.1, 0.1, 0))
plot(redf.dat[['freq']], redf.dat[['gxx']],
     xlim = c(0, 0.01),
     #ylim = range(redf.dat[["ci99"]], redf.dat[["gxx"]]),
     #type ='l',
     #lwd=2,
     type = "n", axes = FALSE,
     ylab = "Spectral Power (dB)", 
     xlab = "Frequency (1/yr)"
)
grid()
lines(redf.dat[["freq"]], redf.dat[["gxx"]], col = "black", lwd=2)
lines(redf.dat[["freq"]], redf.dat[["ci99"]], col = "red")
lines(redf.dat[["freq"]], redf.dat[["ci95"]], col = "pink")
lines(redf.dat[["freq"]], redf.dat[["ci90"]], col = "green")
lines(redf.dat[["freq"]], redf.dat[["ci80"]], col = "orange")
Rspecdf <- data.frame(freq=redf.dat[["freq"]], 
                      spec=redf.dat[["gxx"]],
                      period=1/redf.dat[["freq"]])
Rspecdf <- Rspecdf[order(Rspecdf$spec, decreasing = T),]
head(Rspecdf, 20)
Rspecdf <- Rspecdf[Rspecdf$freq <= 0.01, ]
freqs <- pretty(Rspecdf$freq)
pers <- round(1 / freqs, 2)
axis(1, at = freqs, labels = TRUE)
axis(3, at = freqs, labels = pers)
mtext(text = "Period (yr)", side = 3, line = 2)
axis(2); #axis(4)
legend("topright", c("dat", "CI99", "CI95", "CI90", "CI80"), lwd = 2,
       col = c("black", "red", "pink", "green"),
       bg = "white", cex=0.5)
box()




# Greenland NGRIP Oxy-18 (b2k)
## Investigation of NGRIP delta-Oxy-18 data
# In geochemistry, paleoclimatology and paleoceanography delta-O-18 is a 
# measure of the ratio of stable isotopes oxygen-18 (18O) and oxygen-16 (16O). It is commonly used as a measure of the temperature of precipitation, as a measure of groundwater/mineral interactions, and as an indicator of processes that show isotopic fractionation, like methanogenesis. In paleosciences, \( ^{18}O:^{16}O \) data from corals, foraminifera and ice cores are used as a proxy for temperature. The definition is, in "per mil" (‰, parts per thousand):
# \(
#    \delta^{18}O = \left(
#                         \frac{
#                               \left(  
#                                     \frac{
#                                         ^{18}O
#                                          }{
#                                         ^{16}O}
#                               \right)_{sample}
#                              }
#                             {
#                               \left(
#                                     \frac{
#                                         ^{18}O
#                                          }{
#                                         ^{16}O
#                                          }
#                               \right)_{standard}
#                             }
#                   -1 \right) * 1000% 
# \)
# 
# We have delta-Oxy-18 data for every 20 years for the holocene stage(0~12Ka)
# here. We can see the summary of the data below. The histogram also shows the 
# frequency of delta-Oxy-18 data.

## Extrapolation of temperature
# The stable isotope ratio, (O18/O16), is the main reference parameter, 
# since its variability is determined mainly by the cloud temperature at 
# the moment of snow formation and thus has direct climatic relevance, 
# assuming unchanged temperature and humidity at the original moisture 
# source areas. On the greenland Ice Sheet, the present mean annual 
# delta-Oxy18 ( the per mil deviation of O18/)16 ratio in the sample 
# from the O18/O16 value in standard mean ocean water) of the snow is 
# related closely to the mean annual surface temperature, T in degree 
# Celsius, by the formula (Johnsen et al 1997)
# \( \delta = 0.67 T - 0.137 \)
# \(T = (\delta + 0.137) / 0.67 \)
# 
# The GRIP calibration curve in Figure2(Johnsen et al 1997) is based on a 
# slightly improved model for the temperature profile calculations by 
# accounting fully for the thermal properties of the firn layer. This 
# resulted in slight changes in the 8 - T relationship published 
# earlier [Johnsenet al., 1995a],from 1.7 to 2.0øC/%o at -35%0 and 
# from 3.5 to 3.1øC/%oat -42%0. 

# Greenland NGRIP Oxy-18 (b2k) data
ngrip_age_full <- as.numeric(columns$point[[1]]$content$age)
ngrip_oxy_full <- as.numeric(columns$point[[1]]$content$name)
an <- which(ngrip_age_full == 2.00)
ngrip_age <- 2014 - (ngrip_age_full[0:an] * 1000)
ngrip_oxy <- ngrip_oxy_full[0:an]

ngrip_oxy_2000_df <- data.frame(age=ngrip_age, 
                                oxy18=ngrip_oxy)
deltaOxyToTemp <- function(delta) {
  alpha <- -211.4
  beta <- -11.88
  gamma <- -0.1925
  Temp <- alpha + beta * delta + gamma * delta^2
  return(Temp)
}

ngrip_oxy_2000_df$temp <- deltaOxyToTemp(ngrip_oxy_df$oxy18)
plot(ngrip_oxy_2000_df$age, ngrip_oxy_2000_df$oxy18, 
     t='l', lwd=2, xlab='Period (Ka)', ylab='Delta delta-18O (%.)', 
     main='NGRIP delta-Oxy-18 vs Year')
plot(ngrip_oxy_2000_df$age, ngrip_oxy_2000_df$temp, 
     lwd=2, t='l',
     xlab='Period (Ka)', ylab='Temperature (°C)',
     main='NGRIP delta-Oxy-18 derived Temperature (°C) vs Year')
head(ngrip_oxy_2000_df)

k1 <- kernel('daniell', 1)
oxy18.a <- kernapply(ngrip_oxy_2000_df$age, k1)
oxy18.k1 <- kernapply(ngrip_oxy_2000_df$oxy18, k1)
lines(oxy18.a, oxy18.k1, col='red') 

k2 <- kernel('daniell', 2)
oxy18.a2 <- kernapply(ngrip_oxy_2000_df$age, k2)
oxy18.k2 <- kernapply(ngrip_oxy_2000_df$oxy18, k2)
lines(oxy18.a2, oxy18.k2, col='green') 

k3 <- kernel('modified.daniell', c(1,1))
oxy18.a3 <- kernapply(ngrip_oxy_2000_df$age, k3)
oxy18.k3 <- kernapply(ngrip_oxy_2000_df$oxy18, k3)
lines(oxy18.a3, oxy18.k3, col='darkblue') 

Pspec<- spec.pgram(ngrip_oxy_2000_df$oxy18, 
                   #kernel = k3,
                   demean = TRUE, detrend = TRUE,
                   taper = 0.1, log='no', plot=T)


plot(Pspec$freq, Pspec$spec, t='l', 
     xlim=c(0, 0.2), ylim=c(0,0.4), lwd=2,
     main="Spectral Power vs frequency for NGRIP delta-Oxy18 data",
     xlab='Frequency(1/yr)',
     ylab='Spectral Power'
)
dfPspec <- data.frame(freq = Pspec$freq, spec=Pspec$spec)
dfPspec <- dfPspec[order(dfPspec$spec, decreasing = T),]
time_window <- max(ngrip_oxy_2000_df$age) - min(ngrip_oxy_2000_df$age)
dfPspec$period <- 1 / dfPspec$freq * 
  (time_window / length(ngrip_oxy_2000_df$oxy18)) * 1000
dfPspec <- dfPspec[dfPspec$period >= 90,]
head(dfPspec, n=10)
# freq       spec   period
# 7  0.07 0.35022221 282.8571
# 3  0.03 0.31488731 660.0000
# 4  0.04 0.28987502 495.0000
# 12 0.12 0.25377032 165.0000
# 14 0.14 0.21836049 141.4286
# 15 0.15 0.16631844 132.0000
# 20 0.20 0.14631037  99.0000
# 8  0.08 0.09715666 247.5000
# 9  0.09 0.09511752 220.0000
# 10 0.10 0.09159793 198.0000
freql <- head(dfPspec$freq, n=10)
period <- head(dfPspec$period, n=10)
legnd <- head(dfPspec$period, n=10)

abline(v=freql, lty=2, col=1:length(legnd))
periodl <- round(head(1/freql), 0)
legnd <- paste(round(legnd, 0), ' yr', sep="")
legend('topleft', legend=legnd, 
       lty=2, col=1:length(freql), cex=0.75)

dfPspecP <- dfPspec[order(dfPspec$period, decreasing = F),]
head(dfPspecP)
plot(dfPspecP$period, dfPspecP$spec, 
     type='l', lwd=2, xlim=c(0, 900), ylim=c(0,0.35),
     main="Spectral Power vs period for NGRIP delta-Oxy18 data",
     xlab='Period (yr)',
     ylab='Spectral Power')

abline(v=period, lty=2, col=1:length(legnd))
periodl <- paste(round(period, 0), 'yr')
legend('topright', legend=periodl, 
       lty=2, col=1:length(periodl), cex=0.75)

head(dfPspec, n=20)


library(astrochron)

x<- ngrip_oxy_2000_df$age #yr_sliding_50y 
y<- ngrip_oxy_2000_df$oxy18 #as.numeric(temp_sliding_50y)
yorig <- y
y <- y-mean(y)
#plot(x, y, col=2, t='l')
y <- y/sd(yorig)
#plot(x, y, col=3, t='l')
lmf <- lm(y~x)
y <- y.dtrnd <- lmf$residuals
df <- data.frame(age=x, 
                 oxy18=y)
df_model <- linterp(df, dt=0.02)
Mspec <- mtm(df_model, demean = T, 
             ntap = 7, tbw = 3, #ar1 = T,
             output = 1, pl=2)

Mspecdf <- data.frame(Mspec)
Mspecdf <- Mspecdf[order(Mspecdf$AR1_CL, decreasing = T),]
time_window <- max(df$age) - min(df$age)
N <- length(df$oxy18)
Mspecdf$period <- (1/Mspecdf$Frequency) * 1000 #* (time_window / N)
head(Mspecdf[,c(1,2, 3, 9)], n = 30)

library(dplR)
x=as.numeric(y) #temp_sliding_50y) #(temp_recon)
t=as.numeric(x) #yr_sliding_50y) #(yr)
redf.dat.oxy <- redfit(x=x, nsim = 1000, mctest=TRUE)
redf.dat <- redf.dat.oxy
#par(tcl = 0.5, mar = rep(3, 4), mgp = c(1.1, 0.1, 0))
plot(redf.dat[['freq']], redf.dat[['gxx']],
     xlim = c(0, 0.01),
     #ylim = range(redf.dat[["ci99"]], redf.dat[["gxx"]]),
     #type ='l',
     #lwd=2,
     type = "n", axes = FALSE,
     ylab = "Spectral Power (dB)", 
     xlab = "Frequency (1/yr)"
)
grid()
lines(redf.dat[["freq"]], redf.dat[["gxx"]], col = "black", lwd=2)
lines(redf.dat[["freq"]], redf.dat[["ci99"]], col = "red")
lines(redf.dat[["freq"]], redf.dat[["ci95"]], col = "pink")
lines(redf.dat[["freq"]], redf.dat[["ci90"]], col = "green")
lines(redf.dat[["freq"]], redf.dat[["ci80"]], col = "orange")
Rspecdf <- data.frame(freq=redf.dat[["freq"]], 
                      spec=redf.dat[["gxx"]],
                      period=1/redf.dat[["freq"]])
Rspecdf <- Rspecdf[order(Rspecdf$spec, decreasing = T),]
head(Rspecdf, 20)
Rspecdf <- Rspecdf[Rspecdf$freq <= 0.01, ]
freqs <- pretty(Rspecdf$freq)
pers <- round(1 / freqs, 2)
axis(1, at = freqs, labels = TRUE)
axis(3, at = freqs, labels = pers)
mtext(text = "Period (yr)", side = 3, line = 2)
axis(2); #axis(4)
legend("topright", c("dat", "CI99", "CI95", "CI90", "CI80"), lwd = 2,
       col = c("black", "red", "pink", "green"),
       bg = "white", cex=0.5)
box()

# Causality
library(vars)
data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
causality(var.2c, cause = "e")
#use a robust HC variance-covariance matrix for the Granger test:
causality(var.2c, cause = "e", vcov.=vcovHC(var.2c))
#use a wild-bootstrap procedure to for the Granger test
## Not run: causality(var.2c, cause = "e", boot=TRUE, boot.runs=1000)