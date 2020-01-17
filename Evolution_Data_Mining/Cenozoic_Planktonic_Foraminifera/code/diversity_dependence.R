plot(PF_dff$age, PF_dff$N.species.extinction, t='l')
par(new=T)
plot(PF_dff$age, PF_dff$N.speciations, t='l', col='green', axes=F, xlab='', ylab='')

plot(PF_dff$N.species.extinction, PF_dff$N.speciations, 
     col='green', xlab='', ylab='Speciation')
points(PF_dff$N.species.extinction, PF_dff$N.extinctions, col='red', ylab='')
plot(PF_dff$N.species.extinction, PF_dff$N.turnover)


plot(PF_dff$N.species.speciation, PF_dff$N.speciations)
plot(PF_dff$N.species.speciation, PF_dff$N.extinctions)

df_S_D = data.frame(age=PF_dff$age, 
                    diversity=PF_dff$N.species.extinction,
                    speciation=PF_dff$N.speciations)
df_SE_D = data.frame(age=PF_dff$age, 
                    diversity=PF_dff$N.species.extinction,
                    speciation=PF_dff$N.speciations,
                    extinction=PF_dff$N.extinctions)


# Speciation Diversity dependence
# S: Random variable for speciation count
# s_k : k-th speciation value     S_min <= s_k <= S_max

# D : Random variable for diversity (Number of existing species in the last pseudolevel)
# d_j : j-th diversity value      D_min <= d_j <= D_max

# Probability of speciation related with diversity
# P(S = s_k | D = d_j) =  P(S = s_k, D = d_j) / P(D=d_j)
# P_sk_dj = P_skdj / P_dj

# Calculate P_dj
D = PF_dff$N.species.extinction
D_sorted = sort(D)
d_v = unique(D_sorted)
Td = table(D_sorted)
T_dx = as.integer(names(Td))
T_dy = as.integer(Td)
N = dim(df_S_D)[1]
T_ddf = data.frame(dj=T_dx, dj_freq=T_dy, P_dj=T_dy/N)
head(T_ddf)
sum(T_ddf$P_dj)

# Calculate P_sk
S = PF_dff$N.speciations
S_sorted = sort(S)
s_v = unique(S_sorted)
Ts = table(S_sorted)
T_sx = as.integer(names(Ts))
T_sy = as.integer(Ts)
T_sdf = data.frame(sk=T_sx, sk_freq=T_sy, P_sk=T_sy/N)
plot(T_sdf$sk, T_sdf$sk_freq)
head(T_sdf)
sum(T_sdf$P_sk)

# Calculate P_skdj
i <- 1
Tskdj_df <- data.frame()
for(i in 1:N) {
  a = df_S_D[i,]
  tdf <- data.frame(sk=a$speciation, dj=a$diversity, skdj_freq=1)
  rownames(tdf) <- paste(a$speciation, '_', a$diversity, sep='')
  
  if (rownames(tdf) %in% rownames(Tskdj_df)) {
    id <- which(rownames(Tskdj_df) == rownames(tdf))
    Tskdj_df[id,]$skdj_freq <- Tskdj_df[id,]$skdj_freq + 1
  } else {
    Tskdj_df <- rbind(Tskdj_df, tdf)
  }
}
head(Tskdj_df)
dim(Tskdj_df)
sum(Tskdj_df$skdj_freq)
Tskdj_df$P_skdj = Tskdj_df$skdj_freq/N
sum(Tskdj_df$P_skdj)

# Calculate P_sk_dj 
i <- 1
NN <- dim(Tskdj_df)[1]
c_P_sk_dj <- c()
c_P_sk <- c()
c_P_dj <- c()
for(i in 1:NN) {
  r <- Tskdj_df[i,]
  sk <- r$sk
  dj <- r$dj
  P_skdj <- r$P_skdj
  idj <- which(T_ddf$dj == dj)
  isk <- which(T_sdf$sk == sk)
  
  P_dj <- T_ddf[idj,]$P_dj
  P_sk <- T_sdf[isk,]$P_sk
  P_sk_dj <- P_skdj / P_dj
  
  c_P_sk_dj <- c(c_P_sk_dj, P_sk_dj)
  c_P_sk <- c(c_P_sk, P_sk)
  c_P_dj <- c(c_P_dj, P_dj)
}

Tskdj_df$P_sk <- c_P_sk
Tskdj_df$P_dj <- c_P_dj
Tskdj_df$P_sk_dj <- c_P_sk_dj

library(DT)
rownames(Tskdj_df) <- seq(1,NN)
Tskdj_df <- round(Tskdj_df, 4)
datatable(Tskdj_df)


# Extinction Diversity dependence
# E: Random variable for extinction count
# e_k : k-th extinction value     E_min <= e_k <= E_max

# D : Random variable for diversity (Number of existing species in the last pseudolevel)
# d_j : j-th diversity value      D_min <= d_j <= D_max

# Probability of speciation related with diversity
# P(E = e_k | D = d_j) =  P(E = e_k, D = d_j) / P(D=d_j)
# P_ek_dj = P_ekdj / P_dj

# Calculate P_dj
D = PF_dff$N.species.extinction
D_sorted = sort(D)
d_v = unique(D_sorted)
Td = table(D_sorted)
T_dx = as.integer(names(Td))
T_dy = as.integer(Td)
N = dim(df_S_D)[1]
T_ddf = data.frame(dj=T_dx, dj_freq=T_dy, P_dj=T_dy/N)
head(T_ddf)
sum(T_ddf$P_dj)

# Calculate P_ek
E = PF_dff$N.extinctions
E_sorted = sort(E)
e_v = unique(E_sorted)
Te = table(E_sorted)
T_ex = as.integer(names(Te))
T_ey = as.integer(Te)
T_edf = data.frame(ek=T_ex, ek_freq=T_ey, P_ek=T_ey/N)
plot(T_edf$ek, T_edf$ek_freq)
head(T_edf)
sum(T_edf$P_ek)

plot(PF_dff$N.species.extinction, PF_dff$N.speciations)

# Calculate P_ekdj
i <- 1
Tekdj_df <- data.frame()
for(i in 1:N) {
  a = df_SE_D[i,]
  tdf <- data.frame(ek=a$extinction, dj=a$diversity, ekdj_freq=1)
  rownames(tdf) <- paste(a$extinction, '_', a$diversity, sep='')
  
  if (rownames(tdf) %in% rownames(Tekdj_df)) {
    id <- which(rownames(Tekdj_df) == rownames(tdf))
    Tekdj_df[id,]$ekdj_freq <- Tekdj_df[id,]$ekdj_freq + 1
  } else {
    Tekdj_df <- rbind(Tekdj_df, tdf)
  }
}
head(Tekdj_df)
dim(Tekdj_df)
sum(Tekdj_df$ekdj_freq)
Tekdj_df$P_ekdj = Tekdj_df$ekdj_freq/N
sum(Tekdj_df$P_ekdj)

# Calculate P_ek_dj 
i <- 1
NN <- dim(Tekdj_df)[1]
c_P_ek_dj <- c()
c_P_ek <- c()
c_P_dj <- c()
for(i in 1:NN) {
  r <- Tekdj_df[i,]
  ek <- r$ek
  dj <- r$dj
  P_ekdj <- r$P_ekdj
  idj <- which(T_ddf$dj == dj)
  iek <- which(T_edf$ek == ek)
  
  P_dj <- T_ddf[idj,]$P_dj
  P_ek <- T_edf[iek,]$P_ek
  P_ek_dj <- P_ekdj / P_dj
  
  c_P_ek_dj <- c(c_P_ek_dj, P_ek_dj)
  c_P_ek <- c(c_P_ek, P_ek)
  c_P_dj <- c(c_P_dj, P_dj)
}

Tekdj_df$P_ek <- c_P_ek
Tekdj_df$P_dj <- c_P_dj
Tekdj_df$P_ek_dj <- c_P_ek_dj

library(DT)
rownames(Tekdj_df) <- seq(1,NN)
Tekdj_df <- round(Tekdj_df, 4)
datatable(Tekdj_df)

