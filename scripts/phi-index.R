# phi-index

A_m2 <- 24384

df <- read.csv('data/p_r.csv', sep = ';')
summary(df)

DHR <- sum(c(0, (diff.Date(df$Time_min)*60))*(df$Q_m3s)/A_m2*1000)
Rain <- sum(c(0, (diff.Date(df$Time_min)/60))*(df$R_mm_h))
Dur <- sum(df$R_mm_h > 0)*(2)/60
I <- Rain - DHR
phi_index <- I/Dur

Dur2 <- sum(subset(df, R_mm_h > phi_index & R_mm_h != 0, select = R_mm_h) > 0)*(2)/60
R_phi1 <- sum(subset(df, R_mm_h < phi_index & R_mm_h != 0, select = R_mm_h))*2/60
phi_index2 <- (Rain - DHR - R_phi1)/Dur2


Dur3 <- sum(subset(df, R_mm_h > phi_index2 & R_mm_h != 0, select = R_mm_h) > 0)*(2)/60
R_phi2 <- sum(subset(df, R_mm_h < phi_index2 & R_mm_h != 0, select = R_mm_h))*2/60
phi_index3 <- (Rain - DHR - R_phi2)/Dur3


Dur4 <- sum(subset(df, R_mm_h > phi_index3 & R_mm_h != 0, select = R_mm_h) > 0)*(2)/60
R_phi3 <- sum(subset(df, R_mm_h < phi_index3 & R_mm_h != 0, select = R_mm_h))*2/60
phi_index4 <- (Rain - DHR - R_phi3)/Dur4


Dur5 <- sum(subset(df, R_mm_h > phi_index4 & R_mm_h != 0, select = R_mm_h) > 0)*(2)/60
R_phi4 <- sum(subset(df, R_mm_h < phi_index4 & R_mm_h != 0, select = R_mm_h))*2/60
phi_index5 <- (Rain - DHR - R_phi4)/Dur5


R_effective <- sum(subset(df, R_mm_h > phi_index5, select = R_mm_h) - phi_index5)*2/60
time_effective <- sum(subset(df, R_mm_h > phi_index5, select = R_mm_h) - phi_index5 > 0)*2/60


list('phi-index' = phi_index5,
     'Rain effective' = R_effective,
     'time effective' = time_effective)
