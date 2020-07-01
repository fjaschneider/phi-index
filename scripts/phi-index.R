################################################################################
#                               phi-index                                      #
#                     Por Fabio José Andres Schneider                          #
#         Participação José Américo Nabuco Leva Ferreira de Freitas            #
#                                                                              #
################################################################################         

A_m2 <- 24384

df <- read.csv('data/p_r.csv', sep = ';')
summary(df)
delta_t <- max(diff.Date(df$Time_min))

DHR <- sum(c(0, (diff.Date(df$Time_min)*60))*(df$Q_m3s)/A_m2*1000)
Rain <- sum(c(0, (diff.Date(df$Time_min)/60))*(df$R_mm_h))
Dur <- sum(df$R_mm_h > 0)*(delta_t)/60
I <- Rain - DHR
phi_index <- I/Dur

phi_index_old = phi_index

for(i in seq(10)){
  
  Dur <- sum(subset(df, R_mm_h > phi_index_old & R_mm_h != 0, select = R_mm_h) > 0)*(delta_t)/60
  R_phi <- sum(subset(df, R_mm_h < phi_index_old & R_mm_h != 0, select = R_mm_h))*delta_t/60
  phi_index_new <- (Rain - DHR - R_phi)/Dur
  phi_index_old <- phi_index_new
  
}

phi <- phi_index_new


R_effective <- sum(subset(df, R_mm_h > phi, select = R_mm_h) - phi)*delta_t/60
time_effective <- sum(subset(df, R_mm_h > phi, select = R_mm_h) - phi > 0)*delta_t/60


list('phi-index (mm/h)' = phi,
     'Rain effective (mm)' = R_effective,
     'time effective (h)' = time_effective)

df$R_excess <- ifelse(df$R_mm_h < phi, 0, df$R_mm_h - phi)

library(ggplot2)
library(egg)

ggplot(data = df) +
  geom_col(aes(Time_min, R_mm_h), fill = 1) +
  geom_col(aes(Time_min, R_excess), fill = 2) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,150)) +
  scale_fill_manual(labels = c('Rain', 'Rain excess')) +
  theme_article()
