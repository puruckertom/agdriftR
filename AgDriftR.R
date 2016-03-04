library(ggplot2)
library(xlsx)

#carmen
if(Sys.info()[4]=="DZ2626UCKUAN"){
  cdf.path <- "C://Users//ckuan//git//agdriftR//"
}
#tom
if(Sys.info()[4]=="stp-air-3.local" || Sys.info()[4]=="stp-air.local" || Sys.info()[4]=="stp-air"){
  cdf.path <- "~/git/agdriftR/"
}


# AGDRIFT DATA (webversion) ####
df_python <- read.csv(paste(cdf.path, "agdrift_database.csv", sep = ""), header = TRUE)

# AGDRIFT DATA (worksheet calculator)
df_aerial_excel <- read.xlsx(file = paste(cdf.path, "ubertool spray drift calculator_QC.xlsm", sep = ""), sheetName = "aerial depostion", startRow = 2, header = TRUE)
df_ground_excel <- read.xlsx(file = paste(cdf.path, "ubertool spray drift calculator_QC.xlsm", sep = ""), sheetName = "ground deposition", startRow = 2, header = TRUE)
df_airblast_excel <- read.xlsx(file = paste(cdf.path, "ubertool spray drift calculator_QC.xlsm", sep = ""), sheetName = "airblast deposition", startRow = 2, header = TRUE)

# AGDRIFT CALCULATIONS ####
## TIER I AERIAL

## TIER I GROUND
# Low Boom -- D(x) = c/(1+ax)^b 
# x = downwind distance (ft)
# x[1,1300] ft
df_ground_calc <- data.frame(distance_ft = numeric(length = 1001),
                             ground_low_vf2f_90 = numeric(1001),
                             ground_low_f2mc_90 = numeric(1001),
                             ground_low_vf2f_50 = numeric(1001),
                             ground_low_f2mc_50 = numeric(1001),
                             ground_high_vf2f_90 = numeric(1001),
                             ground_high_f2mc_90 = numeric(1001),
                             ground_high_vf2f_50 = numeric(1001),
                             ground_high_f2mc_50 = numeric(1001), 
                             row.names=NULL, stringsAsFactors=F)
for (x in 0:1000) {
  # distance_ft
  df_ground_calc$distance_ft[x+1] <- x
  # vf2f_90
  if (x < 25) {df_ground_calc$ground_low_vf2f_90[x+1] <- 1.0/(1.0 + 0.4081*x)^1.5866} else {df_ground_calc$ground_low_vf2f_90[x+1] <- 1.3322/(1.0 + 0.5262*x)^1.5548}
  # f2mc_90
  if (x < 25) {df_ground_calc$ground_low_f2mc_90[x+1] <- 1.0/(1.0 + 2.0913*x)^1.2572} else {df_ground_calc$ground_low_f2mc_90[x+1] <- 0.1419/(1.0 + 0.3187*x)^1.3885}
  # vf2f_50
  if (x < 25) {df_ground_calc$ground_low_vf2f_50[x+1] <- 1.0/(1.0 + 0.8082*x)^1.5208} else {df_ground_calc$ground_low_vf2f_50[x+1] <- 1.2628/(1.0 + 0.9749*x)^1.5085}
  #f2mc_50
  if (x < 25) {df_ground_calc$ground_low_f2mc_50[x+1] <- 1.0/(1.0 + 1.3742*x)^1.4863} else {df_ground_calc$ground_low_f2mc_50[x+1] <- 1.2205/(1.0 + 1.6014*x)^1.4803}
  
  # High Boom -- D(x) = (c/(1+ax)^b) * (1+A exp(-Bx)) #exponential expression only for x > 25 ft
  #vf2f_90
  if (x < 25) {df_ground_calc$ground_high_vf2f_90[x+1] <- 1.0/(1.0 + 0.1243*x)^1.9100} else {df_ground_calc$ground_high_vf2f_90[x+1] <- (1.3322/(1.0 + 0.5262*x)^1.5548) * (1.0 + 2.1749*exp(-0.001185*x))}
  #f2mc_90
  if (x < 25) {df_ground_calc$ground_high_f2mc_90[x+1] <- 1.0/(1.0 + 1.3058*x)^1.2714} else {df_ground_calc$ground_high_f2mc_90[x+1] <- (0.1419/(1.0 + 0.3187*x)^1.3885) * (1.0 + 0.7089*exp(-0.000754*x))}
  #vf2f_50
  if (x < 25) {df_ground_calc$ground_high_vf2f_50[x+1] <- 1.0/(1.0 + 0.1409*x)^1.8605} else {df_ground_calc$ground_high_vf2f_50[x+1] <- (1.2628/(1.0 + 0.9749*x)^1.5085) * (1.0 + 5.4877*exp(-0.001541*x))}
  #f2mc_50
  if (x < 25) {df_ground_calc$ground_high_f2mc_50[x+1] <- 1.0/(1.0 + 0.7802*x)^1.5183} else {df_ground_calc$ground_high_f2mc_50[x+1] <- (1.2205/(1.0 + 1.6014*x)^1.4803) * (1.0 + 1.0639*exp(-0.000910*x))}
}

## TIER I ORCHARD/AIRBLAST -- D(x) = c/(1+ax)^b # x[1,600] ft
df_airblast_calc <- data.frame(distance_ft = numeric(length = 1001),
                             airblast_normal_outside = numeric(length = 1001),
                             airblast_normal_inside = numeric(length = 1001),
                             airblast_dense_outside = numeric(length = 1001),
                             airblast_dense_inside = numeric(length = 1001),
                             airblast_sparse_outside = numeric(length = 1001),
                             airblast_sparse_inside = numeric(length = 1001),
                             airblast_vineyard_outside = numeric(length = 1001),
                             airblast_vineyard_inside = numeric(length = 1001),
                             airblast_orchard_outside = numeric(length = 1001),
                             airblast_orchard_inside = numeric(length = 1001),
                             row.names=NULL, stringsAsFactors=F)
for (x in 0:1000) {
  # distance_ft
  df_airblast_calc$distance_ft[x+1] <- x
  df_airblast_calc$airblast_normal_outside[x+1] <- 0.9737/(1.0 + 1.0291*x)^1.9286
  df_airblast_calc$airblast_normal_inside[x+1] <- 0.1320/(1.0 + 1.3961*x)^1.4996
  df_airblast_calc$airblast_dense_outside[x+1] <- 2.9451/(1.0 + 0.1300*x)^2.5037
  df_airblast_calc$airblast_dense_inside[x+1] <- 0.2190/(1.0 + 1.6150*x)^1.1886
  df_airblast_calc$airblast_sparse_outside[x+1] <- 5.1769/(1.0 + 0.0629*x)^3.3201
  df_airblast_calc$airblast_sparse_inside[x+1] <- 5.2652/(1.0 + 0.1582*x)^2.7868
  df_airblast_calc$airblast_vineyard_outside[x+1] <- 2.4409/(1.0 + 0.3581*x)^2.7104
  df_airblast_calc$airblast_vineyard_inside[x+1] <- 0.7219/(1.0 + 1.5541*x)^1.7194
  df_airblast_calc$airblast_orchard_outside[x+1] <- 5.8017/(1.0 + 0.1183*x)^2.7872
  df_airblast_calc$airblast_orchard_inside[x+1] <- 0.3620/(1.0 + 1.0699*x)^1.3649
}


# AGDRIFT PLOTTING ####
# Aerial
## Very Fine to Fine
par(mfrow=c(4,1))
plot(df_python$distance_ft, df_python$pond_aerial_vf2f, main = "vf2f", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,600), ylim=c(0, 0.55))
lines(df_aerial_excel$Distance..ft., df_aerial_excel$Very.Fine.to.Fine, lty=2)
legend("topleft", c("Python", "Peck"), col = c("black", "black"), lty = c(1, 2), bg = "gray90", cex=0.75)
## Fine to medium
plot(df_python$distance_ft, df_python$pond_aerial_f2m, col = "red", main = "f2m", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,600), ylim=c(0, 0.55))
lines(df_aerial_excel$Distance..ft., df_aerial_excel$Fine.to.Med, col = "red", lty=2)
legend("topleft", c("Python", "Peck"), col = c("red", "red"), lty = c(1, 2), bg = "gray90", cex=0.75)
## Medium to coarse
plot(df_python$distance_ft, df_python$pond_aerial_m2c, col = "green3", type = "l", main = "m2c", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,600), ylim=c(0, 0.55))
lines(df_aerial_excel$Distance..ft., df_aerial_excel$Med.to.Coarse, col = "green3", lty=2)
legend("topleft", c("Python", "Peck"), col = c("green3", "green3"), lty = c(1, 2), bg = "gray90", cex=0.75)
## Coarse to very coarse
plot(df_python$distance_ft, df_python$pond_aerial_c2vc, col = "blue", type = "l", main = "c2vc", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,600), ylim=c(0, 0.55))
lines(df_aerial_excel$Distance..ft., df_aerial_excel$Coarse.to.VC, col = "blue", lty=2)
legend("topleft", c("Python", "Peck"), col = c("blue", "blue"), lty = c(1, 2), bg = "gray90", cex=0.75)

# Ground
## Low- very fine to fine
par(mfrow=c(1,5))
plot(df_ground_calc$distance_ft, df_ground_calc$ground_low_vf2f_90, main = "Low vf2f", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,300))
lines(df_ground_excel$Distance..ft., df_ground_excel$Low.boom..vf, lty=2)
lines(df_python$distance_ft, df_python$pond_ground_low_vf2f, lty=4)
legend("topleft", c("Calc", "Peck", "Python"), col = c("black", "black", "black"), lty = c(1, 2, 4), bg = "gray90", cex = 0.75)
## Low- fine to medium coarse
plot(df_ground_calc$distance_ft, df_ground_calc$ground_low_f2mc_90, col = "red", main = "Low f2mc", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,300))
lines(df_ground_excel$Distance..ft., df_ground_excel$low.boom..fmc, col = "red", lty=2)
lines(df_python$distance_ft, df_python$pond_ground_low_f2m, col = "red", lty=4)
legend("topleft", c("Calc", "Peck", "Python"), col = c("red", "red", "red"), lty = c(1, 2, 4), bg = "gray90", cex = 0.75)
## High- very fine to fine
plot(df_ground_calc$distance_ft, df_ground_calc$ground_high_vf2f_90, col = "green3", main = "High Vf2f", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,300))
lines(df_ground_excel$Distance..ft., df_ground_excel$High.boom..vf, col = "green3", lty=2)
lines(df_python$distance_ft, df_python$pond_ground_high_vf2f, col = "green3", lty=4)
legend("topleft", c("Calc", "Peck", "Python"), col = c("green3", "green3", "green3"), lty = c(1, 2, 4), bg = "gray90", cex = 0.75)
## High- fine to medium coarse
plot(df_ground_calc$distance_ft, df_ground_calc$ground_high_f2mc_90, col = "blue", main = "High f2mc", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition",xlim=c(0,300))
lines(df_ground_excel$Distance..ft., df_ground_excel$high.boom..fmc, col = "blue", lty=2)
lines(df_python$distance_ft, df_python$pond_ground_high_f2m, col = "blue", lty=4)
legend("topleft", c("Calc", "Peck", "Python"), col = c("blue", "blue", "blue"), lty = c(1, 2, 4), bg = "gray90", cex = 0.75)

# Airblast
## orchard
par(mfrow=c(1,5))
plot(df_airblast_calc$distance_ft, df_airblast_calc$airblast_orchard_inside, main = "Orchard", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition", xlim=c(0,600))
lines(df_airblast_calc$distance_ft, df_airblast_calc$airblast_orchard_outside, col="blue", lty=2)
lines(df_python$distance_ft, df_python$pond_airblast_orchard, col = "red", lty=2)
lines(df_airblast_excel$Distance..ft., df_airblast_excel$Orchard, col = "green3", lty=2)
legend("topleft", c("Calc- inside", "Calc- outside", "Python", "Peck"), col = c("black", "blue", "red", "green3"), lty = c(1, 2, 2, 2), bg = "gray90", cex=0.75)
## Vineyard
plot(df_airblast_calc$distance_ft, df_airblast_calc$airblast_vineyard_inside, main = "Vineyard", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition", xlim=c(0,600))
lines(df_airblast_calc$distance_ft, df_airblast_calc$airblast_vineyard_outside, col="blue", lty=2)
lines(df_python$distance_ft, df_python$pond_airblast_vineyard, col = "red", lty=2)
lines(df_airblast_excel$Distance..ft., df_airblast_excel$Vineyard, col = "green3", lty=2)
legend("topleft", c("Calc- inside", "Calc- outside", "Python", "Peck"), col = c("black", "blue", "red", "green3"), lty = c(1, 2, 2, 2), bg = "gray90", cex=0.75)
## Normal
plot(df_airblast_calc$distance_ft, df_airblast_calc$airblast_normal_inside, main = "Normal", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition", xlim=c(0,600))
lines(df_airblast_calc$distance_ft, df_airblast_calc$airblast_normal_outside, col="blue", lty=2)
lines(df_airblast_excel$Distance..ft., df_airblast_excel$Normal, col = "green3", lty=2)
legend("topleft", c("Calc- inside", "Calc- outside", "Peck"), col = c("black", "blue", "green3"), lty = c(1, 2, 2), bg = "gray90", cex = 0.75)
## Sparse
plot(df_airblast_calc$distance_ft, df_airblast_calc$airblast_sparse_inside, main = "Sparse", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition", xlim=c(0,600))
lines(df_airblast_calc$distance_ft, df_airblast_calc$airblast_sparse_outside, col="blue", lty=2)
lines(df_airblast_excel$Distance..ft., df_airblast_excel$Sparse, col = "green3", lty=2)
legend("topleft", c("Calc- inside", "Calc- outside", "Peck"), col = c("black", "blue", "green3"), lty = c(1, 2, 2), bg = "gray90", cex = 0.75)
## Dense
plot(df_airblast_calc$distance_ft, df_airblast_calc$airblast_dense_inside, main = "Dense", type = "l", xlab = "Distance (ft)", ylab = "Fraction of Deposition", xlim=c(0,600))
lines(df_airblast_calc$distance_ft, df_airblast_calc$airblast_dense_outside, col="blue", lty=2)
lines(df_airblast_excel$Distance..ft., df_airblast_excel$Dense, col = "green3", lty=2)
legend("topleft", c("Calc- inside", "Calc- outside", "Peck"), col = c("black", "blue", "green3"), lty = c(1, 2, 2), bg = "gray90", cex = 0.75)