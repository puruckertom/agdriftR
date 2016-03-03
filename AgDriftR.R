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
for (n in 1:length(df_python)) {
  assign(names(df_python)[n], df_python[[n]])
}

#df_python$distance_m <- df_python$distance * 0.3048 #ft to meters


# AGDRIFT DATA (worksheet calculator)
df_aerial_excel <- read.xlsx(file = paste(cdf.path, "ubertool spray drift calculator_QC.xlsm", sep = ""), sheetName = "aerial depostion", startRow = 2, header = TRUE)
df_ground_excel <- read.xlsx(file = paste(cdf.path, "ubertool spray drift calculator_QC.xlsm", sep = ""), sheetName = "ground deposition", startRow = 2, header = TRUE)
df_airblast_excel <- read.xlsx(file = paste(cdf.path, "ubertool spray drift calculator_QC.xlsm", sep = ""), sheetName = "airblast deposition", startRow = 2, header = TRUE)

# convert distance(ft) to distance(m)
df_aerial_excel$distance_m <- round(df_aerial_excel$Distance..ft. / 3.28084)
df_ground_excel$distance_m <- round(df_ground_excel$Distance..ft. / 3.28084)
df_airblast_excel$distance_m <- round(df_airblast_excel$Distance..ft. / 3.28084)

# AGDRIFT CALCULATIONS ####
## TIER I AERIAL

## TIER I GROUND
# Low Boom -- D(x) = c/(1+ax)^b 
# x = downwind distance (ft)
# x[1,1300] ft
df_ground_calc <- data.frame(distance_ft = numeric(length = 1301),
                             ground_low_vf2f_90 = numeric(1301),
                             ground_low_f2mc_90 = numeric(1301),
                             ground_low_vf2f_50 = numeric(1301),
                             ground_low_f2mc_50 = numeric(1301),
                             ground_high_vf2f_90 = numeric(1301),
                             ground_high_f2mc_90 = numeric(1301),
                             ground_high_vf2f_50 = numeric(1301),
                             ground_high_f2mc_50 = numeric(1301), 
                             row.names=NULL, stringsAsFactors=F)
for (x in 0:1300) {
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
df_airblast_calc <- data.frame(distance_ft = numeric(length = 601),
                             airblast_normal_outside = numeric(length = 601),
                             airblast_normal_inside = numeric(length = 601),
                             airblast_dense_outside = numeric(length = 601),
                             airblast_dense_inside = numeric(length = 601),
                             airblast_sparse_outside = numeric(length = 601),
                             airblast_sparse_inside = numeric(length = 601),
                             airblast_vineyard_outside = numeric(length = 601),
                             airblast_vineyard_inside = numeric(length = 601),
                             airblast_orchard_outside = numeric(length = 601),
                             airblast_orchard_inside = numeric(length = 601),
                             row.names=NULL, stringsAsFactors=F)
for (x in 0:600) {
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
rownum <- c(1,9, 11:158)
df_python_plot <- df_python[seq(1,300, by = 2),]
df_aerial_excel_plot <- df_aerial_excel[1:150,]
df_ground_excel_plot <- df_ground_excel[rownum,]
df_airblast_excel_plot <- df_airblast_excel[rownum,]

ggplot(df_python, aes(x=df_python$distance_ft)) +
  geom_line(aes(y=df_python$pond_airblast_orchard, colour = "var0")) +
  #geom_line(aes(y=df_airblast_calc$airblast_orchard_outside[1:300], colour = "var1")) +
  geom_line(aes(y=df_airblast_calc$airblast_orchard_inside[1:300], colour = "var2"))