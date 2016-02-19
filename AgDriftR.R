library(ggplot2)

# AGDRIFT CALCULATIONS ####
## TIER I AERIAL

## TIER I GROUND
# Low Boom -- D(x) = c/(1+ax)^b 
# x = downwind distance (ft)
# x[1,1300] ft
for (x in 1:1300) {
  # vf2f_90
  ground_low_vf2f_90 <- vector(length = 1300, "numeric")
  if (x < 25) {
    ground_low_vf2f_90[x] <- 1.0/(1.0 + 0.4081*x)^1.5866
  if (x > 24)
    ground_low_vf2f_90[x] <- 1.3322/(1.0 + 0.5262*x)^1.5548
  }
  # f2mc_90
  while (x < 25) {
    ground_low_f2mc_90 <- 1.0/(1.0 + 2.0913*x)^1.2572
    else
      ground_low_f2mc_90 <- 0.1419/(1.0 + 0.3187*x)^1.3885
  }
  # vf2f_50
  while (x < 25) {
    ground_low_vf2f_50 <- 1.0/(1.0 + 0.8082*x)^1.5208
    else
      ground_low_vf2f_50 <- 1.2628/(1.0 + 0.9749*x)^1.5085
  }
  #f2mc_50
  while (x < 25) {
    ground_low_f2mc_50 <- 1.0/(1.0 + 1.3742*x)^1.4863
    else
      ground_low_f2mc_50 <- 1.2205/(1.0 + 1.6014*x)^1.4803
  }
  
  # High Boom -- D(x) = (c/(1+ax)^b) * (1+A exp(-Bx)) #exponential expression only for x > 25 ft
  #vf2f_90
  while (x < 25) {
    ground_high_vf2f_90 <- 1.0/(1.0 + 0.1243*x)^1.9100
    else
      ground_high_vf2f_90 <- (1.3322/(1.0 + 0.5262*x)^1.5548) * (1.0 + 2.1749*exp(-0.001185*x))
  }
  #f2mc_90
  while (x < 25) {
    ground_high_f2mc_90 <- 1.0/(1.0 + 1.3058*x)^1.2714
    else
      ground_high_f2mc_90 <- (0.1419/(1.0 + 0.3187*x)^1.3885) * (1.0 + 0.7089*exp(-0.000754*x))
  }
  #vf2f_50
  while (x < 25) {
    ground_high_vf2f_50 <- 1.0/(1.0 + 0.1409*x)^1.8605
    else
      ground_high_vf2f_50 <- (1.2628/(1.0 + 0.9749*x)^1.5085) * (1.0 + 5.4877*exp(-0.001541*x))
  }
  #f2mc_50
  while (x < 25) {
    ground_high_f2mc_50 <- 1.0/(1.0 + 0.7802*x)^1.5183
    else
      ground_high_f2mc_50 <- (1.2205/(1.0 + 1.6014*x)^1.4803) * (1.0 + 1.0639*exp(-0.000910*x))
  }
}

## TIER I ORCHARD/AIRBLAST -- D(x) = c/(1+ax)^b # x[1,600] ft
airblast_normal_outside <- 0.9737/(1.0 + 1.0291*x)^1.9286
airblast_normal_inside <- 0.1320/(1.0 + 1.3961*x)^1.4996
airblast_dense_outside <- 2.9451/(1.0 + 0.1300*x)^2.5037
airblast_dense_inside <- 0.2190/(1.0 + 1.6150*x)^1.1886
airblast_sparse_outside <- 5.1769/(1.0 + 0.0629*x)^3.3201
airblast_sparse_inside <- 5.2652/(1.0 + 0.1582*x)^2.7868
airblast_vineyard_outside <- 2.4409/(1.0 + 0.3581*x)^2.7104
airblast_vineyard_inside <- 0.7219/(1.0 + 1.5541*x)^1.7194
airblast_orchard_outside <- 5.8017/(1.0 + 0.1183*x)^2.7872
airblast_orchard_inside <- 0.3620/(1.0 + 1.0699*x)^1.3649


# AGDRIFT DATA (webversion) ####
database <- read.csv(paste("C://Users//ckuan//My Documents//AgDrift//agdrift_database.csv"), header = TRUE)
for (n in 1:length(database)) {assign(names(database)[n], database[[n]])}

# AGDRIFT PLOTTING ####
ggplot(database, aes(x = database$distance, y = database$pond_ground_high_f2m)) +
  geom_line()

