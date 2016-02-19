# AGDRIFT CALCULATIONS

## Tier I Ground

## Low Boom -- D(x) = c/(1+ax)^b # x[1,1300] ft
# vf2f_90
if x < 25 {
  # x = downwind distance (ft)
  ground_low_vf2f_90 <- 1.0/(1.0 + 0.4081*x)^1.5866
  else
    ground_low_vf2f_90 <- 1.3322/(1.0 + 0.5262*x)^1.5548
}
# f2mc_90
if x < 25 {
  ground_low_f2mc_90 <- 1.0/(1.0 + 2.0913*x)^1.2572
  else
    ground_low_f2mc_90 <- 0.1419/(1.0 + 0.3187*x)^1.3885
}
# vf2f_50
if x < 25 {
  ground_low_vf2f_50 <- 1.0/(1.0 + 0.8082*x)^1.5208
  else
    ground_low_vf2f_50 <- 1.2628/(1.0 + 0.9749*x)^1.5085
}
#f2mc_50
if x < 25 {
  ground_low_f2mc_50 <- 1.0/(1.0 + 1.3742*x)^1.4863
  else
    ground_low_f2mc_50 <- 1.2205/(1.0 + 1.6014*x)^1.4803
}

## High Boom -- D(x) = (c/(1+ax)^b) * (1+A exp(-Bx)) #exponential expression only for x > 25 ft
#vf2f_90
if x < 25 {
  ground_high_vf2f_90 <- 1.0/(1.0 + 0.1243*x)^1.9100
  else
    ground_high_vf2f_90 <- (1.3322/(1.0 + 0.5262*x)^1.5548) * (1.0 + 2.1749*exp(-0.001185*x))
}
#f2mc_90
if x < 25 {
  ground_high_f2mc_90 <- 1.0/(1.0 + 1.3058*x)^1.2714
  else
    ground_high_f2mc_90 <- (0.1419/(1.0 + 0.3187*x)^1.3885) * (1.0 + 0.7089*exp(-0.000754*x))
}
#vf2f_50
if x < 25 {
  ground_high_vf2f_50 <- 1.0/(1.0 + 0.1409*x)^1.8605
  else
    ground_high_vf2f_50 <- (1.2628/(1.0 + 0.9749*x)^1.5085) * (1.0 + 5.4877*exp(-0.001541*x))
}
#f2mc_50
if x < 25 {
  ground_high_f2mc_50 <- 1.0/(1.0 + 0.7802*x)^1.5183
  else
    ground_high_f2mc_50 <- (1.2205/(1.0 + 1.6014*x)^1.4803) * (1.0 + 1.0639*exp(-0.000910*x))
}

## Tier I Orchard/Airblast

## D(x) = c/(1+ax)^b # x[1,600] ft

