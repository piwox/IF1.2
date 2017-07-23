source('globals.R')

type_g   = 'call'          #choose 'call' or 'put' 
strike_g = 2400         #project requirement
sigma    = sigma_g   
sigma_max_g <- 0.3/sqrt(252) #project requirement
sigma_min_g <- 0.2/sqrt(252) #project requirement
n_days_g = n_days_0 
I_g      = 300 
r_g      = log(1+0.02)/252
spread_g = 200
div_g    = 200
