#-------------External global variables--------------

#Estimates based on historical data

wig20_quotes      <- read.csv('wig20_quotes.csv')[,'Zamkniecie']
n_days_0          <- length(wig20_quotes)
wig20_returns     <- wig20_quotes[2:n_days_0]/wig20_quotes[1:(n_days_0 - 1)]
wig20_logreturns  <- log(wig20_returns)
sigma_g           <- sd(wig20_logreturns)
mu_g              <- mean(wig20_logreturns)
vol_g             <- sigma_g * sqrt(252)
drift_g           <- mu_g * 252
