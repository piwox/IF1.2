#---------Incorporating uncertainity of volatility parameter--------


#Finite difference algorithm for vanilla european option (uncertain volatility)
FD_UV <- function(I         = I_g, 
                  strike    = strike_g,
                  type      = type_g,
                  sigma_min = sigma_min_g,
                  sigma_max = sigma_max_g, 
                  n_days    = n_days_g,
                  r         = r_g)
{
   dS    <- 3 * strike/I
   dt    <- 0.5*0.9/sigma^2/I^2  #another fix to hold stability
   T_    <- floor(n_days/dt) + 1
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S     <- I:0 * dS
   mesh[,T_ + 1] <- Payoff(strike, S, type) 
   for(t in (T_ + 1):1)
   {
      for(i in 2:I)
      {
         Delta <- (mesh[i - 1,t] - mesh[i + 1,t])/2/dS
         Gamma <- (mesh[i - 1,t] - 2 * mesh[i,t] + mesh[i + 1,t])/dS^2
         theta <- r * mesh[i,t] - 0.5 * ifelse(Gamma<0,sigma_min,sigma_max)^2 * S[i]^2 * Gamma - r * S[i] * Delta #uncertain volatility
         mesh[i,t - 1] <- mesh[i,t] - dt * theta
      }
      #mesh[I + 1,t - 1] <- mesh[I + 1,t] * (1 - r * dt)
      #mesh[1,    t - 1] <- 2 * mesh[2,t - 1] - mesh[3,t - 1]
      mesh[c(I+1,1),t-1] <- c(mesh[I + 1,t] * (1 - r * dt),2 * mesh[2,t - 1] - mesh[3,t - 1])
   }
   return(mesh)
}

#Finite difference algorithm for barrier european option (uncertain volatility)
FD_Bar_UV <- function(I         = I_g, 
                                 strike    = strike_g,
                                 type      = type_g,
                                 sigma_min = sigma_min_g,
                                 sigma_max = sigma_max_g,
                                 n_days    = n_days_g,
                                 r         = r_g,
                                 barrier_spread = 200)
{
   dS    <- (ifelse(type=='call', 1, 2) * strike + barrier_spread) / I
   dt    <- 0.5*0.45*(1+(type=='call'))/sigma^2/I^2
   T_    <- floor(n_days/dt) + 1
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S <- (I-1):0 * dS
   if(type=='put') 
      S <- S + strike - barrier_spread
   mesh[(1:I) + (type=='call'),T_ + 1] <- Payoff(strike, S, type) 
   for(t in (T_ + 1):1)
   {
      for(i in 2:I)
      {
         Delta <- (mesh[i - 1,t] - mesh[i + 1,t])/2/dS
         Gamma <- (mesh[i - 1,t] - 2 * mesh [i,t] + mesh[i + 1,t])/dS^2
         theta <- r * mesh[i,t] - 0.5 * ifelse(Gamma<0,sigma_min,sigma_max)^2 * S[i]^2 * Gamma - r * S[i] * Delta
         mesh[i,t - 1] <- mesh[i,t] - dt * theta
      }
   }
   return(mesh)
}

#Finite difference algorithm for vanilla american option (uncertain volatility)
FD_American_UV <- function(I         = I_g, 
                           strike    = strike_g,
                           type      = type_g,
                           sigma_min = sigma_min_g,
                           sigma_max = sigma_max_g,
                           n_days    = n_days_g,
                           r         = r_g)
{
   dS    <- 3 * strike/I
   dt    <- 0.5*0.9/sigma^2/I^2
   T_    <- floor(n_days/dt) + 1
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S     <- I:0 * dS
   mesh[,T_ + 1] <- Payoff(strike, S, type) 
   for(t in (T_ + 1):1)
   {
      for(i in 2:I)
      {
         Delta <- (mesh[i - 1,t] - mesh[i + 1,t])/2/dS
         Gamma <- (mesh[i - 1,t] - 2 * mesh [i,t] + mesh[i + 1,t])/dS^2
         theta <- r * mesh[i,t] - 0.5 * ifelse(Gamma<0,sigma_min,sigma_max)^2 * S[i]^2 * Gamma - r * S[i] * Delta
         mesh[i,t - 1] <- max(mesh[i,t] - dt * theta, mesh[i,T_ + 1])
      }
      #mesh[I + 1,t - 1] <- mesh[I + 1,t] * (1 - r * dt)
      #mesh[1,    t - 1] <- 2 * mesh[2,t - 1] - mesh[3,t - 1]
      mesh[c(I+1,1),t-1] <- c(mesh[I + 1,t] * (1 - r * dt),2 * mesh[2,t - 1] - mesh[3,t - 1])
   }
   return(mesh)
}

#Finite difference algorithm for barrier american option (uncertain volatility)
FD_Bar_American_UV <- function(I         = I_g, 
                               strike    = strike_g,
                               type      = type_g,
                               sigma_min = sigma_min_g,
                               sigma_max = sigma_max_g,
                               n_days    = n_days_g,
                               r         = r_g,
                               barrier_spread = 200)
{
   dS    <- (ifelse(type=='call', 1, 2) * strike + barrier_spread) / I
   dt    <- 0.5*0.45*(1+(type=='call'))/sigma^2/I^2
   T_    <- floor(n_days/dt) + 1
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S <- (I-1):0 * dS
   if(type=='put') 
      S <- S + strike - barrier_spread
   mesh[(1:I) + (type=='call'),T_ + 1] <- Payoff(strike, S, type) 
   for(t in (T_ + 1):1)
   {
      for(i in 2:I)
      {
         Delta <- (mesh[i - 1,t] - mesh[i + 1,t])/2/dS
         Gamma <- (mesh[i - 1,t] - 2 * mesh[i,t] + mesh[i + 1,t])/dS^2
         theta <- r * mesh[i,t] - 0.5 * ifelse(Gamma<0,sigma_min,sigma_max)^2 * S[i]^2 * Gamma - r * S[i] * Delta
         mesh[i,t - 1] <- max(mesh[i,t] - dt * theta, mesh[i,T_ + 1])
      }
   }
   return(mesh)
}
