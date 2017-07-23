#------Initial approach: pure Black-Scholes-------



#Resets global variables to default
Reset <- function()
{
   source('reset.R')
}

#Finite difference algorithm for vanilla european option
FD <- function(I      = I_g,      #number of desired asset steps
               strike = strike_g, 
               type   = type_g,
               sigma  = sigma_g,
               n_days = n_days_g, #time in days
               r      = r_g)
{
   dS    <- 3 * strike/I         #3*strike stands for the highest asset price considered
   dt    <- 0.9/sigma^2/I^2      #stability condition for time step
   T_    <- floor(n_days/dt) + 1 #number of time steps - adjusted to be an integer
   dt    <- n_days/T_            #time step fitted to grid 
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S     <- I:0 * dS
   mesh[,T_ + 1] <- Payoff(strike, S, type)
   for (t in (T_ + 1):2)
   {
      for (i in 2:I)
      {
         Delta <- (mesh[i - 1,t] - mesh[i + 1,t])/2/dS #rows indices numvers have opposite order, hence the switch 
         Gamma <- (mesh[i - 1,t] - 2 * mesh[i,t] + mesh[i + 1,t])/dS^2
         theta <- r * mesh[i,t] - 0.5 * sigma^2 * S[i]^2 * Gamma - r * S[i] * Delta #from Black-Scholes equation
         mesh[i,t - 1] <- mesh[i,t] - dt * theta #option value drops by theta when asset price is fixed at particular level
      }
      #mesh[I + 1,t - 1] <- mesh[I + 1,t] * (1 - r * dt)
      #mesh[1,    t - 1] <- 2 * mesh[2,t - 1] - mesh[3,t - 1]
      mesh[c(I + 1,1),t - 1] <- c(mesh[I + 1,t] * (1 - r * dt),2 * mesh[2,t - 1] - mesh[3,t - 1]) #forward differnce for the last S value, and boundary at 0
   }
   return(mesh)
}


#Finite difference algorithm for barrier european option
FD_Bar <- function(I      = I_g, 
                   strike = strike_g,
                   type   = type_g,
                   sigma  = sigma_g,
                   n_days = n_days_g,
                   r      = r_g,
                   barrier_spread = 200)
{
   dS    <- (ifelse(type == 'call', 1, 2) * strike + barrier_spread) / I #put S values range twice the call S values to meet the 3*strike requirement
   dt    <- 0.45 * (1 + (type == 'call'))/sigma^2/I^2 #adjusted by additional 1/2 due to lack of stability
   T_    <- floor(n_days/dt) + 1
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S <- (I - 1):0 * dS #[from 0 to barrier]
   if (type == 'put') 
      S <- S + strike - barrier_spread #[from barrier to infinity]
   mesh[(1:I) + (type == 'call'),T_ + 1] <- Payoff(strike, S, type) #2:(I+1) for call  
   for (t in (T_ + 1):2)
   {
      for (i in 2:I)
      {
         Delta <- (mesh[i - 1,t] - mesh[i + 1,t])/2/dS
         Gamma <- (mesh[i - 1,t] - 2 * mesh[i,t] + mesh[i + 1,t])/dS^2
         theta <- r * mesh[i,t] - 0.5 * sigma^2 * S[i]^2 * Gamma - r * S[i] * Delta
         mesh[i,t - 1] <- mesh[i,t] - dt * theta
      }
   }
   return(mesh)
}

#Finite difference algorithm for vanilla american option
FD_American <- function(I      = I_g, 
                        strike = strike_g,
                        type   = type_g,
                        sigma  = sigma_g,
                        n_days = n_days_g,
                        r      = r_g)
{
   dS    <- 3 * strike/I
   dt    <- 0.9/sigma^2/I^2
   T_    <- floor(n_days/dt) + 1
   r     <- r * dt
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S     <- I:0 * dS
   mesh[,T_ + 1] <- Payoff(strike, S, type) 
   for (t in (T_ + 1):2)
   {
      for (i in 2:I)
      {
         Delta <- (mesh[i - 1,t] - mesh[i + 1,t])/2/dS
         Gamma <- (mesh[i - 1,t] - 2 * mesh[i,t] + mesh[i + 1,t])/dS^2
         theta <- r * mesh[i,t] - 0.5 * sigma^2 * S[i]^2 * Gamma - r * S[i] * Delta
         mesh[i,t - 1] <- max(mesh[i,t] - dt * theta, Payoff(strike,S[i],type)) #early exercise condition
      }
      #mesh[I + 1,t - 1] <- mesh[I + 1,t] * (1 - r * dt)
      #mesh[1,    t - 1] <- 2 * mesh[2,t - 1] - mesh[3,t - 1]
      mesh[c(I + 1,1),t-1] <- c(mesh[I + 1,t] * (1 - r * dt),2 * mesh[2,t - 1] - mesh[3,t - 1])
   } 
   return(mesh)
}

#Finite difference algorithm for barrier american option
FD_Bar_American <- function(I      = I_g, 
                            strike = strike_g,
                            type   = type_g,
                            sigma  = sigma_g,
                            n_days = n_days_g,
                            r      = r_g,
                            barrier_spread = spread_g)
{
   dS    <- (ifelse(type == 'call', 1, 2) * strike + barrier_spread) / I
   dt    <- 0.45*(1 + (type == 'call'))/sigma^2/I^2
   T_    <- floor(n_days/dt) + 1
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S <- (I - 1):0 * dS
   if (type == 'put') 
      S <- S + strike - barrier_spread
   mesh[(1:I) + (type == 'call'),T_ + 1] <- Payoff(strike, S, type) 
   for (t in (T_ + 1):2)
   {
      for (i in 2:I)
      {
         Delta <- (mesh[i - 1,t] - mesh[i + 1,t])/2/dS
         Gamma <- (mesh[i - 1,t] - 2 * mesh[i,t] + mesh[i + 1,t])/dS^2
         theta <- r * mesh[i,t] - 0.5 * sigma^2 * S[i]^2 * Gamma - r * S[i] * Delta
         mesh[i,t - 1] <- max(mesh[i,t] - dt * theta, Payoff(strike,S[i],type))
      }
   }
   return(mesh)
}
