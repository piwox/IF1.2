#----------Functions using C++ in bottlenecks----------

#Finite difference algorithm for vanilla european option
cpp_FD <- function(I      = I_g,      #number of desired asset steps
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
   mesh  <- cpp_zeros_matrix(I + 1, T_ + 1)
   S     <- I:0 * dS
   mesh[,T_ + 1] <- Payoff(strike, S, type)
   return(cpp_FD_Loop(mesh, I, T_, dS, dt, sigma, r, S))
}

#Finite difference algorithm for barrier european option
cpp_FD_Bar <- function(I      = I_g, 
                   strike = strike_g,
                   type   = type_g,
                   sigma  = sigma_g,
                   n_days = n_days_g,
                   r      = r_g,
                   barrier_spread = 200)
{
   dS    <- (ifelse(type == 'call', 1, 2) * strike + barrier_spread) / I #put S values range twice the call S values to meet the 3*strike requirement
   dt    <- 0.45 * (1 + (type == 'call'))/sigma^2/I^2
   T_    <- floor(n_days/dt) + 1
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S <- ((I - 1):0 + (type == 'put')) * dS #[from 0 to barrier]
   if (type == 'put') 
      S <- S + strike - barrier_spread #[from barrier to infinity]
   mesh[(1:I) + (type == 'call'),T_ + 1] <- Payoff(strike, S, type) #2:(I+1) for call  
   return(cpp_FD_Loop_Bar(mesh,I,T_,dS,dt,sigma,r,S))
}

#Finite difference algorithm for vanilla american option
cpp_FD_American <- function(I      = I_g, 
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
   payoff<- Payoff(strike, S, type) 
   mesh[,T_ + 1] <- payoff
   return(cpp_FD_Loop_American(mesh, I, T_, dS, dt, sigma, r, S, payoff))
}

#Finite difference algorithm for barrier american option
cpp_FD_Bar_American <- function(I      = I_g, 
                            strike = strike_g,
                            type   = type_g,
                            sigma  = sigma_g,
                            n_days = n_days_g,
                            r      = r_g,
                            barrier_spread = spread_g)
{
   dS    <- (ifelse(type == 'call', 1, 2) * strike + barrier_spread) / I
   dt    <- 0.45 * (1 + (type == 'call'))/sigma^2/I^2
   T_    <- floor(n_days/dt) + 1
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S <- ((I - 1):0 + (type == 'put')) * dS 
   if (type == 'put') 
      S <- S + strike - barrier_spread
   payoff <- Payoff(strike, S, type) 
   mesh[(1:I) + (type == 'call'),T_ + 1] <- payoff
   return(cpp_FD_Loop_Bar_American(mesh, I, T_, dS, dt, sigma, r, S, payoff, type))
}

#------------Uncertain volatility---------------------

#Finite difference algorithm for vanilla european option
cpp_FD_UV <- function(I   = I_g,      #number of desired asset steps
                   strike = strike_g, 
                   type   = type_g,
                   sigma_min = sigma_min_g,
                   sigma_max = sigma_max_g, 
                   n_days = n_days_g, #time in days
                   r      = r_g)
{
   dS    <- 3 * strike/I         #3*strike stands for the highest asset price considered
   dt    <- 0.5 * 0.9/sigma^2/I^2     #stability condition for time step
   T_    <- floor(n_days/dt) + 1 #number of time steps - adjusted to be an integer
   dt    <- n_days/T_            #time step fitted to grid 
   mesh  <- cpp_zeros_matrix(I + 1, T_ + 1)
   S     <- I:0 * dS
   mesh[,T_ + 1] <- Payoff(strike, S, type)
   return(cpp_FD_Loop_UV(mesh, I, T_, dS, dt, sigma_min, sigma_max, r, S))
}

#Finite difference algorithm for barrier european option
cpp_FD_Bar_UV <- function(I   = I_g, 
                       strike = strike_g,
                       type   = type_g,
                       sigma_min = sigma_min_g,
                       sigma_max = sigma_max_g, 
                       n_days = n_days_g,
                       r      = r_g,
                       barrier_spread = 200)
{
   dS    <- (ifelse(type == 'call', 1, 2) * strike + barrier_spread) / I #put S values range twice the call S values to meet the 3*strike requirement
   dt    <- 0.5 * 0.45 * (1 + (type == 'call'))/sigma^2/I^2
   T_    <- floor(n_days/dt) + 1
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S <- ((I - 1):0 + (type == 'put')) * dS #[from 0 to barrier]
   if (type == 'put') 
      S <- S + strike - barrier_spread #[from barrier to infinity]
   mesh[(1:I) + (type == 'call'),T_ + 1] <- Payoff(strike, S, type) #2:(I+1) for call  
   return(cpp_FD_Loop_Bar_UV(mesh, I, T_, dS, dt, sigma_min, sigma_max, r, S))
}

#Finite difference algorithm for vanilla american option
cpp_FD_American_UV <- function(I   = I_g, 
                            strike = strike_g,
                            type   = type_g,
                            sigma_min = sigma_min_g,
                            sigma_max = sigma_max_g, 
                            n_days = n_days_g,
                            r      = r_g)
{
   dS    <- 3 * strike/I
   dt    <- 0.5 * 0.9/sigma^2/I^2
   T_    <- floor(n_days/dt) + 1
   r     <- r * dt
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S     <- I:0 * dS
   payoff<- Payoff(strike, S, type) 
   mesh[,T_ + 1] <- payoff
   return(cpp_FD_Loop_American_UV(mesh, I, T_, dS, dt, sigma_min, sigma_max, r, S, payoff))
}

#Finite difference algorithm for barrier american option
cpp_FD_Bar_American_UV <- function(I   = I_g, 
                                strike = strike_g,
                                type   = type_g,
                                sigma_min = sigma_min_g,
                                sigma_max = sigma_max_g, 
                                n_days = n_days_g,
                                r      = r_g,
                                barrier_spread = spread_g)
{
   dS    <- (ifelse(type == 'call', 1, 2) * strike + barrier_spread) / I
   dt    <- 0.5 * 0.45 * (1 + (type == 'call'))/sigma^2/I^2
   T_    <- floor(n_days/dt) + 1
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S <- ((I - 1):0 + (type == 'put')) * dS #[from 0 to barrier]
   if (type == 'put') 
      S <- S + strike - barrier_spread
   payoff <- Payoff(strike, S, type) 
   mesh[(1:I) + (type == 'call'),T_ + 1] <- payoff
   return(cpp_FD_Loop_Bar_American_UV(mesh, I, T_, dS, dt, sigma_min, sigma_max, r, S, payoff))
}


#-------------------Dividend-----------------

#Finite difference algorithm for barrier european option
cpp_FD_Bar_Div <- function(I      = I_g, 
                       strike = strike_g,
                       type   = type_g,
                       sigma  = sigma_g,
                       n_days = n_days_g,
                       r      = r_g,
                       barrier_spread = spread_g,
                       div    = div_g)
{
   dS    <- (ifelse(type == 'call', 1, 2) * strike + barrier_spread) / I #put S values range twice the call S values to meet the 3*strike requirement
   dt    <- 0.45 * (1 + (type == 'call'))/sigma^2/I^2 #adjusted by additional 1/2 due to lack of stability
   T_    <- floor(n_days/dt) + 1
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S <- ((I - 1):0 + (type == 'put')) * dS #[from 0 to barrier]
   if (type == 'put') 
      S <- S + strike - barrier_spread #[from barrier to infinity]
   mesh[(1:I) + (type == 'call'),T_ + 1] <- Payoff(strike, S, type) #2:(I+1) for call  
   return(cpp_FD_Loop_Bar_Div(mesh,I,T_,dS,dt,sigma,r,S,div,type))
}

#Finite difference algorithm for vanilla american option
cpp_FD_American_Div <- function(I      = I_g, 
                            strike = strike_g,
                            type   = type_g,
                            sigma  = sigma_g,
                            n_days = n_days_g,
                            r      = r_g,
                            div    = div_g)
{
   dS    <- 3 * strike/I
   dt    <- 0.9/sigma^2/I^2
   T_    <- floor(n_days/dt) + 1
   r     <- r * dt
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S     <- I:0 * dS
   payoff <- Payoff(strike, S, type) 
   payoff_div <- Payoff(strike, S + div, type) 
   mesh[,T_ + 1] <- payoff
   return(cpp_FD_Loop_American_Div(mesh, I, T_, dS, dt, sigma, r, S, payoff, payoff_div, div))
}

#Finite difference algorithm for barrier american option
cpp_FD_Bar_American_Div <- function(I      = I_g, 
                                strike = strike_g,
                                type   = type_g,
                                sigma  = sigma_g,
                                n_days = n_days_g,
                                r      = r_g,
                                barrier_spread = spread_g,
                                div    = div_g)
{
   dS    <- (ifelse(type == 'call', 1, 2) * strike + barrier_spread) / I
   dt    <- 0.45 * (1 + (type == 'call'))/sigma^2/I^2
   T_    <- floor(n_days/dt) + 1
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S <- ((I - 1):0 + (type == 'put')) * dS #[from 0 to barrier]
   if (type == 'put') 
      S <- S + strike - barrier_spread
   payoff <- Payoff(strike, S, type) 
   payoff_div <- Payoff(strike, S + div, type)
   mesh[(1:I) + (type == 'call'),T_ + 1] <- payoff
   return(cpp_FD_Loop_Bar_American_Div(mesh, I, T_, dS, dt, sigma, r, S, payoff, div, type))
}

#------------Uncertain volatility---------------------

#Finite difference algorithm for barrier european option
cpp_FD_Bar_UV_Div <- function(I   = I_g, 
                          strike = strike_g,
                          type   = type_g,
                          sigma_min = sigma_min_g,
                          sigma_max = sigma_max_g, 
                          n_days = n_days_g,
                          r      = r_g,
                          barrier_spread = 200,
                          div    = div_g)
{
   dS    <- (ifelse(type == 'call', 1, 2) * strike + barrier_spread) / I #put S values range twice the call S values to meet the 3*strike requirement
   dt    <- 0.5 * 0.45 * (1 + (type == 'call'))/sigma^2/I^2 #adjusted by additional 1/2 due to lack of stability
   T_    <- floor(n_days/dt) + 1
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S <- ((I - 1):0 + (type == 'put')) * dS #[from 0 to barrier]
   if (type == 'put') 
      S <- S + strike - barrier_spread #[from barrier to infinity]
   mesh[(1:I) + (type == 'call'),T_ + 1] <- Payoff(strike, S, type) #2:(I+1) for call  
   return(cpp_FD_Loop_Bar_UV_Div(mesh, I, T_, dS, dt, sigma_min, sigma_max, r, S, div, type))
}

#Finite difference algorithm for vanilla american option
cpp_FD_American_UV_Div <- function(I   = I_g, 
                               strike = strike_g,
                               type   = type_g,
                               sigma_min = sigma_min_g,
                               sigma_max = sigma_max_g, 
                               n_days = n_days_g,
                               r      = r_g,
                               div    = div_g)
{
   dS    <- 3 * strike/I
   dt    <- 0.45/sigma^2/I^2
   T_    <- floor(n_days/dt) + 1
   r     <- r * dt
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S     <- I:0 * dS
   payoff <- Payoff(strike, S, type) 
   payoff_div <- Payoff(strike, S + div, type) 
   mesh[,T_ + 1] <- payoff
   return(cpp_FD_Loop_American_UV_Div(mesh, I, T_, dS, dt, sigma_min, sigma_max, r, S, payoff, payoff_div))
}

#Finite difference algorithm for barrier american option
cpp_FD_Bar_American_UV_Div <- function(I   = I_g, 
                                   strike = strike_g,
                                   type   = type_g,
                                   sigma_min = sigma_min_g,
                                   sigma_max = sigma_max_g, 
                                   n_days = n_days_g,
                                   r      = r_g,
                                   barrier_spread = spread_g,
                                   div    = div_g)
{
   dS    <- (ifelse(type == 'call', 1, 2) * strike + barrier_spread) / I
   dt    <- 0.5 * 0.45 * (1 + (type == 'call'))/sigma^2/I^2
   T_    <- floor(n_days/dt) + 1
   dt    <- n_days/T_
   mesh  <- matrix(0, nrow = I + 1, ncol = T_ + 1)
   S <- ((I - 1):0 + (type == 'put')) * dS #[from 0 to barrier]
   if (type == 'put') 
      S <- S + strike - barrier_spread
   payoff <- Payoff(strike, S, type) 
   mesh[(1:I) + (type == 'call'),T_ + 1] <- payoff
   return(cpp_FD_Loop_Bar_American_UV_Div(mesh, I, T_, dS, dt, sigma_min, sigma_max, r, S, payoff, div, type))
}