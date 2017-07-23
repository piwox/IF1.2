#-------------Comparison to BS model------------



#Returns payoff matrix (vector, value)
Payoff <- function(strike = strike_g,  
                   S      = strike_g,
                   type   = type_g)   #'call' or 'put'
{
   ((type == 'call') * (S > strike) + (type == 'put') * (S < strike)) * abs(strike - S)
}

d1  <- function(strike = strike_g, sigma = sigma_g, S, t = n_days_g) ##where t is time to expiration date
{
   1/(sigma * sqrt(t)) * (log(S/strike) + (r_g + sigma^2/2) * t)
}
d2  <- function(strike =  strike_g, sigma = sigma_g, S, t = n_days_g)
{
   d1(strike, sigma, S, t) - sigma * sqrt(t)
}  

BSPrice <- function(strike = strike_g, 
                    type    = type_g,       
                    sigma  = sigma_g, 
                    S, 
                    t      = n_days_g)  
{

   if(t==0)
      return(Payoff(strike, S, type))
   else
   {
      if(type=='call')
         return(S * pnorm(d1(strike, sigma, S, t))
                -pnorm(d2(strike, sigma, S, t)) * strike * exp(-r_g * t))
      else 
         return(-S*pnorm(-d1(strike, sigma, S, t))
                +pnorm(-d2(strike, sigma, S, t)) * strike * exp(-r_g * t))
   }
}
