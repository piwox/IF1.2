#------------Executive file--------------

options(error = traceback) #crucial for tracing errors

#------------Libraries-------------------

require('shiny')
require('ggplot2')
require('reshape2')
require('plotly')
require('Rcpp')
require('microbenchmark')

#------------Set global variables--------

source('globals.R')
type_g   = 'call'       #choose 'call' or 'put' 
strike_g = 2400         #project requirement
sigma    = sigma_g  
sigma_max_g <- 0.3/sqrt(252) #project requirement
sigma_min_g <- 0.2/sqrt(252) #project requirement
n_days_g = n_days_0
I_g      = 300 
r_g      = log(1 + 0.0129)/252 #based on 32-week treasury
spread_g = 200          #project requirement
div_g    = 200

#------------Load functions-------------


source('fd_algorithms.R')
source('uncertain_vol.R')
sourceCpp('cpp_facilities.cpp')
source('uncertain_vol.R')
source('cpp_fd_algorithms.R')
source('gamma_time.R')
source('bsprice.R')

#------------Test batch-----------------

source('test.R', echo = T) #visual comparisons - comment if needless

ifelse(is.null(warnings()),'Application loaded succesfully!',warnings())

