#-------------------------------------   
# Checking the sign of Gamma and optimal strategies for american options
#-------------------------------------

GammaSign <- function(fd_marix)
  {
   result <- matrix(0, (nrow(fd_marix)),(ncol(fd_marix)))
   for(i in ncol(result):1 ) 
   {
      for(j in 2:(nrow(result)-1))
      {
         result[j,i] = as.integer((fd_marix[j - 1,i] - 2 * fd_marix[j,i] + fd_marix[j + 1,i])>=0)
         #encoded 1 - positive, 0 - negative
      }
   }
   return(result)
}
Gamma <- function(fd_marix, spanS = 7200) #of little interest
{
   gamma <- matrix(0, (nrow(fd_marix)-2),(ncol(fd_marix)))
   dS = spanS/nrow(fd_marix)
   for(i in ncol(gamma):1 ) 
   {
      for(j in 1:nrow(gamma))
      {
         gamma[j,i] = (fd_marix[j+1 - 1,i] - 2 * fd_marix[j+1,i] + fd_marix[j+1 + 1,i])/dS^2
      }
   }
   return(gamma)
}


StoppingTime<-function(fd_marix)
{
   payoff <- fd_marix[,ncol(fd_marix)]
   return(matrix(as.integer(fd_marix==payoff),nrow(fd_marix),ncol(fd_marix)))
}
is.matrix(kiedy_wykonac(cpp_FD_Bar_American()))

