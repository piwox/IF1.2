#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
double cpp_Payoff(double strike, 
                  double S, 
                  std::string type)
{
   std::string c("call");
   std::string p("put");
   return ((type == c) * (S > strike) + (type == p) * (S < strike)) * std::abs(strike - S);
}

//[[Rcpp::export]]
NumericMatrix cpp_FD_Loop(NumericMatrix mesh, 
                          int I, 
                          int T_, 
                          double dS, 
                          double dt, 
                          double sigma, 
                          double r, 
                          NumericVector S)
{
   for(int t = T_ ; t > 0; t--)
   {
      for(int i = 1; i < I; i++)
      {
         double Delta = (mesh(i - 1,t) - mesh(i + 1,t))/(2 * dS); //rows indices numbers have opposite order, hence the switch 
         double Gamma = (mesh(i - 1,t) - 2 * mesh(i,t) + mesh(i + 1,t))/pow(dS,2.0);
         double theta = r * mesh(i,t) - 0.5 * pow(sigma * S[i],2.0) * Gamma - r * S[i] * Delta; //from Black-Scholes equation
         mesh(i,t - 1) = mesh(i,t) - dt * theta; //option value drops by theta when asset price is fixed at particular level
      }
      mesh(I,t - 1) = mesh(I,t) * (1 - r * dt);
      mesh(0,t - 1) = 2 * mesh(1,t - 1) - mesh(2,t - 1);
//      mesh[c(I+1,1),t-1] <- c(mesh[I + 1,t] * (1 - r * dt),2 * mesh[2,t - 1] - mesh[3,t - 1]) #forward differnce for the last S value, and boundary at 0
   }
   return mesh;
}

//[[Rcpp::export]]
NumericMatrix cpp_FD_Loop_Bar(NumericMatrix mesh, 
                              int I, 
                              int T_, 
                              double dS, 
                              double dt, 
                              double sigma, 
                              double r, 
                              NumericVector S)
{
   for(int t = T_ ; t > 0; t--)
   {
      for(int i = 1; i < I; i++)
      {
         double Delta = (mesh(i - 1,t) - mesh(i + 1,t))/(2 * dS); //rows indices numbers have opposite order, hence the switch 
         double Gamma = (mesh(i - 1,t) - 2 * mesh(i,t) + mesh(i + 1,t))/pow(dS,2.0);
         double theta = r * mesh(i,t) - 0.5 * pow(sigma * S[i],2.0) * Gamma - r * S[i] * Delta; //from Black-Scholes equation
         mesh(i,t - 1) = mesh(i,t) - dt * theta; //option value drops by theta when asset price is fixed at particular level
      }
   }
   return mesh;
}

//[[Rcpp::export]]
NumericMatrix cpp_FD_Loop_American(NumericMatrix mesh, 
                                   int I, 
                                   int T_, 
                                   double dS, 
                                   double dt, 
                                   double sigma, 
                                   double r, 
                                   NumericVector S,
                                   NumericVector payoff)
{
   for(int t = T_ ; t > 0; t--)
   {
      for(int i = 1; i < I; i++)
      {
         double Delta = (mesh(i - 1,t) - mesh(i + 1,t))/(2 * dS); //rows indices numbers have opposite order, hence the switch 
         double Gamma = (mesh(i - 1,t) - 2 * mesh(i,t) + mesh(i + 1,t))/pow(dS,2.0);
         double theta = r * mesh(i,t) - 0.5 * pow(sigma * S[i],2.0) * Gamma - r * S[i] * Delta; //from Black-Scholes equation
         mesh(i,t - 1) = std::max(mesh(i,t) - dt * theta, payoff[i]); //option value drops by theta when asset price is fixed at particular level
      }
      mesh(I,t - 1) = mesh(I,t);
      mesh(0,t - 1) = 2 * mesh(1,t - 1) - mesh(2,t - 1);
      //      mesh[c(I+1,1),t-1] <- c(mesh[I + 1,t] * (1 - r * dt),2 * mesh[2,t - 1] - mesh[3,t - 1]) #forward differnce for the last S value, and boundary at 0
   }
   return mesh;
}

//[[Rcpp::export]]
NumericMatrix cpp_FD_Loop_Bar_American(NumericMatrix mesh, 
                                       int I, 
                                       int T_, 
                                       double dS, 
                                       double dt, 
                                       double sigma, 
                                       double r, 
                                       NumericVector S,
                                       NumericVector payoff,
                                       std::string type)
{
   for(int t = T_ ; t > 0; t--)
   {
      for(int i = 1; i < I; i++)
      {
         double Delta = (mesh(i - 1,t) - mesh(i + 1,t))/(2 * dS); //rows indices numbers have opposite order, hence the switch 
         double Gamma = (mesh(i - 1,t) - 2 * mesh(i,t) + mesh(i + 1,t))/pow(dS,2.0);
         double theta = r * mesh(i,t) - 0.5 * pow(sigma * S[i],2.0) * Gamma - r * S[i] * Delta; //from Black-Scholes equation
         mesh(i,t - 1) = std::max(mesh(i,t) - dt * theta, payoff[i - (type == "call")]); //option value drops by theta when asset price is fixed at particular level
      }
      //      mesh[c(I+1,1),t-1] <- c(mesh[I + 1,t] * (1 - r * dt),2 * mesh[2,t - 1] - mesh[3,t - 1]) #forward differnce for the last S value, and boundary at 0
   }
   return mesh;
}

//----------Uncerain volatility--------------------

//[[Rcpp::export]]
NumericMatrix cpp_FD_Loop_UV(NumericMatrix mesh,
                             int I, 
                             int T_, 
                             double dS, 
                             double dt, 
                             double sigma_min,
                             double sigma_max,
                             double r, 
                             NumericVector S)
{
   for(int t = T_ ; t > 0; t--)
   {
      for(int i = 1; i < I; i++)
      {
         double Delta = (mesh(i - 1,t) - mesh(i + 1,t))/(2 * dS); //rows indices numbers have opposite order, hence the switch 
         double Gamma = (mesh(i - 1,t) - 2 * mesh(i,t) + mesh(i + 1,t))/pow(dS,2.0);
         double theta = r * mesh(i,t) - 0.5 * pow(((Gamma > 0)?sigma_max:sigma_min) * S[i],2.0) * Gamma - r * S[i] * Delta; //from Black-Scholes equation
         mesh(i,t - 1) = mesh(i,t) - dt * theta; //option value drops by theta when asset price is fixed at particular level
      }
      mesh(I,t - 1) = mesh(I,t) * (1 - r * dt);
      mesh(0,t - 1) = 2 * mesh(1,t - 1) - mesh(2,t - 1);
      //      mesh[c(I+1,1),t-1] <- c(mesh[I + 1,t] * (1 - r * dt),2 * mesh[2,t - 1] - mesh[3,t - 1]) #forward differnce for the last S value, and boundary at 0
   }
   return mesh;
}

//[[Rcpp::export]]
NumericMatrix cpp_FD_Loop_Bar_UV(NumericMatrix mesh, 
                              int I, 
                              int T_, 
                              double dS, 
                              double dt, 
                              double sigma_min,
                              double sigma_max, 
                              double r, 
                              NumericVector S)
{
   for(int t = T_ ; t > 0; t--)
   {
      for(int i = 1; i < I; i++)
      {
         double Delta = (mesh(i - 1,t) - mesh(i + 1,t))/(2 * dS); //rows indices numbers have opposite order, hence the switch 
         double Gamma = (mesh(i - 1,t) - 2 * mesh(i,t) + mesh(i + 1,t))/pow(dS,2.0);
         double theta = r * mesh(i,t) - 0.5 * pow(((Gamma > 0)?sigma_max:sigma_min) * S[i],2.0) * Gamma - r * S[i] * Delta; //from Black-Scholes equation
         mesh(i,t - 1) = mesh(i,t) - dt * theta; //option value drops by theta when asset price is fixed at particular level
      }
   }
   return mesh;
}

//[[Rcpp::export]]
NumericMatrix cpp_FD_Loop_American_UV(NumericMatrix mesh, 
                                   int I, 
                                   int T_, 
                                   double dS, 
                                   double dt, 
                                   double sigma_min,
                                   double sigma_max, 
                                   double r, 
                                   NumericVector S,
                                   NumericVector payoff)
{
   for(int t = T_ ; t > 0; t--)
   {
      for(int i = 1; i < I; i++)
      {
         double Delta = (mesh(i - 1,t) - mesh(i + 1,t))/(2 * dS); //rows indices numbers have opposite order, hence the switch 
         double Gamma = (mesh(i - 1,t) - 2 * mesh(i,t) + mesh(i + 1,t))/pow(dS,2.0);
         double theta = r * mesh(i,t) - 0.5 * pow(((Gamma > 0)?sigma_max:sigma_min) * S[i],2.0) * Gamma - r * S[i] * Delta; //from Black-Scholes equation
         mesh(i,t - 1) = std::max(mesh(i,t) - dt * theta, payoff[i]); //option value drops by theta when asset price is fixed at particular level
      }
      mesh(I,t - 1) = mesh(I,t);
      mesh(0,t - 1) = 2 * mesh(1,t - 1) - mesh(2,t - 1);
      //      mesh[c(I+1,1),t-1] <- c(mesh[I + 1,t] * (1 - r * dt),2 * mesh[2,t - 1] - mesh[3,t - 1]) #forward differnce for the last S value, and boundary at 0
   }
   return mesh;
}

//[[Rcpp::export]]
NumericMatrix cpp_FD_Loop_Bar_American_UV(NumericMatrix mesh, 
                                       int I, 
                                       int T_, 
                                       double dS, 
                                       double dt, 
                                       double sigma_min,
                                       double sigma_max, 
                                       double r, 
                                       NumericVector S,
                                       NumericVector payoff)
{
   for(int t = T_ ; t > 0; t--)
   {
      for(int i = 1; i < I; i++)
      {
         double Delta = (mesh(i - 1,t) - mesh(i + 1,t))/(2 * dS); //rows indices numbers have opposite order, hence the switch 
         double Gamma = (mesh(i - 1,t) - 2 * mesh(i,t) + mesh(i + 1,t))/pow(dS,2.0);
         double theta = r * mesh(i,t) - 0.5 * pow(((Gamma > 0)?sigma_max:sigma_min) * S[i],2.0) * Gamma - r * S[i] * Delta; //from Black-Scholes equation
         mesh(i,t - 1) = std::max(mesh(i,t) - dt * theta, payoff[i]); //option value drops by theta when asset price is fixed at particular level
      }
      //      mesh[c(I+1,1),t-1] <- c(mesh[I + 1,t] * (1 - r * dt),2 * mesh[2,t - 1] - mesh[3,t - 1]) #forward differnce for the last S value, and boundary at 0
   }
   return mesh;
}

//---------Dividend-------------------------------



//[[Rcpp::export]]
NumericMatrix cpp_FD_Loop_Bar_Div(NumericMatrix mesh, 
                              int I, 
                              int T_, 
                              double dS, 
                              double dt, 
                              double sigma, 
                              double r, 
                              NumericVector S,
                              double div,
                              std::string type)
{
   for(int t = T_ ; t > 0; t--)
   {
      for(int i = 1; i < I; i++)
      {
         double Delta = (mesh(i - 1,t) - mesh(i + 1,t))/(2 * dS); //rows indices numbers have opposite order, hence the switch 
         double Gamma = (mesh(i - 1,t) - 2 * mesh(i,t) + mesh(i + 1,t))/pow(dS,2.0);
         double theta = r * mesh(i,t) - 0.5 * pow(sigma * S[i],2.0) * Gamma - r * S[i] * Delta; //from Black-Scholes equation
         mesh(i,t - 1) = mesh(i,t) - dt * theta; //option value drops by theta when asset price is fixed at particular level
      }
      if (t == floor(T_/2))
      {
         if(type == "call")
         {
            for( int i = I; i >= I - div/dS; i--)
               mesh(i,t - 1) = 0.0;
            for( int i = 1; i < I - div/dS; i++)
               mesh(i,t - 1) = mesh(static_cast<int>(i + floor(div/dS)),t - 1); 
         }
         else
         {   
            for( int i = 1; i < I - div/dS; i++)
               mesh(i,t - 1) = mesh(static_cast<int>(i + floor(div/dS)),t - 1);
            for( int i = I; i >= I - div/dS; i--)
               mesh(i,t - 1) = 0.0;
         }
      }
   }
   return mesh;
}

//[[Rcpp::export]]
NumericMatrix cpp_FD_Loop_American_Div(NumericMatrix mesh, 
                                   int I, 
                                   int T_, 
                                   double dS, 
                                   double dt, 
                                   double sigma, 
                                   double r, 
                                   NumericVector S,
                                   NumericVector payoff,
                                   NumericVector payoff_div,
                                   double div)
{
   for(int t = T_; t > 0; t--)
   {
      if(t == floor(T_/2))
         payoff = payoff_div;  
      for(int i = 1; i < I; i++)
      {
         double Delta = (mesh(i - 1,t) - mesh(i + 1,t))/(2 * dS); //rows indices numbers have opposite order, hence the switch 
         double Gamma = (mesh(i - 1,t) - 2 * mesh(i,t) + mesh(i + 1,t))/pow(dS,2.0);
         double theta = r * mesh(i,t) - 0.5 * pow(sigma * S[i],2.0) * Gamma - r * S[i] * Delta; //from Black-Scholes equation
         mesh(i,t - 1) = std::max(mesh(i,t) - dt * theta, payoff[i]); //option value drops by theta when asset price is fixed at particular level
      }
      mesh(I,t - 1) = mesh(I,t);
      mesh(0,t - 1) = 2 * mesh(1,t - 1) - mesh(2,t - 1);
   }
   
   return mesh;
}

//[[Rcpp::export]]
NumericMatrix cpp_FD_Loop_Bar_American_Div(NumericMatrix mesh, 
                                       int I, 
                                       int T_, 
                                       double dS, 
                                       double dt, 
                                       double sigma, 
                                       double r, 
                                       NumericVector S,
                                       NumericVector payoff,
                                       double div,
                                       std::string type)
{
   for(int t = T_ ; t > 0; t--)
   {
      for(int i = 1; i < I; i++)
      {
         double Delta = (mesh(i - 1,t) - mesh(i + 1,t))/(2 * dS); //rows indices numbers have opposite order, hence the switch 
         double Gamma = (mesh(i - 1,t) - 2 * mesh(i,t) + mesh(i + 1,t))/pow(dS,2.0);
         double theta = r * mesh(i,t) - 0.5 * pow(sigma * S[i],2.0) * Gamma - r * S[i] * Delta; //from Black-Scholes equation
         mesh(i,t - 1) = std::max(mesh(i,t) - dt * theta, payoff[i - (type == "call")]); //option value drops by theta when asset price is fixed at particular level
      }
      if (t == floor(T_/2))
      {
         for( int i = 1; i <= I - div/dS; i++)
            mesh(i,t - 1) = mesh(static_cast<int>(i + floor(div/dS)),t - 1); 
         for( int i = I; i > I - div/dS; i--)
            mesh(i,t - 1) = 0.0;
      }
     // mesh(0,t - 1) = 2 * mesh(1,t - 1) - mesh(2,t - 1);
      //      mesh[c(I+1,1),t-1] <- c(mesh[I + 1,t] * (1 - r * dt),2 * mesh[2,t - 1] - mesh[3,t - 1]) #forward differnce for the last S value, and boundary at 0
   }
   return mesh;
}

//----------Uncertain volatility along with dividend--------------------


//[[Rcpp::export]]
NumericMatrix cpp_FD_Loop_Bar_UV_Div(NumericMatrix mesh, 
                                  int I, 
                                  int T_, 
                                  double dS, 
                                  double dt, 
                                  double sigma_min,
                                  double sigma_max, 
                                  double r, 
                                  NumericVector S,
                                  double div,
                                  std::string type)
{
   for(int t = T_ ; t > 0; t--)
   {
      for(int i = 1; i < I; i++)
      {
         double Delta = (mesh(i - 1,t) - mesh(i + 1,t))/(2 * dS); //rows indices numbers have opposite order, hence the switch 
         double Gamma = (mesh(i - 1,t) - 2 * mesh(i,t) + mesh(i + 1,t))/pow(dS,2.0);
         double theta = r * mesh(i,t) - 0.5 * pow(((Gamma > 0)?sigma_max:sigma_min) * S[i],2.0) * Gamma - r * S[i] * Delta; //from Black-Scholes equation
         mesh(i,t - 1) = mesh(i,t) - dt * theta; //option value drops by theta when asset price is fixed at particular level
      }
      if (t == floor(T_/2))
      {
         if(type == "call")
         {
            for( int i = I; i >= I - div/dS; i--)
               mesh(i,t - 1) = 0.0;
            for( int i = 1; i < I - div/dS; i++)
               mesh(i,t - 1) = mesh(static_cast<int>(i + floor(div/dS)),t - 1); 
         }
         else
         {   
            for( int i = 1; i < I - div/dS; i++)
               mesh(i,t - 1) = mesh(static_cast<int>(i + floor(div/dS)),t - 1);
            for( int i = I; i >= I - div/dS; i--)
               mesh(i,t - 1) = 0.0;
         }
      }
      
   }
   return mesh;
}

//[[Rcpp::export]]
NumericMatrix cpp_FD_Loop_American_UV_Div(NumericMatrix mesh, 
                                       int I, 
                                       int T_, 
                                       double dS, 
                                       double dt, 
                                       double sigma_min,
                                       double sigma_max, 
                                       double r, 
                                       NumericVector S,
                                       NumericVector payoff,
                                       NumericVector payoff_div)
{
   for(int t = T_; t > 0; t--)
   {
      if(t == floor(T_/2))
         payoff = payoff_div;  
      for(int i = 1; i < I; i++)
      {
         double Delta = (mesh(i - 1,t) - mesh(i + 1,t))/(2 * dS); //rows indices numbers have opposite order, hence the switch 
         double Gamma = (mesh(i - 1,t) - 2 * mesh(i,t) + mesh(i + 1,t))/pow(dS,2.0);
         double theta = r * mesh(i,t) - 0.5 * pow(((Gamma > 0)?sigma_max:sigma_min) * S[i],2.0) * Gamma - r * S[i] * Delta; //from Black-Scholes equation
         mesh(i,t - 1) = std::max(mesh(i,t) - dt * theta, payoff[i]); //option value drops by theta when asset price is fixed at particular level
      }
      mesh(I,t - 1) = mesh(I,t);
      mesh(0,t - 1) = 2 * mesh(1,t - 1) - mesh(2,t - 1);
   }
   
   return mesh;
}

//[[Rcpp::export]]
NumericMatrix cpp_FD_Loop_Bar_American_UV_Div(NumericMatrix mesh, 
                                           int I, 
                                           int T_, 
                                           double dS, 
                                           double dt, 
                                           double sigma_min,
                                           double sigma_max,
                                           double r, 
                                           NumericVector S,
                                           NumericVector payoff,
                                           double div,
                                           std::string type)
{
   for(int t = T_ ; t > 0; t--)
   {
      for(int i = 1; i < I; i++)
      {
         double Delta = (mesh(i - 1,t) - mesh(i + 1,t))/(2 * dS); //rows indices numbers have opposite order, hence the switch 
         double Gamma = (mesh(i - 1,t) - 2 * mesh(i,t) + mesh(i + 1,t))/pow(dS,2.0);
         double theta = r * mesh(i,t) - 0.5 * pow(((Gamma > 0)?sigma_max:sigma_min) * S[i],2.0) * Gamma - r * S[i] * Delta; //from Black-Scholes equation
         mesh(i,t - 1) = std::max(mesh(i,t) - dt * theta, payoff[i - (type == "call")]); //option value drops by theta when asset price is fixed at particular level
      }
      if (t == floor(T_/2))
      {
         for( int i = 1; i <= I - div/dS; i++)
            mesh(i,t - 1) = mesh(static_cast<int>(i + floor(div/dS)),t - 1); 
         for( int i = I; i > I - div/dS; i--)
            mesh(i,t - 1) = 0.0;
      }
      // mesh(0,t - 1) = 2 * mesh(1,t - 1) - mesh(2,t - 1);
      //      mesh[c(I+1,1),t-1] <- c(mesh[I + 1,t] * (1 - r * dt),2 * mesh[2,t - 1] - mesh[3,t - 1]) #forward differnce for the last S value, and boundary at 0
   }
   return mesh;
}

//---------Additional functions-------------------

//[[Rcpp::export]]
NumericMatrix cpp_zeros_matrix(int nrow, int ncol)
{
   return NumericMatrix(nrow, ncol);
}

 
// [[Rcpp::export]]
double cpp_mean(NumericVector x)
{
   return std::accumulate(x.begin(),x.end(),0.0)/x.size();
}

//[[Rcpp::export]]
double cpp_var(NumericVector x, bool bias = false)
{
   double mean = cpp_mean(x);
   double sum = 0.0;
   int n = x.size();
   for(int i = 0; i < n; i++){
      sum += pow(x[i] - mean, 2.0); 
   }
   return sum/(n - !bias);
}
//[[Rcpp::export]]
double cpp_sd(NumericVector x, bool bias = false)
{
   double mean = cpp_mean(x);
   double sum = 0.0;
   int n = x.size();
   for(int i = 0; i < n; i++){
      sum += pow(x[i] - mean, 2.0); 
   }
   return sqrt(sum/(n - !bias));
}






