# IF1.2
Finite difference pricing tool for barrier and non-barrier options with nonlinear volatility

-------------------------------
Significant running notes: 

1. The code was developed using RStudio API 1.0.143 and R version 3.3.3.
2. Devtools package is required to run the code (parts of which were written in C++). Binaries are avaialable at CRAN's repository https://cran.r-project.org/web/packages/devtools/index.html - note that it that the installation is manual and can't be perfermed solely within RStudio API
3. The following R libraries shall be automatically installed: Shiny, Rcpp, microbenchmark, ggplot2, reshape2 with all their possible dependencies.
4. Application is memory heavy. One should prepare about 1GB of RAM for full interaction.
5. Computational complexity is also of significant order. Some patience will be expected from user.
6. Variables names are consistent with Paul Wilmott's 'On quantitatative finance'.

-------------------------------
How to run:

1. Source app.R file to load all the data. 
2. Run 3Dshiny.R to see the main graph for visualising the results.
3. Have fun with buttons and sliders.

-------------------------------
Brief description of the subject:

All the theoretical aspects and outcomes are discussed in depth within the attached report (IF1_2_git.pdf). As for the time being only Polish language version is available. 

(to be completed)

-------------------------------
The application was created as a major assignment for the Financial Engineering course at University of Wroclaw, Mathametical Institute. Results were presented in January 2017 in a form of public defence resulting in highest grade possible.
