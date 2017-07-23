strike_g    <- 2400
type_g      <- 'put'
sigma       <- sigma_g
sigma_max_g <- 0.3/sqrt(252)
sigma_min_g <- 0.2/sqrt(252)
I_g         <- 300 
r_g         <- log(1 + 0.02)/252
spread_g    <- 200

#--- Non Cpp funtions, needless thus omitted---------

# m1 <- FD()
# m2 <- FD_Bar()
# 
# plot(m2[,1], type = 'l')
# plot(m1[,1], type = 'l')
# m3 <- FD_American()
# m4 <- FD_Bar_American()
# plot(m4[,1], type = 'l')
# lines(m2[,1])
# plot(m3[,1], type = 'l')
# plot(m3[,1] - m1[,1], type = 'l')
# 
# m5 <- FD_UV()
# m6 <- FD_Bar_UV()
# 
# m2_max <- FD_Bar(sigma = sigma_max_g)
# m2_min <- FD_Bar(sigma = sigma_min_g)
# 
# plot(m6[,1], type = 'l')
# lines(m2_max[,1])
# lines(m2_min[,1])
# 
# plot(cpp_FD()[,1], type = 'l')
# lines(BSPrice(S = seq(7200, 0, by = -24)))
# 
# plot(cpp_FD(I = 1800)[,1] - BSPrice(S = seq(7200, 0, by = -4)), type = 'l')
# 
# plot(m6[,1], type = 'l')
# plot(m5[,1], type = 'l')
# m7 <- FD_American_UV()
# m8 <- FD_Bar_American_UV()
# plot(m8[,1], type = 'l')
# lines(m6[,1])
# plot(m5[,1] - m1[,1],type='l')
# plot(m3[,1] - m1[,1],type='l')
# plot(m7[,1] - m3[,1],type='l')
# 
# rm(m1,m2,m3,m4,m5,m6,m7,m8)

#---------------Plots for general check--------------

plot(cpp_FD_Bar_American_UV()[,1], type = 'l')
plot(cpp_FD_UV()[,1], type = 'l')
lines(cpp_FD_American_UV()[,1])
plot(cpp_FD_Bar_UV()[,1], type = 'l')
lines(cpp_FD_Bar(sigma = sigma_max_g)[,1])
lines(cpp_FD_Bar(sigma = sigma_min_g)[,1])

plot(cpp_FD_American_UV()[,1],type='l')
lines(cpp_FD_American(sigma = sigma_max_g)[,1])
lines(cpp_FD_American(sigma = sigma_min_g)[,1])

ncollow <- ncol(cpp_FD_Bar_Div())
ncolhigh <- ncol(cpp_FD_Bar_Div(I=1000,div=200))
plot(cpp_FD_Bar_Div()[,ncollow])
plot(cpp_FD_Bar_Div(I=1000,div=200)[,ncolhigh])
rm(ncollow,ncolhigh)

#---------------Plotly tryout--------------


type_g = 'call'
m2df <- as.data.frame(cpp_FD_Bar_Div(I=150,div=200, barrier_spread = 200)[1:61,])
g<-plot_ly(m2df, type = 'surface',z =~as.matrix(m2df), y =~1:61, x =~1:495, colors)
rm(m2df)

type_g = 'put'
m2dfput <- as.data.frame(cpp_FD_Bar_Div(I=100,div=300,barrier_spread = 500)[61:101,])
plot_ly() %>% add_surface(z= ~as.matrix(m2dfput), opacity = 0.98,inherit = FALSE) %>% add_surface(z= ~znak_gammy(m2dfput)-2, opacity = 1,inherit = FALSE)
m2dfput[,1]
rm(m2dfput)

m2dfam <- as.data.frame(cpp_FD_American_Div(I=150,div=200)[1:151,])
plot_ly(m2dfam, type = 'surface',z =~as.matrix(m2dfam), y =~1:151, x =~1:495)
m2dfam[,111]
rm(m2dfam)

type_g = 'call'
m2dfamcall <- as.data.frame(cpp_FD_American_Div(I=100,div=200)[1:101,])
plot_ly(m2dfamcall, type = 'surface',z =~as.matrix(m2dfamcall), y =~1:101, x =~1:221)
rm(m2dfamcall)

type_g='put'
plot(cpp_FD_American_Div(div=200)[,1],type='l')
lines(cpp_FD_American()[,1])
plot(cpp_FD_American_Div(div=200)[,1]-cpp_FD_American()[,1],type='l')

type_g = 'call'
m2dfambarcall <- as.data.frame(cpp_FD_Bar_American_Div(I=100,div=500,barrier_spread = 1000)[1:101,])
plot_ly(m2dfambarcall, type = 'surface',z =~as.matrix(m2dfambarcall), y =~1:101, x =~1:221)
rm(m2dfambarcall)

type_g = 'put'
m2dfambarput <- as.data.frame(cpp_FD_Bar_American_Div(I=100,div=400,barrier_spread = 2000)[1:101,])
plot_ly(m2dfambarput, type = 'surface',z =~as.matrix(m2dfambarput), y =~1:101, x =~1:440)
rm(m2dfambarput)

#-------Uncertain volatility & dividend-----

type_g = 'call'
m2df_UV <- as.data.frame(cpp_FD_Bar_UV_Div(I=150,div=200, barrier_spread = 200)[1:61,])
plot_ly(m2df_UV, type = 'surface',z =~as.matrix(m2df_UV), y =~1:61, x =~1:988)
rm(m2df_UV)

type_g = 'put'
m2dfput_UV <- as.data.frame(cpp_FD_Bar_UV_Div(I=100,div=300,barrier_spread = 500)[61:101,])
plot_ly(m2dfput_UV, type = 'surface',z =~as.matrix(m2dfput_UV), y =~61:101, x =~1:879)
m2dfput_UV[,1]
rm(m2dfput_UV)

m2dfam_UV <- as.data.frame(cpp_FD_American_UV_Div(I=150,div=200)[1:151,])
plot_ly(m2dfam_UV, type = 'surface',z =~as.matrix(m2dfam_UV), y =~1:151, x =~1:988)
m2dfam_UV[,111]
rm(m2dfam_UV)

type_g = 'call'
m2dfamcall_UV <- as.data.frame(cpp_FD_American_UV_Div(I=100,div=200)[1:101,])
plot_ly(m2dfamcall_UV, type = 'surface',z =~as.matrix(m2dfamcall_UV), y =~1:101, x =~1:440)

rm(m2dfamcall_UV)

type_g='put'
plot(cpp_FD_American_UV_Div(div=200)[,1],type='l')
lines(cpp_FD_American_UV()[,1])
plot(cpp_FD_American_UV_Div(div=200)[,1]-cpp_FD_American_UV()[,1],type='l')

type_g = 'call'
m2dfambarcall_UV <- as.data.frame(cpp_FD_Bar_American_UV_Div(I=100,div=1000,barrier_spread = 1000)[1:101,])
plot_ly(m2dfambarcall_UV, type = 'surface',z =~as.matrix(m2dfambarcall_UV), y =~1:101, x =~1:440)
rm(m2dfambarcall_UV)

type_g = 'put'
m2dfambarput_UV <- as.data.frame(cpp_FD_Bar_American_UV_Div(I=100,div=400,barrier_spread = 2000)[1:101,])
plot_ly(m2dfambarput_UV, type = 'surface',z =~as.matrix(m2dfambarput_UV), y =~1:101, x =~1:879)
rm(m2dfambarput_UV)

Reset()