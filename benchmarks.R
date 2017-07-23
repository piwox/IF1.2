x <- rnorm(10^6)
microbenchmark(mean(x))
microbenchmark(cpp_mean(x))
mean(x)-cpp_mean(x)

microbenchmark(var(x))
microbenchmark(cpp_var(x))
var(x) - cpp_var(x)

microbenchmark(sd(x))
microbenchmark(cpp_sd(x))
sd(x) - cpp_sd(x)

microbenchmark(matrix(0.0,10^3,10^4))
microbenchmark(cpp_zeros_matrix(10^3,10^4))

microbenchmark(FD(),times = 10)
microbenchmark(cpp_FD())

microbenchmark(cpp_FD(I=1000),times=1)


microbenchmark(cpp_FD(),cpp_FD_Bar(),cpp_FD_American(),cpp_FD_Bar_American(),
               cpp_FD_UV(),cpp_FD_Bar_UV(),cpp_FD_American_UV(),cpp_FD_Bar_American_UV(),
               cpp_FD_Bar_Div(),cpp_FD_American_Div(),cpp_FD_Bar_American_Div(),
               cpp_FD_Bar_UV_Div(),cpp_FD_American_UV_Div(),cpp_FD_Bar_American_UV_Div(),times=10)

microbenchmark(znak_gammy(cpp_FD()),times=1)
microbenchmark(kiedy_wykonac(cpp_FD()),times=10)
