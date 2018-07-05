
set.seed(100)
dados <- rbinom(n = 40, size = 5, prob = 0.1)
cov <- rep(1, times = 40)
n <- 5
soma <- 0

ll <-  function(beta){
        fato <- log(factorial(n)/(factorial(n-dados)*factorial(dados)))
        fn_dados <- fato + dados*cov*beta - n*log(1+ exp(cov*beta))
        soma <- sum(fn_dados)
          return(-soma)
}

optim(ll, par = c(0), method = 'Brent', upper = 100, lower = -100)

fail <- n - dados

a <-glm(formula = cbind(dados, fail) ~ 1, family= binomial())
logLik(a)

