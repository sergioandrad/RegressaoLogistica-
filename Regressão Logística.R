set.seed(100)
dados <- rbinom(n = 40, size = 5, prob = 0.1)
n <- 5

#Modelo Nulo:
null_model <-function(dados, n){
soma <- 0
cov <- rep(1, times = length(dados))
ll <-  function(beta){
        fato <- log(factorial(n)/(factorial(n-dados)*factorial(dados)))
        fn_dados <- fato + dados*cov*beta - n*log(1+ exp(cov*beta))
        soma <- sum(fn_dados)
          return(-soma)
}
res <- suppressWarnings(optim(ll, par = c(0), method = 'Brent', upper = 10^8, lower = -10^8))
parametro <- res$par
verossimilhança <- res$value
cat(paste('Parâmetro estimado:',as.character(round(parametro, digits =3)),'\n',
          'Verossimilhança completa do modelo nulo:',as.character(round(res$value, digits =3))), "\n",
          'Graus de liberdade',length(dados)-1                                                                       
                                                                         )

}

null_model(dados, n)

fail <- n - dados
test <-glm(formula = cbind(dados, fail) ~ 1, family= binomial())
test
logLik(test)

#Modelo com cováriaveis:
summary(mtcars)
attach(mtcars) 

cov_mtcars <- cbind(cyl, hp, mpg)
vars <- as.matrix(cbind(am, rep(1, times = length(am)), cov_mtcars))
mtcars
vars

cov_model <- function(dados, n, cov){
  vars <- as.matrix(cbind(rep(1, times = length(dados)), cov))
    pars <- matrix(nrow = ncol(vars), 1)
    fn_dados <- 0
      ll <-  function(pars){
        for(i in c(1:length(dados))){
          fato <- log(factorial(n)/(factorial(n-dados[i])*factorial(dados[i])))
          rep <- fato + dados[i]*vars[i,]%*%pars - n*log(1+ exp(vars[i,]%*%pars))
          fn_dados <- fn_dados + rep
         }
        return(-fn_dados)
      }
      if(ncol(vars)>1){
    res <-optim(ll, par = rep(0, times = ncol(vars)), method = 'BFGS')}
      else{res <-optim(ll, par = rep(0, times = ncol(vars)), method = 'Brent',
                               upper = 10^3, lower = -10^3)}
    parametro <- res$par
    verossimilhança <- res$value
    cat('Parâmetro estimado:', parametro,'\n',
              'Verossimilhança completa do modelo nulo:', verossimilhança, "\n",
        'Graus de liberdade',length(dados)-ncol(cov)-1                                                                       
    )
}

cov_model(dados = am, n = 1, cov = cov_mtcars)

test2 <- glm(am~cyl + hp + mpg, family = binomial())
logLik(test)
test2
