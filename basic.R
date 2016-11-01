rm(list=ls())

# results <- read.csv("Segregation_100K_4CHAINS.csv")
results <- read.csv("150K_4Chains.csv")

head(results)

# apply(results[-c(1:120000),],2,mean)

library(coda)

netlogo_to_coda <- function(fit,drop,chains){
    fit <- fit[-c(1:drop),]
    nvars=ncol(fit)/chains
    
    cols_take <- matrix(data = 1:ncol(fit),ncol = chains)
    temp <- apply(cols_take,MARGIN = 2,FUN = function(x) fit[,x])

    mcmc <- mcmc.list(lapply(temp, function(x) {
        names(x) <- names(fit)[1:nvars]
        mcmc(x)
    }))
    
    return(mcmc)
}


fit_mcmc <- netlogo_to_coda(drop=120000,chains=4,fit=results)

effectiveSize(fit_mcmc)

gelman.plot(fit_mcmc)

traceplot(fit_mcmc)

# Z scores of converges.
geweke.diag(fit_mcmc) 

heidel.diag(fit_mcmc)

autocorr(fit_mcmc)
autocorr.plot(fit_mcmc, ask=TRUE)



