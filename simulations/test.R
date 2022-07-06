
########           ########
######## packages  ########
########           ########

library(parallel)
library(ggplot2)
library(reshape2)
library(cowplot)

cores <- detectCores()
#cores <- 95





########                   ########
######## one.simu function ########
########                   ########

one.simu <- function(i,
                     nbCoupons,
                     beta,
                     type)
{
  if(type == "linear"){proba <- 1/nbCoupons + beta * (1:nbCoupons - (nbCoupons + 1)/2)}
  if(type == "two_proba")
  {
    proba <- c(rep(1, nbCoupons/2),rep(beta, nbCoupons/2))
    proba <- proba/sum(proba)
  }

  res <- simu.coupon(nbCoupons, proba)
  df <- data.frame(numeric(0), numeric(0), numeric(0), numeric(0))
  colnames(df) <- c("index", "nbCoupons", "beta", "time")
  df[1,] <- c(i, nbCoupons, beta, res)
  return(df)
}


####################################
####### : MSE and NbSegment  #######
####################################

nbSimus <- 100 ### nb simu for each experiment
nbCoupons <- 100
myBetas <- seq(from = 0, to = 2/(nbCoupons*(nbCoupons-1))*(1- 1/nbCoupons), length.out = 40)

res <- NULL
for(beta in myBetas)
{
  res <- c(res, mclapply(1:nbSimus, FUN = one.simu,
                         nbCoupons = nbCoupons,
                         beta = beta,
                         type = "linear",
                         mc.cores = cores))
  print(beta)
}

df <- do.call(rbind, res)

write.csv(df, file="minAngles.csv", row.names = FALSE)



###############################
####### read the result #######
###############################



df
# Everything on the same plot
p <- ggplot(df, aes(x = beta, y = time)) + geom_point()
p

####################################################################################



nbSimus <- 100 ### nb simu for each experiment
nbCoupons <- 100
myBetas <- seq(from = 1, to = 50, length.out = 100)

res <- NULL
for(beta in myBetas)
{
  res <- c(res, mclapply(1:nbSimus, FUN = one.simu,
                         nbCoupons = nbCoupons,
                         beta = beta,
                         type = "two_proba",
                         mc.cores = cores))
  print(beta)
}

df <- do.call(rbind, res)

df
# Everything on the same plot
p <- ggplot(df, aes(x = beta, y = time)) + geom_point()
p


plot((1+myBetas)/2 *nbCoupons*log(nbCoupons))


#
# sommer des N/x log(N/x) avec des proba différentes
# facile pour des fonctions en escalier
# converge vers une intégrale pour fonctions continues
#


