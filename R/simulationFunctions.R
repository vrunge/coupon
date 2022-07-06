



simu.coupon <- function(nbCoupons = 10, prob = rep(1,nbCoupons))
{
  NB <- 0
  collection <- rep(FALSE, nbCoupons)
  time <- 0
  while(NB < nbCoupons)
  {
    i <-  sample(x = nbCoupons, size = 1, prob = prob)
    if(collection[i] == FALSE)
    {
      NB <- NB + 1
      collection[i] <- TRUE
    }
    time <- time + 1
  }
  return(time)
}



simu.coupon_Geom <- function(nbCoupons = 10, prob = rep(1,nbCoupons)/nbCoupons)
{
  return(max(rgeom(nbCoupons, prob)) + 1)
}


##################


simu.couponCensored <- function(nbCoupons = 30, prob = rep(1,nbCoupons), N = 100)
{
  coupons <- rep(0, nbCoupons)
  for(i in 1:N)
  {
    i <- sample(x = nbCoupons, size = 1, prob = prob)
    coupons[i] <- coupons[i] + 1
  }
  return(sum(coupons > 0))
}






estimatorExpectation <- function(nbCoupons = 200, prob = rep(1,nbCoupons), N = 300)
{
  nb_time <- apply(dynamicCollection(nbCoupons = nbCoupons,prob = prob, N = N), 1, function(x) sum(x > 0))
  test <- Inf
  for(nb in max(nb_time):(max(nb_time)+N))
  {
    newTest <- sum((nb_time-nb*(1- (1-1/nb)^(1:myN)))^2)
    if(newTest < test)
    {
      test <- newTest
      myNB <- nb
    }
  }
  return(list(estimation = myNB, max_observation = max(nb_time)))
}
