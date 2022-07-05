



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

