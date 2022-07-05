


coupon <- function(nbCoupons = 10, prob = rep(1,nbCoupons), stop = 10^9, approx = "none")
{
  if(approx == "none")
  {
    NB <- 0
    collection <- rep(FALSE, nbCoupons)
    coupons <- NULL
    loop <- 0
    while(NB < nbCoupons && loop < stop)
    {
      i <-  sample(x = nbCoupons, size = 1, prob = prob)
      coupons <- c(coupons, i)
      if(collection[i] == FALSE)
      {
        NB <- NB + 1
        collection[i] <- TRUE
      }
      loop <- loop + 1
    }
    return(list(time = length(coupons), coupons = coupons, positions = NULL))
  }
  if(approx == "geom")
  {
    positions <- rgeom(nbCoupons, prob/sum(prob)) + 1
    return(list(time = max(positions), coupons = NULL, positions = positions))
  }
}




couponCensored <- function(nbCoupons = 30, prob = rep(1,nbCoupons), N = 100, type = "2")
{
  coupons <- NULL
  if(type == "1")
  {
    for(i in 1:N)
    {
      i <-  sample(x = nbCoupons, size = 1, prob = prob)
      coupons <- c(coupons, i)
    }
    return(list(counts = table(coupons), coupons = coupons))
  }
  if(type == "2")
  {
    coupons <- rep(0, nbCoupons)
    for(i in 1:N)
    {
      i <- sample(x = nbCoupons, size = 1, prob = prob)
      coupons[i] <- coupons[i] + 1
    }
    return(list(counts = table(coupons)))
  }

}


