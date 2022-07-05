

dynamicCollection <- function(nbCoupons = 1000, prob = rep(1,nbCoupons), N = 1000)
{
  image <- matrix(0, nrow = N, ncol = nbCoupons)
  image[1,1] <- 1
  for(i in 2:N)
  {
    j <- sample(x = nbCoupons, size = 1, prob = prob)
    image[i,] <- image[i-1,]
    image[i,j] <- image[i,j] + 1
  }
  image <- apply(image,1, function(x) sort(x,decreasing = TRUE))
  return(t(image))
}
