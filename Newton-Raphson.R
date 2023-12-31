pressure <- c(1051, 1337, 1389, 1921, 1942, 2322, 3629, 4006, 4012, 
              4063, 4921, 5445, 5620, 5817, 5905, 5956, 6068, 6121, 
              6473, 7501, 7886, 8108, 8546, 8666, 8831, 9106, 9711, 
              9806, 10205, 10396, 10861, 11026, 11214, 11362, 11604,
              11608, 11745, 11762, 11895, 12044, 13520, 13670, 14110, 
              14496, 15395, 16179, 17092, 17568, 17568)

hist(pressure, nclass=17, xlim=c(0,20000), xlab="Time to failure (hours)")

## Maximum likelihood estimator of the Weibull scale parameter (theta)
## The shape parameter (lambda) is fixed.

theta.hat <- function(y, lambda){
  ( sum(y^lambda) / length(y) )^(1/lambda)
}

theta.hat(y=pressure, lambda=2)