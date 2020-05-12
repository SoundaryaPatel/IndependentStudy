### double check multivariate normal sampling

library(dplyr)
library(ggplot2)

### use 3 random variables as a check

### define the covariance matrix
Kmat <- matrix(c(1, 0.5, 0.85, 0.5, 2, 0.95, 0.85, 0.95, 1), nrow = 3)

Kmat

### generate 4 random draws of the 3 variables
### use zero means
set.seed(1234)
x4 <- MASS::mvrnorm(n = 4, mu = rep(0, ncol(Kmat)), Sigma = Kmat)

x4

### calculate the cholesky decomposition (lower triangular)
Lmat <- t(chol(Kmat))

Lmat

### generate standard normals
set.seed(1234)
zvec <- rnorm(n = nrow(x4) * ncol(Kmat))

### reshape into a matrix
Zmat <- matrix(zvec, nrow = nrow(x4), byrow = TRUE)

Zmat

### backtransform to the original scale and correlations
x4b <- t(Lmat %*% t(Zmat))

x4b

### double check the correlations from the random draws

set.seed(7653)
x5000 <- MASS::mvrnorm(n = 5000, mu = rep(0, ncol(Kmat)), Sigma = Kmat)

cov(x5000)

cor(x5000)

### the manual approach
set.seed(7653)
zvec5000 <- rnorm(n = nrow(x5000) * ncol(Kmat))

Zmat5000 <- matrix(zvec5000, nrow = nrow(x5000), byrow = TRUE)

x5000b <- t(Lmat %*% t(Zmat5000))

cov(x5000b)

cor(x5000b)
