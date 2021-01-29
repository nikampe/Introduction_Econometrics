# install.packages(MASS)  # Falls noch nicht vorhanden
library(MASS)

set.seed(412354329)

n = 1000

mu = c(0,0)
Sigma = matrix(c(100,-7,-7,1),2,2)
err = mvrnorm(n, mu, Sigma)
u = err[,1]
v = err[,2]

z = rbinom(n,1,0.5)

pi0 = -1
pi1 = 1
KV = pi0 + pi1 * z + v
KV = as.numeric(KV > 0)

b0 = 50
b1 = 10
G = b0 + b1 * KV + u