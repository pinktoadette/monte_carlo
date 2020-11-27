# Fit and find a point estimate for the stable population level where R = S. 
# Use the bootstrap method.

R = c(68, 77, 299, 220, 142, 287, 276, 115, 64, 206, 222, 205, 233, 228, 188, 132, 285, 188, 224, 121, 311, 166, 248, 161, 226, 67, 201, 267, 121, 301, 244, 222, 195, 203, 210, 275, 286, 275, 304, 214)
S = c(56, 62, 445, 279, 138, 428, 319, 102, 51, 289, 351, 282, 310, 266, 256, 144, 447, 186, 389, 113, 412, 176, 313, 162, 368, 54, 214, 429, 115, 407, 265, 301, 234, 229, 270, 478, 419, 490, 430, 235)
dat<- data.frame(R,S)
ydat <- log(R/S)
fit <- lm(ydat ~ S)
alpha <- exp(fit$coef[1])
print (alpha)
K <- -1/fit$coef[2]
plot(S, R, xlim=c(0, max(S)), ylim=c(0, max(R)))

B=2000
result = rep(0, B)
for (i in 1:B) {
     boot.sample = sample(length(dat$R), replace = TRUE)
     result[i] = mean(dat$R[boot.sample])
}
hist(result)


# For each sample compute the bootstrap and jackknife estimate for variance for theta = X
# and compute the mean and standard deviation of these variance estimates over the 100
# samples.

set.seed(10)
sample_data <- runif(10, 0, 1)
jackknife <- function(sample_size) {
  #for the specific example given
  x <- sample(1:100, size = sample_size)
  M <- numeric(sample_size)
  
  #take sample, remove one
  for (i in 1:sample_size){
    y <- x[-i]
    M[i] <- median(y)
    
  }
}
  
Mbar <- mean(M)   
Mb <- replicate(1000, expr = {
    y <- sample(x, size = n, replace = TRUE)
    print (median(y))
    print(sd(Mb)) 
})
#> jackknife(10)
  