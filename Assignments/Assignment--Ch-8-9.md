---
title: 'Assignment: Ch 8-9'
author: "Monikrishna Roy"
date: "04/02/2021 (updated: 2021-05-11)"
output: 
  bookdown::html_document2:
    number_sections: FALSE
    toc: yes
    keep_md: yes
---

<style type="text/css">
h1.title {
  text-align: center;
}
h4.author {
  text-align: center;
}
h4.date {
  text-align: center;
}
</style>




## Ex 8.7

Code to estimate the bias and standard error of $\hat{\theta}$ using $bootstrap$.


```r
# Using the equation 
lambda.hat <- eigen(cov(scor))$values 
theta.hat <- lambda.hat[1]/sum(lambda.hat) 
theta.hat
```

```
## [1] 0.619115
```

```r
# Bootstrap estimates of its bias and std
theta.i <- function(x, i) {
  eigen(cov(x[i, ]))$values[1] / sum(eigen(cov(x[i, ]))$values)
}

B <- 2000 #no. boostrap resamples
boot.out <- boot(data = scor, statistic = theta.i, R = B)
boot.out
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = scor, statistic = theta.i, R = B)
## 
## 
## Bootstrap Statistics :
##     original      bias    std. error
## t1* 0.619115 0.002070387  0.04876563
```

## Ex 8.8

Continuing with the test score data from previous Exercise, to find jackknife estimates of bias and std. error. Sample R code.


```r
theta <- function(x) {
  eigen(cov(x))$values[1] / sum(eigen(cov(x))$values)
}

n <- length(scor[, 1])
x <- as.matrix(scor)
theta.jack <- numeric(n)

for (i in 1:n) {
  theta.jack[i] <- theta(x[-i, ])
}

bias.jack <- (n - 1) * (mean(theta.jack) - theta.hat)
theta.bar <- mean(theta.jack)
se.jack <- sqrt((n - 1) * mean((theta.jack - theta.bar) ^ 2))

cbind(theta.hat = theta(scor), bias = bias.jack, se = se.jack)
```

```
##      theta.hat        bias         se
## [1,]  0.619115 0.001069139 0.04955231
```

## Ex 8.9

Continuing with the test score data from previous Exercise, to find 95% bootstrap confidence intervals for $\hat{\theta}$, sample R code is.


```r
theta.i = function(x, i) {
  eigen(cov(x[i,]))$values[1] / sum(eigen(cov(x[i,]))$values)
}

B = 2000
boot.out <- boot(data = scor, statistic = theta.i, R = B)
boot.out
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = scor, statistic = theta.i, R = B)
## 
## 
## Bootstrap Statistics :
##     original       bias    std. error
## t1* 0.619115 0.0001706823  0.04636184
```

```r
boot.ci(boot.out, type = c('perc', 'bca')) 
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 2000 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = boot.out, type = c("perc", "bca"))
## 
## Intervals : 
## Level     Percentile            BCa          
## 95%   ( 0.5219,  0.7055 )   ( 0.5196,  0.7044 )  
## Calculations and Intervals on Original Scale
```

## Ex 8.10

This gives the estimates for prediction error for the cubic model $18.17756$, which is larger than the MSPE for the quadratic model $(17.85248)$ seen in the Example.The quadratic model remains the best of the candidate group. 


```r
attach(ironslag)

n <- length(magnetic)
e5 <- numeric(n)

for (k in 1:n) {
  y <- magnetic[-k]
  x <- chemical[-k]
  J5 <- lm(y ~ x + I(x ^ 2) + I(x ^ 3))
  e5[k] <- magnetic[k] - predict(J5, newdata = data.frame(x = chemical[k]))
} 
mean(e5 ^ 2) 
```

```
## [1] 18.17756
```

To compare these results with another model selection criterion such as maximizing the adjusted coefficient of determination, $R^2_{adj}$, from which we see the quadratic model has the highest $R^2_{adj}$ and would again be preferred. 


```r
y = magnetic
x = chemical
L1 = lm(y ~ x)
L2 = lm(y ~ x + I(x ^ 2))
L3 = lm(log(y) ~ x)
L5 = lm(y ~ x + I(x ^ 2) + I(x ^ 3))
cbind(
  linear = summary(L1)$adj.r.squared,
  quadratic = summary(L2)$adj.r.squared,
  exponential = summary(L3)$adj.r.squared,
  cubic = summary(L5)$adj.r.squared
) 
```

```
##         linear quadratic exponential     cubic
## [1,] 0.5281545 0.5768151   0.5280556 0.5740396
```

## Ex 8.11

According to the estimates for prediction error, the quadratic model is best fit for data. Then the exponential model, then the linear model, and the Log-log model is the worst.


```r
n <- length(magnetic) 
e1 <- e2 <- e3 <- e4 <- numeric(n*(n-1)) # 'leave two out' has n(n-1) combinations

for (i in 1:n){
  for (j in i:n){
    if (i != j){
      y=magnetic[c(-i,-j)]
      x=chemical[c(-i,-j)]
      
      J1 <- lm(y ~ x)
      yhat11 <- J1$coef[1] + J1$coef[2] * chemical[i]
      yhat12 <- J1$coef[1] + J1$coef[2] * chemical[j]
      e1[(i-1)*n+j] <- sqrt((magnetic[i] - yhat11)^2+(magnetic[j] - yhat12)^2)
      
      J2 <- lm(y ~ x + I(x^2))
      yhat21 <- J2$coef[1] + J2$coef[2] * chemical[i] + J2$coef[3] * chemical[i]^2
      yhat22 <- J2$coef[1] + J2$coef[2] * chemical[j] + J2$coef[3] * chemical[j]^2
      e2[(i-1)*n+j] <- sqrt((magnetic[i] - yhat21)^2+(magnetic[j] - yhat22)^2)
      
      J3 <- lm(log(y) ~ x)
      logyhat31 <- J3$coef[1] + J3$coef[2] * chemical[i]
      logyhat32 <- J3$coef[1] + J3$coef[2] * chemical[j]
      yhat31 <- exp(logyhat31)
      yhat32 <- exp(logyhat32)
      e3[(i-1)*n+j] <- sqrt((magnetic[i] - yhat31)^2+(magnetic[j] - yhat32)^2)
      
      J4 <- lm(log(y) ~ log(x))
      logyhat41 <- J4$coef[1] + J4$coef[2] * log(chemical[i])
      logyhat42 <- J4$coef[1] + J4$coef[2] * log(chemical[j])
      yhat41 <- exp(logyhat41)
      yhat42 <- exp(logyhat42)
      e4[(i-1)*n+j] <- sqrt((magnetic[i] - yhat41)^2+(magnetic[j] - yhat42)^2)
    }
  }
}
# estimates for prediction error
cbind(linear = mean(e1^2), qaudric = mean(e2^2), exponential = mean(e3^2), loglog = mean(e4^2))
```

```
##        linear  qaudric exponential   loglog
## [1,] 19.57227 17.87018    18.45491 20.46718
```

## Ex 9.4

Implementation the resampling cases method on the MASS::mammals data using the boot function.


```r
stats <- function(dat, i) {
  x <- dat$body[i]
  y <- dat$brain[i]
  Lb <- lm(y ~ x)
  s <- summary(Lb)$sigma
  c(Lb$coeff[1], slope = Lb$coeff[2], s = s)
}
boot.out <- boot(mammals, statistic = stats, R = 2000)
broom::tidy(boot.out)
```

```
## # A tibble: 3 x 4
##   term        statistic    bias std.error
##   <chr>           <dbl>   <dbl>     <dbl>
## 1 (Intercept)    91.0   -16.8      36.4  
## 2 slope.x         0.966   0.193     0.347
## 3 s             335.    -83.8     126.
```

```r
boot.out$t0 # original estimates
```

```
## (Intercept)     slope.x           s 
##  91.0043962   0.9664964 334.7197598
```

## Ex 9.5

Sample code for a statistic function for use with the boot function that will return the MSE for the fitted simple linear regression model.


```r
n <- 2000
stats <- function(dat, i) {
  x <- dat$V1[i]
  y <- dat$V2[i]
  Lb <- lm(y ~ x)
  c(MSE = sum((Lb$residuals) ^ 2) / (n - 2))
}

mu <- c(0, 2)
sigma <- matrix(c(10, 3, 3, 2), nrow = 2, ncol = 2)

random.bvn.dat <- mvrnorm(n = n, mu = mu, Sigma = sigma) %>% as.data.frame()
head(random.bvn.dat)
```

```
##          V1         V2
## 1 -4.235458  0.6051533
## 2 -3.752278 -0.3253357
## 3  4.470354  3.2614245
## 4  2.605278  3.2296856
## 5 -1.092753  0.9725714
## 6 -1.261519  1.1748607
```

```r
# using boot
boot.out <- boot(random.bvn.dat, statistic = stats, R = 2000)
broom::tidy(boot.out)
```

```
## # A tibble: 1 x 4
##   term  statistic     bias std.error
##   <chr>     <dbl>    <dbl>     <dbl>
## 1 MSE        1.11 -0.00177    0.0354
```

```r
# using replicate
rep.out <- replicate(2000, expr = {
  i <- sample(1:n, replace = TRUE, size = n)
  stats(random.bvn.dat, i)
})
mean(rep.out)
```

```
## [1] 1.111163
```

## Ex 9.6

Using the jackknife-after-bootstrap to identify points which are influential.


```r
# previous function, colnames are changed
stats <- function(dat, i) {
  x <- dat$body[i]
  y <- dat$brain[i]
  Lb <- lm(y ~ x)
  n <- length(i)
  c(MSE = sum((Lb$residuals) ^ 2) / (n - 2))
}

boot.out <- boot(mammals, statistic = stats, R = 2000)
broom::tidy(boot.out)
```

```
## # A tibble: 1 x 4
##   term  statistic    bias std.error
##   <chr>     <dbl>   <dbl>     <dbl>
## 1 MSE     112037. -32589.    69678.
```

```r
# Jackknife-after-Bootstrap
infl.out <- empinf(boot.out, type = "jack")
infl.out
```

```
##  [1] -113229.51 -109765.64 -108377.45 -101324.20 -115789.15 -115827.31
##  [7] -115781.62 -107964.41 -114391.25 -108233.95 -107856.17 -108021.79
## [13] -108170.24 -107151.05 -107304.58 -108487.35 -108996.72 -107993.92
## [19] 4868154.39 -107178.28  -93076.63 -111986.82 -107642.08 -115619.22
## [25] -110886.94 -108021.00 -109940.36 -109473.52 -101954.42  -91599.94
## [31] -109200.02 1320944.96 4767119.31 -107224.17 -108863.01 -110853.72
## [37] -109450.17 -107293.60 -107197.35 -107171.14 -109128.32  -89808.72
## [43] -108880.37 -114866.50 -114847.54  -22108.62 -109388.01 -107479.21
## [49] -110974.98 -110156.30 -112543.20 -107434.31 -107339.69 -107668.15
## [55] -107179.40 -106034.88 -110842.09 -109662.04 -107453.29 -108906.04
## [61] -107578.45 -113734.10
```
