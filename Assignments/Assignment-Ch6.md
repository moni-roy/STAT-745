---
title: "Assignment Ch 6"
author: "Monikrishna Roy"
date: "03/19/2021 (update: 2021-05-11)"
output: 
  bookdown::html_document2:
    number_sections: FALSE
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




# Exercise 6.1

Code for integral of $sin(t)$ over $0 < t < π/3$. We see that the Monte Carlo estimate (left) is almost similar to the true value (right). 


```r
m <- 10000
t <- runif(m, min = 0, max = pi/3)
theta.hat <- mean(sin(t)) * pi/3
theta.true <- -cos(pi/3) - (-cos(0))

c(theta.hat, theta.true)
```

```
## [1] 0.4958094 0.5000000
```

# Exercise 6.2

Code for computing a Monte Carlo estimate of the standard normal cdf, by generating from the $Uniform(0,x)$ distribution. The comparison between estimated values and pnorm values are almost similar. Also estimated of the variance of your Monte Carlo estimate of $Φ(2)$, and a $95%$ confidence interval for $Φ(2)$.


```r
x <- seq(.1, 2, length = 10)
m <- 10000

g <- function(xi) {
  u <- runif(m, 0, xi)
  xi * exp(-(xi * u) ^ 2 / 2)
}

mc.norm <- function (x) {
  cdf <- numeric(length(x))
  for (i in 1:length(x)) {
    cdf[i] <- mean(g(x[i])) / sqrt(2 * pi) + 0.5
  }
  
  return(data.frame(cdf))
}
# comparison cdf with pnorm
mc <- mc.norm(x)
Phi <- pnorm(x)
print(round(rbind(x, cdf=mc$cdf, Phi), 3))
```

```
##     [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10]
## x   0.10 0.311 0.522 0.733 0.944 1.156 1.367 1.578 1.789 2.000
## cdf 0.54 0.624 0.706 0.779 0.832 0.854 0.840 0.813 0.779 0.750
## Phi 0.54 0.622 0.699 0.768 0.828 0.876 0.914 0.943 0.963 0.977
```

```r
# variance and confidence interval
x <- 2
var <- var(g(x))/m
cdf <-  mean(g(x)) / sqrt(2 * pi) + 0.5

c(cdf, var)
```

```
## [1] 7.480812e-01 4.915664e-05
```

```r
c(cdf - 1.96 * sqrt(var), cdf + 1.96 * sqrt(var))
```

```
## [1] 0.7343393 0.7618232
```

# Exercise 6.3

Code for integral of $e^{–x}$ over $0 < x < 0.5$:  We see that sampling from $U(0,0.5)$ over the exact range of the integral produces roughly the same approximation as sampling from $Exp(1)$, but with far smaller (estimated) variance.


```r
m <- 10000

estimate.unif <- function () {
  x <- runif(m, min = 0, max = .5)
  g <- exp(-x)
  theta.hat = mean(g) * 0.5
  var = (0.5 - 0)^2 * var(g) / m
  return(data.frame(theta.hat, var))
}

estimate.exp <- function () {
  y <- rexp(m, rate = 1)
  var <- var(y < 0.5)/m
  
  theta.star <- mean(y < 0.5)
  return(data.frame(theta.star, var))
}

estimate.unif()
```

```
##   theta.hat          var
## 1 0.3940137 3.230759e-07
```

```r
estimate.exp()
```

```
##   theta.star          var
## 1     0.3949 2.389779e-05
```

# Exercise 6.6

We know $\theta = \int_0^1e^udu = e-1 = 1.1718282$
Set $U \sim U(0,10)$

We wish to compare the variance of the simple approximation for $\theta$ to the antithetic approximation for $\theta$. 

If $U_i \sim$ i.i.d.$U(0,1)$ then for $g(u)=e^u$,

$$
\hat{\theta} = \frac{2}{m}\sum_{i=1}^{m/2} \frac{1}{2}\left(g(U_i)+g(1-U_i) \right) = m^{-1}\sum_{i=1}^{m/2}\left(g(U_i)+g(1-U_i) \right)
$$
Consider first the simple approximation: as the $U_i$ variates are i.i.d., 

$$
Var[\overline{g(U)}] = m^{-2}\sum_{i=1}^{m}Var[g(U_i)] = \frac{Var[e^U]}{m}
$$

From example 6.7, we know,
$Var[e^U] = \frac{e^2-1}{2} - (e-1)^2$, so this gives,
$$
Var[\overline{g(U)}] =  \frac{\frac{e^2-1}{2} - (e-1)^2}{m} = 0.2420351/m
$$


For the antithetic approximation,

$$
Var[\hat{\theta}] = m^{-2}Var[\sum_{i=1}^{m/2} \left(g(U_i) + g(i-U_i) \right)] \\
= m^{-2}\sum_{i=1}^{m/2}\left(Var[g(U_i)] + Var[g(1-U_i)] + 2Cov[g(U_i), g(1-U_i)] \right)
$$

We have seen that the antithetic variables $U_i$ and $1–U_i$ are identically distributed, and so $g(U_i)]$ and $g(1–U_i)$ must also be identically distributed. Hence $Var[g(U_i)] = Var[g(1–U_i)]$ is a constant, say, $Var[g(U)]$. Similarly, $Cov[g(U_i),g(1–U_i)]$ is a constant, say, $Cov[g(U),g(1–U)]$. Thus we can write 

$$
Var[\hat{\theta}] = m^{-2}\sum_{i=1}^{m/2}\left(2Var[g(U_i)] + 2Cov[g(U_i), g(1-U_i)] \right) \\
= m^{-1} \left( Var[g(U_i)] + Cov[g(U_i), g(1-U_i)] \right)
$$

Here we require $Var[g(U)] = Var[eU] = \frac{1}{2}(e–1)(e+1) – (e–1)^2$ , as seen above, and
$Cov[g(U),g(1–U)] = Cov[e^U,e^{1–U}] = E[e^Ue^{1–U}] – E[e^U]E[e^{1–U}] = E[e^Ue^{1–U}] – E^2[e^U]$, because $U$ and $1–U$ are identically distributed. We find
$$
 E[e^Ue^{1–U}] = \int_0^1e^ue^{1-u}du = \int_{0}^{1}e^1 = e
$$
 while clearly $E[eU] = \int_{0}^{1}e^udu = e-1$. Thus $Cov[e^U,e^{1–U}] = e – (e–1)^2$ so,
 $$
 Var[\hat{\theta}] = m^{-1} \left( \frac{1}{2} (e-1)(e+1)-(e-1)^2 + e-(e-1)^2 \right) = (5e-1.5e^2 - 2.5)/m = 0.007824994/m
 $$
 Thus the variance reduction is (the m–1 terms all cancel)
 $100(0.2420356 – 0.007824994)/0.2420356 = 96.76701\%$




# Exercise 6.7
Continuing from Exercise 6.6, sample R code to estimate the analysis is


```r
m <- 10000
u1 <- runif(as.integer(m / 2))
u.anti <- 1 - u1
g.anti <- (exp(u1) + exp(u.anti)) / 2
T.anti <- mean(g.anti)
var.anti <- var(g.anti) / (m / 2)
u <- runif(m)
T0 <- mean(exp(u))
var0 <- var(exp(u)) / m
perc.reduc <- 100 * (var0 - var.anti) / var0

# Monte Carlo(left) and antithetic(right)
theta = data.frame(list("theta Monte Carlo" = T0, "theta antithetic" = T.anti) )
variance = data.frame(list("var Monto carlo" = var0, "var antithetic" = var.anti, "percent" = perc.reduc))
# Compare these to the theoretical values from Exercise 6.6:
varianceTh = data.frame(list("theory var Monto carlo" = 0.2420356/m, "theory var antithetic" = 0.007824994/m, "theory percent" = 96.76701))
theta 
```

```
##   theta.Monte.Carlo theta.antithetic
## 1          1.706716         1.717487
```

```r
variance
```

```
##   var.Monto.carlo var.antithetic  percent
## 1    2.424548e-05   7.709554e-07 96.82021
```

```r
varianceTh
```

```
##   theory.var.Monto.carlo theory.var.antithetic theory.percent
## 1           2.420356e-05          7.824994e-07       96.76701
```

# Exercise 6.12

If $\hat{\theta}^{IS}_f$ is an importance estimator,

$$
\theta = \int g(x)dx = \int \frac{g(x)}{\hat{\theta}^{IS}_f}\hat{\theta}^{IS}_fd(x) = E[\frac{g(x)}{\hat{\theta}^{IS}_f}]
$$
the estimator is again the sample-mean,
$$
\hat{\theta} = \overline{g(x)} = \frac{1}{n} \sum_{i=1}^n \frac{g(X_i)}{\hat{\theta}^{IS}_f}
$$
the importance sampling method is a sample-mean method
$$
Var(\hat{\theta}) = E[\hat{\theta^2}] - (E[\hat{\theta}])^2 = \int \frac{g^2(x)}{\hat{\theta}^{IS}_f}dx - \theta^2
$$
The minimum variance,
$$
\left( \int |g(x)|dx \right)^2 - \theta^2
$$
is obtained when,
$$
\hat{\theta}^{IS}_f = \frac{|g(x)|}{\int |g(x)|dx}
$$
For general $f(x)$, choose $\hat{\theta}^{IS}_f$ so that $\hat{\theta}^{IS}_f \sim |g(x)|f(x)$. If the ratio $g(x)/f(x)$ of the function being integrated to the importance function is bounded, then the importance sampling estimator will have finite variance.

# Exercise 6.13

Two importance functions $f_1$ (rayleigh distribution) and $f_2$ (Normal Distribution) that are supported on $(1, \infty)$. $f_2$ is a little closer to $g(x)$.


```r
g <- function (x) {
  x ^ 2 / sqrt(2 * pi) * exp(-x ^ 2 / 2) * (x > 0)
}

xs <- seq(0,10,0.1)

ys.g <- g(xs)
ys.rayleigh <- drayleigh(xs)
ys.norm = dnorm(xs, mean = 1.5)
lim = max(c(ys.g, ys.rayleigh, ys.norm))

theta = data.frame(list(
  "rayleigh" = mean(ys.rayleigh),
  "norm" = mean(ys.norm)
))

se = data.frame(list(
  "rayleigh" = sd(ys.rayleigh)/sqrt(length(xs)),
  "norm" = sd(ys.norm)/sqrt(length(xs))
))
rbind(theta = theta, se = se)
```

```
##         rayleigh       norm
## theta 0.09892735 0.09302047
## se    0.01846239 0.01374151
```

```r
plot(xs, ys.g, type = "l", ylim = c(0, lim), xlab="X", ylab="")
lines(xs, ys.rayleigh, col="red", ylim = c(0, lim))
lines(xs, ys.norm, col="blue", ylim = c(0, lim))
legend("topright", legend = c("g", "rayleig", "norm"),col = c('black', 'red', 'blue'), lty = 1)
```

<div class="figure" style="text-align: center">
<img src="README_figs/README-6-unnamed-chunk-6-1.png" alt="Importance functions"  />
<p class="caption">(\#fig:unnamed-chunk-6)Importance functions</p>
</div>

# Exercise 6.15

R code for the stratified importance sampling estimate in Example 6.14


```r
m <- 10000
g <- function(x) {
  exp(-x - log(1 + x ^ 2)) * (x >0) * (x < 1)
}
k <- 5
theta <- se <- numeric(k)

for (j in 1:k) {
  u <- runif(m/k, (j-1)/ k, j / k)
  
  x <- -log(1 - u * (1 - exp(-1)))
  fg <- g(x) / (exp(-x) / (1 - exp(-1)))
  theta[j] <- mean(fg)
  se[j] <- sd(fg)
}

rbind(theta = c(estimate = mean(theta), previous = 0.5257801), se = c(estimate = mean(se), previous = 0.0970314))
```

```
##        estimate  previous
## theta 0.5247884 0.5257801
## se    0.0183516 0.0970314
```

