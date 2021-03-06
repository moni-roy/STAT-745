---
title: 'Assignment: Ch 10'
author: "Monikrishna Roy"
date: "04/09/2021 (updated: 2021-05-11)"
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




## Ex 10.1

Here, $p=0.525$ which is bigger than $\alpha = 0.10$. So, the test is not rejected at significant level $\alpha$. 


```r
attach(chickwts)
x <- sort(weight[feed == "casein"])
y <- sort(weight[feed == "sunflower"])
detach(chickwts)

R <- 999 #number of replicates
z <- c(x, y) #pooled sample
n <- length(x)
N <- length(z)
K <- 1:N
reps <- numeric(R) #storage for replicates
t0 <- ks.test(x, y)$statistic

for (i in 1:R) {
  #generate indices k for the first sample
  k <- sample(K, size = n, replace = FALSE)
  x1 <- z[k]
  y1 <- z[-k] #complement of x1
  reps[i] <- ks.test(x1, y1)$statistic
}

p <- mean(c(t0, reps) >= t0)
p
```

```
## [1] 0.525
```

```r
hist(
  reps,
  main = "",
  freq = FALSE,
  xlab = "D",
  breaks = "scott"
)
points(t0, 0, cex = 1, pch = 16) 
```

<div class="figure" style="text-align: center">
<img src="README_figs/README-10-unnamed-chunk-1-1.png" alt="Permutation distribution of replicates."  />
<p class="caption">(\#fig:unnamed-chunk-1)Permutation distribution of replicates.</p>
</div>

## Ex 10.2

A function to compute the two-sample Cramér–von Mises statistic.


```r
# two-sample Cram´er-von Mises test
cm <- function(x, y) {
  n <- length(x)
  m <- length(y)
  n.v <- numeric(n)
  m.v <- numeric(m)
  N <- m+n
  for (i in 1:n)
    n.v[i] <- (x[i] - i) ** 2
  for (j in 1:m)
    m.v[j] <- (y[j] - j) ** 2
  
  # Test statistic for original dataset
  ((n * sum(n.v) + m * sum(m.v)) / (m * n * N)) - (4 * m * n - 1) / (6 * N)
}
```

## Ex 10.3

The two-sample Cramér-von Mises test for equal distributions as a permutation test using the function (10.14) of previous exercise 10.2.


```r
purmutation_test <- function(x, y) {
  # Test statistic for original dataset
  t_orig <- cm(x, y)
  
  # Calculate test statistic for each permautation samples
  r <- 999 # iteration
  n <- length(x)
  z <- c(x, y)
  N <- length(z)
  t <- numeric(r)
  for (k in 1:r) {
    w <- sample(N, size = n, replace = FALSE)
    x1 <- sort(z[w])
    y1 <- sort(z[-w])
    
    t[k] <- cm(x1, y1)
  }
  
  p <- mean(c(t_orig, t) >= t_orig)
  
  result = list("t_orig" = t_orig, "t" = t, "p" = p)
  return(result)
}

data("chickwts")
x <- with(chickwts, sort(as.vector(weight[feed == "soybean"])))
y <- with(chickwts, sort(as.vector(weight[feed == "linseed"])))
cvm <- purmutation_test(x, y)
cbind(cvm$p, cvm$t_orig)
```

```
##       [,1]     [,2]
## [1,] 0.114 4218.373
```

```r
# plot
histogram(
  c(cvm$t_orig, cvm$t) ,
  type = "density" ,
  xlab = "Cram´er-von Mises test",
  ylab = list(rot = 0),
  panel = function(...) {
    panel.histogram(...)
    panel.abline(v = cvm$t_orig, col = 2, lwd = 2)
  }
)
```

<div class="figure" style="text-align: center">
<img src="README_figs/README-10-unnamed-chunk-3-1.png" alt="The two-sample Cramér-von Mises test for the Data of Example 10.1. Red line indicates the original"  />
<p class="caption">(\#fig:unnamed-chunk-3)The two-sample Cramér-von Mises test for the Data of Example 10.1. Red line indicates the original</p>
</div>


```r
x <- with(chickwts, sort(as.vector(weight[feed == "sunflower"])))
y <- with(chickwts, sort(as.vector(weight[feed == "linseed"])))
cvm <- purmutation_test(x, y)
cbind(cvm$p, cvm$t_orig)
```

```
##       [,1]     [,2]
## [1,] 0.001 6374.229
```

```r
# plot
histogram(
  c(cvm$t_orig, cvm$t) ,
  type = "density" ,
  xlab = "Cram´er-von Mises test",
  ylab = list(rot = 0),
  panel = function(...) {
    panel.histogram(...)
    panel.abline(v = cvm$t_orig, col = 2, lwd = 2)
  }
)
```

<div class="figure" style="text-align: center">
<img src="README_figs/README-10-unnamed-chunk-4-1.png" alt="The two-sample Cramér-von Mises test for the Data of Example 10.2"  />
<p class="caption">(\#fig:unnamed-chunk-4)The two-sample Cramér-von Mises test for the Data of Example 10.2</p>
</div>

## Ex 10.6


```r
# Simulated data
x <- runif(100, -1, 1)
y <- x^2
```

### a

correlation, 
$$\rho(X,Y)=\frac{Cov(X,Y)}{\sigma_X\sigma_Y}=\frac{E[(X-\mu_X)(Y-\mu_Y)]}{\sigma_X\sigma_Y}$$
$$=\frac{E[XY]-E[X]E[Y]}{\sigma_X\sigma_Y}$$

Given, $X \sim Uniform(-1, 1)$ and $Y = X^2$, So, $E[X] = \frac{1}{2}(1-1)=0$, and so $E[XY] = 0$.

Finally, $\rho(X,Y)=0$.

### b

Applying the correlation $t-test$ to the simulated data $(x, y)$ using $cor.test$ to test the null hypothesis.


```r
cor.test(x, y, method="pearson")
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  x and y
## t = -1.6918, df = 98, p-value = 0.09387
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.35318426  0.02892315
## sample estimates:
##       cor 
## -0.168452
```
Here, $\alpha = 0.05$, and the p-vale is $0.09784$ greater-than $\alpha$, so the null hypothesis $\rho = 0$ is not rejected.

### c

Distance covariance test using $dcov.test$ and following the Example $10.14$.


```r
# Using function dcov.test
dcov.test(x, y, R = 1000)
```

```
## 
## 	dCov independence test (permutation test)
## 
## data:  index 1, replicates 1000
## nV^2 = 2.55, p-value = 0.000999
## sample estimates:
##      dCov 
## 0.1596879
```

```r
# As example 10.14
dCov <- function(x, y) {
  x <- as.matrix(x)
  y <- as.matrix(y)
  n <- nrow(x)
  m <- nrow(y)
  if (n != m || n < 2)
    stop("Sample sizes must agree")
  if (!(all(is.finite(c(x, y)))))
    stop("Data contains missing or infinite values")
  Akl <- function(x) {
    d <- as.matrix(dist(x))
    m <- rowMeans(d)
    M <- mean(d)
    a <- sweep(d, 1, m)
    b <- sweep(a, 2, m)
    return(b + M)
  }
  A <- Akl(x)
  B <- Akl(y)
  dCov <- sqrt(mean(A * B))
  dCov
}

ndCov2 <- function(z, ix, dims) {
  # dims contains dimensions of x and y
  p <- dims[1]
  q1 <- p + 1
  d <- p + dims[2]
  x <- z[ , 1:p] # leave x as is
  y <- z[ix, q1:d] # permute rows of y
  return(nrow(z) * dCov(x, y)^2)
}

z <- as.matrix(cbind(x,y))

boot.obj <- boot(data = z, statistic = ndCov2, R = 999,  sim = "permutation", dims = c(1,1))
tb <- c(boot.obj$t0, boot.obj$t)
hist(tb, nclass="scott", xlab="", main="", freq=FALSE)
points(boot.obj$t0, 0, cex=1, pch=16)
```

<div class="figure" style="text-align: center">
<img src="README_figs/README-10-unnamed-chunk-7-1.png" alt="Permutation replicates of dCov."  />
<p class="caption">(\#fig:unnamed-chunk-7)Permutation replicates of dCov.</p>
</div>

```r
mean(tb >= boot.obj$t0)
```

```
## [1] 0.001
```

```r
boot.obj
```

```
## 
## DATA PERMUTATION
## 
## 
## Call:
## boot(data = z, statistic = ndCov2, R = 999, sim = "permutation", 
##     dims = c(1, 1))
## 
## 
## Bootstrap Statistics :
##     original    bias    std. error
## t1* 2.550023 -2.310797   0.1393817
```

The p-vale is $0.001$ less-than $\alpha$, so the null hypothesis of independence is rejected.


### d

* We applied two test on the simulated data. First, the correlation $t-test$ to test null hypothesis. And, the distance covariance test to test the null hypothesis of independence. 

* According to results of the distance covariance test, the null hypothesis of independence is rejected. So, it indicates that $X$ and $Y$ are dependent.
