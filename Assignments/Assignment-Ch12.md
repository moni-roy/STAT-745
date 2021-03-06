---
title: 'Assignment: Ch 12'
author: "Monikrishna Roy"
date: "04/23/2021 (updated: 2021-05-11)"
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




## Ex 12.1


```r
par(mfrow=c(1,2))
n <- 100
x <- rlnorm(n)
nclass <- ceiling(1 + log2(n))
cwidth <- diff(range(x) / nclass)
breaks <- min(x) + cwidth * 0:nclass
h.sturges <- hist(x, breaks = breaks, freq = FALSE, main = "Hist: Sturges")
curve(dlnorm(x), add=TRUE)


# Applying Doane’s correction requires the sample skewness.
skew <- function(x) {
  xbar <- mean(x)
  m3 <- mean((x - xbar) ^ 3)
  m2 <- mean((x - xbar) ^ 2)
  return(m3 / m2 ^ 1.5)
}

# Doane constant
abs_data = abs(skew(x))
se <- sqrt(6 * (n - 2) / ((n + 1) * (n + 3)))
Ke <- log2(1 + abs_data / se)
nclass.Doane <- ceiling(nclass.Sturges(x) + Ke)
cwidth.Doane <- diff(range(x) / nclass.Doane)
breaks.Doane <- min(x) + cwidth.Doane * seq(0, nclass.Doane)
h.Doane <- hist(x, breaks = breaks.Doane, freq = FALSE, main = "Hist: Doane")
curve(dlnorm(x), add=TRUE)
```

<div class="figure" style="text-align: center">
<img src="README_figs/README-12-unnamed-chunk-1-1.png" alt="Histogram of density for a random lognormal sample using Sturges and donane correction."  />
<p class="caption">(\#fig:unnamed-chunk-1)Histogram of density for a random lognormal sample using Sturges and donane correction.</p>
</div>



```r
# number of bins

cbind(Sturges = nclass, Doane = nclass.Doane)
```

```
##      Sturges Doane
## [1,]       8    13
```

```r
# bins
cbind(bins.Sturges = cwidth, bins.Doane = cwidth.Doane)
```

```
##      bins.Sturges bins.Doane
## [1,]     2.403656   1.479173
```

```r
# breaks length
cbind(breaks.Struge = length(breaks), breaks.Doane = length(breaks.Doane))
```

```
##      breaks.Struge breaks.Doane
## [1,]             9           14
```

```r
# Counts Struges
h.sturges$counts
```

```
## [1] 83 11  4  1  0  0  0  1
```

```r
# counts Doane
h.Doane$counts
```

```
##  [1] 69 18  6  5  0  1  0  0  0  0  0  0  1
```

```r
# The deciles of the standard lognormal
decile.true = qlnorm( seq(.1,.9,.1) )
round(decile.true, 2 )
```

```
## [1] 0.28 0.43 0.59 0.78 1.00 1.29 1.69 2.32 3.60
```

```r
# df(d)
f.true10 = dlnorm( decile.true )
round(f.true10, 2 )
```

```
## [1] 0.63 0.65 0.59 0.50 0.40 0.30 0.21 0.12 0.05
```

```r
# Struges breaks
round(h.sturges$breaks, 1)
```

```
## [1]  0.1  2.5  4.9  7.3  9.7 12.1 14.5 16.9 19.3
```

```r
# Doane breaks
round(h.Doane$breaks, 1)
```

```
##  [1]  0.1  1.6  3.1  4.6  6.0  7.5  9.0 10.5 12.0 13.4 14.9 16.4 17.9 19.3
```

```r
# Sturges density
round(h.sturges$density, 2)
```

```
## [1] 0.35 0.05 0.02 0.00 0.00 0.00 0.00 0.00
```

```r
# Doane density
round(h.Doane$density, 2)
```

```
##  [1] 0.47 0.12 0.04 0.03 0.00 0.01 0.00 0.00 0.00 0.00 0.00 0.00 0.01
```
Noticed that for the Sturges breaks, all of the true deciles lie below the first break point except $d9 = 3.60 > 2.5$. Thus the Struges histogram will estimate every value of $f(d_q)$ as the same number: the height of the first class intervals bar.

Similarly, for the Doane breaks all true deciles but the last three $ (1.69, 2.32, 3.60) > 1.6$, lie below the first class interval’s break at $x = 2.16$. Thus the Doane histogram will estimate every value of $f(dq)$, $q = 1,...,6$ , as the same number: the height of the first class interval’s bar $.47$. Thus, these histograms provide poor estimates of the true density near x = 0



## Ex 12.4

R code for a frequency polygon density estimation for the precip dataset.


```r
n <- length(precip)
# freq poly bin width using normal ref rule
h <- 2.15 * (stats::IQR(precip) / 1.348) * n ^ (-1 / 5)

# calculate the sequence of breaks and histogram
br <- pretty(precip, diff(range(precip)) / h)
brplus <- c(min(br) - h, max(br + h))
histg <- hist( precip, breaks = br, freq = FALSE, main = "", xlim = brplus)

vx <- histg$mids # density est at vertices of polygon
vy <- histg$density

delta <- diff(vx)[1] # h after pretty is applied
k <- length(vx)

vx <- vx + delta # the bins on the ends
vx <- c(vx[1] - 2 * delta, vx[1] - delta, vx)
vy <- c(0, vy, 0)

# add the polygon to the histogram
polygon(vx, vy)
```

![](README_figs/README-12-unnamed-chunk-3-1.png)<!-- -->

```r
# check estimates by numerical integration
fpoly <- approxfun(vx, vy)
print(integrate(fpoly, lower=min(vx), upper=max(vx)))
```

```
## 1 with absolute error < 1.1e-14
```

## Ex 12.6

An ASH density estimation for the faithful$eruptions dataset in R, using width h determined by the normal reference rule.


```r
eruptions <- faithful$eruptions
n <- length(eruptions)

m <- 20
a <- min(eruptions) - .5
b <- max(eruptions) + .5
h <- 2.576 * sqrt(var(eruptions)) * n ^ (-1 / 5)
delta <- h / m
#get the bin counts on the delta-width mesh.
br <- seq(a - delta * m, b + 2 * delta * m, delta)
histg <- hist(eruptions, breaks = br, plot = FALSE)
nk <- histg$counts
K <- abs((1 - m):(m - 1))

fhat <- function(x) {
  # locate the leftmost interval containing x
  i <- max(which(x > br))
  k <- (i - m + 1):(i + m - 1)
  # get the 2m-1 bin counts centered at x
  vk <- nk[k]
  sum((1 - K / m) * vk) / (n * h) #f.hat
  sum((15 / 16) * (1 - (K / m) ^ 2) ^ 2 * vk) / (n * h)
}

# density can be computed at any points in range of data
z <- as.matrix(seq(a, b + h, .1))
f.ash <- apply(z, 1, fhat) #density estimates at midpts

# plot ASH density estimate over histogram
br2 <- seq(a, b + h, h)
hist(
  eruptions,
  breaks = br2,
  freq = FALSE,
  main = "",
  ylim = c(0, max(f.ash))
)
lines(z, f.ash, xlab = "eruptions")
```

<div class="figure" style="text-align: center">
<img src="README_figs/README-12-unnamed-chunk-4-1.png" alt=" ASH density estimate of the eruptions faithful dataset."  />
<p class="caption">(\#fig:unnamed-chunk-4) ASH density estimate of the eruptions faithful dataset.</p>
</div>

## Ex 12.8

For kernel density estimates (KDEs) in the R density() function, two recommended bandwidth algorithms are the default (bw='nrd0', Silverman’s rule), and the SheatherJones option (bw='SJ'). Implement these with the Gaussian kernel ans biweight kernel.


```r
library( gss )
data( buffalo ) 
buffalo.dens0 = density( buffalo, bw='nrd0', kernel='gaussian')
buffalo.dens0SJ = density( buffalo, bw='SJ', kernel='gaussian') 
buffalo.densBiw = density( buffalo, bw='nrd0', kernel='biweight')
buffalo.densBiwSJ = density( buffalo, bw='SJ', kernel='biweight')
par( mfrow= c(2,1) )
plot( buffalo.dens0, xlim=c(0,150), ylim=c(0,0.018) )
plot( buffalo.dens0SJ, xlim=c(0,150), ylim=c(0,0.018)) 
```

<div class="figure" style="text-align: center">
<img src="README_figs/README-12-unnamed-chunk-5-1.png" alt="Kernel density estimates of buffalo data using density with different bandwidths using Gaussian-based KDEs."  />
<p class="caption">(\#fig:unnamed-chunk-5)Kernel density estimates of buffalo data using density with different bandwidths using Gaussian-based KDEs.</p>
</div>


```r
par( mfrow= c(2,1) )
plot( buffalo.densBiw, xlim=c(0,150), ylim=c(0,0.018) )
plot( buffalo.densBiwSJ, xlim=c(0,150), ylim=c(0,0.018) )
```

<div class="figure" style="text-align: center">
<img src="README_figs/README-12-unnamed-chunk-6-1.png" alt="Kernel density estimates of buffalo data using density with different bandwidths using biweight-based KDEs."  />
<p class="caption">(\#fig:unnamed-chunk-6)Kernel density estimates of buffalo data using density with different bandwidths using biweight-based KDEs.</p>
</div>

Little distinguishability is evidenced among the plots. (Moving to more extreme choices for the bandwidth would give clear differences, although the resulting KDEs may not be very useful.) 
