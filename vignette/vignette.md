# Application

This file provides reproduced results from the literature for each provided functionality of the \textit{spINAR} package. 

First, we reproduce the Tables 8, 9 and part of Table 10 published in Jentsch and Weiß (2017) who analyze a time series of length $n=1632$ containing counts concerning the Lufthansa stock traded in the XETRA system of Deutsche Börse. This data set was first analyzed by Jung and Tremayne (2011). For reproducing the results of the stated Tables, we use the functions \texttt{spinar\_est}, \texttt{spinar\_est\_par} and \texttt{spinar\_boot}, where the latter implicitly makes additionally use of \texttt{spinar\_sim}.  

``` {r}
# Load the package
library(spINAR)
```

The following data were read from Figure 3 in Jung and Tremayne (2011).

``` {r}
ice <- c(0, 0, 0, 1, 1, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 2, 1, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 2,
         3, 2, 1, 0, 1, 1, 0, 2, 0, 0, 1, 2, 2, 1, 1, 1, 0, 0, 0, 0, 0, 2, 2, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 2, 1, 1, 2, 2, 2, 1,
         2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 2, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 2, 1, 1,
         0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 2, 0, 5, 1, 2, 0, 0, 0,
         1, 2, 0, 1, 1, 3, 2, 3, 1, 2, 3, 2, 1, 1, 1, 1, 2, 0, 1, 0, 0, 1, 1, 1,
         2, 3, 1, 1, 1, 2, 2, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
         2, 1, 1, 1, 2, 2, 2, 0, 1, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 1, 1, 1, 
         1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 2, 1, 0, 1, 0, 0, 1,
         0, 2, 1, 1, 2, 0, 1, 0, 0, 2, 0, 1, 1, 0, 1, 1, 0, 1, 0, 3, 1, 1, 2, 0,
         0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
         1, 0, 0, 0, 0, 0, 1, 1, 2, 2, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 2,
         2, 2, 2, 2, 0, 2, 2, 0, 2, 2, 2, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 
         1, 1, 1, 1, 0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
         0, 1, 1, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 3, 4, 4, 4, 3, 3, 3, 2, 1, 2, 2,
         2, 2, 1, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0,
         0, 0, 1, 2, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 
         1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 2, 0, 1, 2, 
         2, 1, 1, 1, 0, 2, 1, 1, 2, 3, 3, 2, 1, 1, 2, 1, 1, 0, 1, 1, 2, 3, 2, 2,
         2, 1, 2, 1, 0, 1, 1, 1, 0, 0, 4, 3, 2, 1, 3, 1, 0, 1, 0, 0, 1, 0, 0, 0, 
         1, 0, 0, 0, 1, 1, 2, 2, 3, 3, 2, 2, 1, 1, 1, 1, 2, 1, 0, 0, 0, 0, 0, 1, 
         2, 2, 2, 0, 0, 0, 1, 3, 1, 2, 1, 0, 1, 1, 1, 2, 2, 1, 2, 1, 1, 1, 0, 1, 
         0, 0, 1, 0, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1,
         1, 1, 1, 1, 2, 3, 2, 0, 1, 1, 1, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 1,
         0, 0, 1, 1, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 2, 1, 1, 1, 1,
         1, 0, 1, 1, 2, 2, 2, 1, 1, 0, 1, 0, 1, 0, 1, 2, 3, 2, 1, 0, 1, 0, 0, 0,
         0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
         0, 0, 0, 2, 0, 1, 2, 2, 0, 1, 0, 0, 1, 0, 1, 2, 1, 1, 1, 0, 0, 2, 0, 1,
         1, 3, 2, 2, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 2,
         1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 3, 3, 5,
         1, 2, 2, 2, 2, 3, 2, 3, 3, 2, 3, 3, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1,
         1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 3,
         1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 3, 3, 3, 2, 1, 2, 1, 0, 1, 2, 1, 2, 1, 1,
         1, 2, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 2, 2, 4, 3, 2, 0, 2,
         2, 2, 0, 2, 2, 2, 2, 0, 1, 2, 4, 2, 1, 0, 1, 3, 1, 0, 1, 0, 0, 0, 0, 1,
         1, 0, 1, 0, 1, 1, 2, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 2, 1, 1, 2, 2, 2, 0,
         2, 1, 2, 0, 2, 2, 0, 0, 0, 2, 2, 1, 2, 2, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,
         1, 2, 3, 2, 2, 0, 2, 1, 1, 2, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,
         0, 1, 2, 1, 1, 1, 1, 0, 2, 1, 1, 1, 1, 2, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1,
         1, 0, 0, 0, 0, 0, 1, 2, 1, 2, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 3, 2, 2, 0,
         0, 1, 1, 0, 2, 0, 1, 0, 1, 1, 2, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1,
         0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1,
         0, 0, 0, 1, 1, 0, 1, 2, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 4, 1, 2, 1, 1, 3,
         2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0,
         0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
         0, 0, 0, 0, 1, 1, 1, 1, 4, 0, 2, 2, 3, 1, 1, 1, 1, 2, 0, 3, 3, 2, 1, 1,
         1, 1, 0, 2, 2, 2, 1, 0, 1, 0, 0, 1, 2, 1, 2, 0, 1, 1, 1, 1, 1, 1, 0, 1,
         1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 2, 0,
         0, 2, 0, 4, 2, 2, 0, 1, 3, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1,
         3, 4, 5, 4, 4, 3, 2, 2, 2, 2, 2, 1, 1, 2, 1, 2, 1, 1, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 2, 1, 1, 1, 0, 0, 1,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 2, 0, 0, 0, 1,
         1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 1,
         0, 3, 1, 2, 3, 3, 1, 1, 0, 0, 2, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2,
         0, 0, 0, 1, 1, 1, 1, 1, 1, 3, 1, 1, 2, 0, 0, 2, 1, 1, 2, 3, 2, 7, 3, 6,
         5, 0, 1, 1, 1, 1, 2, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0,
         2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0,
         2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 1, 2, 0, 1, 1, 1, 1, 1, 1, 2,
         0, 1, 2, 1, 1, 2, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 1, 2, 1, 2, 3, 1, 1, 1, 0, 1, 1, 1, 2, 1,
         2, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
         2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 1, 0, 1, 1, 2, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
```

Semi-parametric INAR($p$) estimates for iceberg counts (Table 8):

``` {r}
# p=1
xmax <- max(ice)
est_sp_1 <- spinar_est(ice,1)
mu_e_sp_1 <- sum(est_sp_1[-1]*(0:xmax))
sigma2_e_sp_1 <- sum(est_sp_1[-1]*(0:xmax)^2) - mu_e_sp_1^2
c(est_sp_1, mu_e_sp_1,sigma2_e_sp_1)

# p=2
est_sp_2 <- spinar_est(ice,2)
mu_e_sp_2 <- sum(est_sp_2[-(1:2)]*(0:xmax))
sigma2_e_sp_2 <- sum(est_sp_2[-(1:2)]*(0:xmax)^2) - mu_e_sp_2^2
c(est_sp_1, mu_e_sp_2,sigma2_e_sp_2)
```
Parametric INAR($p$) estimates for iceberg counts (Table 9)

``` {r}
# p=1
est_p_1 <- spinar_est_param(ice,1,"mom","poi")
est_p_1_v <- c(est_p_1[1],dpois(0:xmax,est_p_1[2]))
unname(c(est_p_1_v, est_p_1[2], est_p_1[2]))

# p=2
est_p_2 <- spinar_est_param(ice,2,"mom","poi")
est_p_2_v <- c(est_p_2[1:2],dpois(0:xmax,est_p_2[3]))
unname(c(est_p_2_v, est_p_2[3], est_p_2[3]))
```
For Table 10, we focus on the reproducing of the confidence intervals of the observations' mean.

```{r}
# Setting of parametrizations (Jentsch and Weiß, 2017)
B <- 10^4
level <- 0.05
M <- 10

mu_est <- mean(ice)
```

$95%$ confidence intervals for the observations' mean using the parametric approach

```{r}
# p=1
xstar_p <- spinar_boot(ice, 1, B, "p", "mom", "poi", progress = FALSE)$x_star
mu_est_star <- apply(xstar_p, 2, mean)
mu_est_star_cent <- mu_est_star - mu_est
srt <- sort(mu_est_star_cent)
c(mu_est - quantile(srt,1-level,names=FALSE), mu_est - quantile(srt,level,names=FALSE))

# p=2
xstar_p <- spinar_boot(ice, 2, B, "p", "mom", "poi", progress = FALSE)$x_star
mu_est_star <- apply(xstar_p, 2, mean)
mu_est_star_cent <- mu_est_star - mu_est
srt <- sort(mu_est_star_cent)
c(mu_est - quantile(srt,1-level,names=FALSE), mu_est - quantile(srt,level,names=FALSE))
```

$95%$ confidence intervals for the observations' mean using the semi-parametric approach

```{r eval=FALSE}
# p=1
mu_est_cent <- mu_e_sp_1/(1-est_sp_1[1])
xstar_sp <- spinar_boot(ice, 1, B, "sp", progress = FALSE)$x_star
mu_est_star <- apply(xstar_sp, 2, mean)
mu_est_star_cent <- mu_est_star - mu_est_cent
srt <- sort(mu_est_star_cent)
c(mu_est - quantile(srt,1-level,names=FALSE), mu_est - quantile(srt,level,names=FALSE))

# p=2
mu_est_cent <- mu_e_sp_2/(1-est_sp_2[1]-est_sp_2[2])
xstar_sp <- spinar_boot(ice, 2, B, "sp", progress = FALSE)$x_star
mu_est_star <- apply(xstar_sp, 2, mean)
mu_est_star_cent <- mu_est_star - mu_est_cent
srt <- sort(mu_est_star_cent)
c(mu_est - quantile(srt,1-level,names=FALSE), mu_est - quantile(srt,level,names=FALSE))
```

Next, we reproduce the results of Figure 10 published in Faymonville et al. (2022). They considered a time series of length $n=51$ of the monthly demand of a car spare part from January 1998 to March 2002. The data set was first analyzed in Hyndman (2008) and is available in the R package \textit{expsmooth}. For reproducing, we use the function \texttt{spinar_penal}. A validation of the penalization parameters could be done with \texttt{spinar_penal_val}.

```{r}
# Load the data set
data <- c(1,1,0,2,1,4,4,5,4,0,2,1,0,0,1,2,1,3,1,0,1,2,1,0,0,1,0,1,0,0,2,0,2,2,0,
2,1,0,1,2,1,1,0,0,0,0,0,0,1,2,2)
```

```{r}
# Unpenalized estimation of the innovation distribution 
est_unpenal <- spinar_est(data,1)

# Penalized estimation of the innovation distribution
est_penal <- spinar_penal(data,1,0,1)
```

Plot of the unpenalized and penalized estimated innovation distribution

```{r}
par(mfrow=c(1,2))
barplot(est_unpenal[-1], ylim=c(0,1),names.arg=0:5,
main="Unpenalized estimated \n innovation distribution")
barplot(est_penal[-1], ylim=c(0,1),names.arg=0:5,
main="Penalized estimated \n innovation distribution")
```

![](https://github.com/MFaymon/spINAR/blob/main/vignette/barplots.png)
## References

* Faymonville, M., Jentsch, C., Weiß, C.H. and Aleksandrov, B. (2022). "Semiparametric Estimation of INAR Models using Roughness Penalization". Statistical Methods & Applications. [DOI](https://doi.org/10.1007/s10260-022-00655-0)
* Hyndman, R., Koehler, A., Ord, K. and Snyder, R. (2008). "Forecasting with Exponential Smoothing". Springer. [DOI](https://doi.org/10.1007/978-3-540-71918-2)
* Jentsch, C. and Weiß, C.H. (2017), “Bootstrapping INAR Models”. Bernoulli 25(3), pp. 2359-2408. [DOI](https://doi.org/10.3150/18-BEJ1057)
* Jung, R.C. and Tremayne, A.R. (2011), “Convolution-closed Models for Count Time Series with Applications”. Journal of Time Series Analysis 32, pp. 268-280. [DOI](https://doi.org/10.1111/j.1467-9892.2010.00697.x)

