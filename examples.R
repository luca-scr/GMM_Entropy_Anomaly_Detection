# install.packages("mclust")
library(mclust)
colors = mclust.options("classPlotColors")
symbols = mclust.options("classPlotSymbols")
library(MASS)

# Simulated example 1: Gaussian distribution with outliers ----

set.seed(20230127)
data = mvrnorm(n = 200, mu = c(15, 15), 
               Sigma = matrix(c(1, 0.5, 0.5, 1.0), nrow = 2, ncol = 2))
noise = rbind(c(5,24), c(13,10), c(19,19), c(23,16), c(22,5))

x = cbind(x1 = c(data[,1], noise[,1]),
          x2 = c(data[,2], noise[,2]))
cluster = as.character(rep(c(1,0), c(nrow(data), nrow(noise))))
clPairs(x, cluster, colors = c("black", colors), symbols = c(3, symbols))

mod = Mclust(x)
summary(mod)
plot(mod, what = "classification")

h = -log(as.densityMclust(mod)$density)/mod$n
(H = sum(h))  # entropy

df = data.frame(pp = ppoints(length(h)),
                hi = sort(h))
plot(hi ~ pp, data = df)
abline(h = log(hypvol(mod$data))/mod$n, lty = 2, col = "indianred")

noise = ifelse(h > log(hypvol(mod$data))/mod$n, "outlier", "inlier")
clPairs(x, noise, colors = "black", symbols = c(1,3))

modNoise = Mclust(x, initialization = list(noise = (noise == "outlier")))
summary(modNoise)
plot(modNoise, what = "classification")


# Simulated example 2: Three-component Gaussian mixture with uniform random noise ----

set.seed(20230209)
n = 200
pro = c(1/3, 1/3, 1/3)
mean = cbind(c(0, 3), c(3, 0), c(-3, 0))
Sigma = array(c(2, 0.5, 0.5, 0.5,
                1, 0, 0, 0.1,
                2, -1, -1, 1),
              dim = c(2, 2, 3))
cl = sample(1:3, size = 200, prob = pro, replace = TRUE)
data = matrix(as.double(NA), nrow = n, ncol = 2)
for(k in 1:3)
  data[cl==k,] = MASS::mvrnorm(sum(cl==k), mu = mean[,k], Sigma = Sigma[,,k])
noise = matrix(runif(100, -10, 10), nrow = 50, ncol = 2)

x = cbind(x1 = c(data[,1], noise[,1]),
          x2 = c(data[,2], noise[,2]))
cluster = as.character(c(cl, rep(0, 50)))
clPairs(x, cluster, colors = c("black", colors), symbols = c(3, symbols))

mod = Mclust(x)
summary(mod$BIC)
plot(mod, what = "classification")

h = -log(as.densityMclust(mod)$density)/mod$n
(H = sum(h))  # entropy

df = data.frame(pp = ppoints(length(h)),
                hi = sort(h))
plot(hi ~ pp, data = df)
abline(h = log(hypvol(mod$data))/mod$n, lty = 2, col = "indianred")

noise = ifelse(h > log(hypvol(mod$data))/mod$n, "outlier", "inlier")
clPairs(x, noise, colors = "black", symbols = c(1,3))

modNoise = Mclust(x, initialization = list(noise = (noise == "outlier")))
summary(modNoise)
plot(modNoise, what = "classification")


# Wisconsin diagnostic breast cancer data example ----

data("wdbc", package = "mclust")
# help("wdbc", package = "mclust")
X = wdbc[, c("Texture_mean", "Area_extreme", "Smoothness_extreme")]
Diagnosis = wdbc[, "Diagnosis"]
table(Diagnosis)

ICL = mclustICL(X)
summary(ICL)
mod = Mclust(X, modelNames = "VVE", G = 2)
summary(mod)
plot(mod, what = "classification")

h = -log(as.densityMclust(mod)$density)/mod$n
(H = sum(h))  # entropy

df = data.frame(pp = ppoints(length(h)),
                hi = sort(h))
plot(hi ~ pp, data = df)
abline(h = log(hypvol(mod$data))/mod$n, lty = 2, col = "indianred")

noise = ifelse(h > log(hypvol(mod$data))/mod$n, "outlier", "inlier")
clPairs(X, noise, colors = "black", symbols = c(1,3))

modNoise = Mclust(X, initialization = list(noise = (noise == "outlier")))
summary(modNoise)
plot(modNoise, what = "classification")



