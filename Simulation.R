library(ggplot2)

lambda <- 0.2
n_sim <- 1000
samplesize_n <- 40
set.seed(820)

simdata <- data.frame(replicate(n_sim, mean(rexp(samplesize_n,lambda))))
names(simdata) <- c("x")
head(simdata)

distribution <- ggplot(simdata, aes(x = x)) + 
                geom_histogram(alpha = .20, 
                               binwidth=.3, 
                               colour = "blue", 
                               aes(y = ..density..)) +
  stat_function(fun=dnorm,
                size = 1, 
                args=list(
                  mean=mean(simdata$x), 
                  sd=sd(simdata$x)))+
                  labs(title="Distribution of random exponetial numbers, Lambda=0.2",
                  y="Density") +
                  geom_vline(aes(xintercept=median(x, na.rm=T)), 
                             color="red", 
                             linetype="longdash", 
                             size=1)

print(distribution)

simmean <- mean(simdata$x)
simmean

simsd <- sd(simdata$x)
simsd

simvar <- var(simdata$x)
simvar

#Expected Mean
expmean = (1/lambda)
expmean

#Expected Standard Deviation
expsd = (1/lambda)/sqrt(samplesize_n)
expsd

# Expected Variance
expvar = ((1/lambda)/sqrt(samplesize_n))^2
expvar

# Theoretical vs simulated plot

# plot the histogram
hist(simdata, breaks=50, prob=TRUE,
     main="Distribution of averages with lambda=0.2",
     xlab="")

# Averages density
lines(density(simdata))

# Theoretical Center of distribution
abline(v=1/lambda, col="red")

# Theoretical Averages density 

xfit <- seq(min(simdata), max(simdata), length=100)
yfit <- dnorm(xfit, 
              mean=1/lambda, 
              sd=(1/lambda/sqrt(samplesize_n)))
lines(xfit, yfit, pch=22, col="red", lty=2)

legend('topright', c("Simulation", "Theoretical"), lty=c(1,2), col=c("black", "red"))

# Confidence Interval

Confidenceinterval <- round((simmean +( c(-1, 1) * 1.96 * simsd / sqrt(length(simdata$x))) ),2)
Confidenceinterval
