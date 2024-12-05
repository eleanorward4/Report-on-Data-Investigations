library(tidyverse)

## Investigation 1: Rainfall

# To find the mean, variance and standard deviation of the data
m = mean(Rainfall$Excess) #18.889655...
v = var(Rainfall$Excess) #616.1121839...
s = sd(Rainfall$Excess)  #24.8216...

#1a

#inputting alpha and beta
alphaestimate = 0.5791462689
betaestimate = 0.03065944103

#standard error for gamma
alpha <- numeric(10000)
beta <- numeric(10000)
for(i in 1:10000) {
  y <- rgamma(145, alphaestimate, betaestimate)
  alpha[i] <- mean(y)^2 / var(y)
  beta[i] <- mean(y) / var(y)
}
sd(alpha) #0.1080
sd(beta)  #0.0068

# to plot graphs to analyse the realism of the model
#qqplot
qqplot1 = ggplot(Rainfall, aes(sample = Excess)) +
  stat_qq(distribution = qgamma, dparams = list(shape = alphaestimate,
                                                rate = betaestimate)) +
  geom_abline(intercept = 0, slope = 1, colour='red', size=0.75) +
  labs(x = "Gamma Quantiles", y = "Sample Quantiles (mm)", 
       title = "QQ Plot for Excess Rainfall against a Gamma Distribution")
ggsave('qqplot1.jpeg', qqplot1, width=6, height=4, dpi=500)

#histogram
histogram1 = ggplot(Rainfall) +
  geom_histogram(aes(x = Excess, y = ..density..), fill='white', 
                 colour='black', binwidth=4) +
  labs(x = "Excess Rainfall (mm) ", y = "Density", 
       title = "Histogram of the Excess Rainfall") +
  stat_function(geom = "line", fun = dgamma,
                args = list(shape = alphaestimate, rate = betaestimate), 
                colour='red', size=0.75) +
  lims(y=c(0,0.05))
ggsave('histogram1.jpeg', histogram1, width=6, height=4, dpi=500)

#1b

#inputting found values

sigmaestimate=14.91476424
gammaestimate=0.2104268656

#standard error for model
sigma <- numeric(10000)
gamma <- numeric(10000)
for(i in 1:10000) {
  y <- rmodel(145, scale = sigmaestimate, shape = gammaestimate)
  sigma[i] <- (mean(y)/2) + ((mean(y))^3 / (2 * var(y)))
  gamma[i] <- 0.5 - (mean(y))^2 / (2 * var(y))
}
sd(sigma) #1.893835
sd(gamma) #0.0887039

#to plot graphs to analyse realism of the model
#qqplot
qqplot2 = ggplot(Rainfall, aes(sample = Excess)) +
  stat_qq(distribution = qmodel, dparams = list(scale = sigmaestimate,
                                                shape = gammaestimate)) +
  geom_abline(intercept = 0, slope = 1, colour='red', size=0.75) +
  labs(x = "M Quantiles", y = "Sample Quantiles (mm)", 
       title = "QQ Plot for Excess Rainfall against the M Distribution")
ggsave('qqplot2.jpeg', qqplot2, width=6, height=4, dpi=500)

#histogram
histogram2 = ggplot(Rainfall) +
  geom_histogram(aes(x = Excess, y = ..density..), fill='white', 
                 colour='black', binwidth=4) +
  labs(x = "Excess Rainfall (mm) ", y = "Density", 
       title = "Histogram of the Excess Rainfall") +
  stat_function(geom = "line", fun = dmodel,
                args = list(scale = sigmaestimate, shape = gammaestimate), 
                colour='red', size=0.75) +
  lims(y=c(0,0.05))
ggsave('histogram2.jpeg', histogram2, width=6, height=4, dpi=500)

#1c
#to find 10 year return level

pmodel(50, scale = sigmaestimate, shape = gammaestimate) #0.9208835
pmodel(60, scale = sigmaestimate, shape = gammaestimate) #0.9457721
pmodel(70, scale = sigmaestimate, shape = gammaestimate) #0.9617824
pmodel(80, scale = sigmaestimate, shape = gammaestimate) #0.9724114
pmodel(81, scale = sigmaestimate, shape = gammaestimate) #0.9732641

##Investigation 2: Antibiotic
#2a

#to find the mean for the point estimate and standard deviation 
# for the confidence interval
m2 = mean(Antibiotic$Outcome) #0.7347
s2 = sd(Antibiotic$Outcome)   #0.4438

#to find the number of people in the study (n)
Antibiotic %>% 
  summarise(total_number = n()) #98

#2b
#number of people in hospital a and hospital b
Antibiotic %>% 
  group_by(Hospital) %>%
  summarise(total_number = n()) #55 in A and 43 in B

#point estimate of mean for each hospital 
Antibiotic %>% 
  group_by(Hospital) %>%
  summarise(mean = mean(Outcome)) # A = 0.836, B = 0.605

#standard deviation for each hospital for confidence interval
Antibiotic %>% 
  group_by(Hospital) %>%
  summarise(sd = sd(Outcome)) # A = 0.373, B = 0.495