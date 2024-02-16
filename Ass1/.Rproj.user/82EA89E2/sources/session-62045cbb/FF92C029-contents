# Exercise 1
data <- read.csv("Ice_cream-1.csv")

# a)
summary(data$video)

par(mfrow=c(1, 2))
hist(data$video)
qqnorm(data$video)

# CI
alpha = 0.03
ci <- qnorm(1 - alpha/2) * sd(data$video) / sqrt(length(data$video))
c(mean(data$video) - ci, mean(data$video) + ci)
n <- (qnorm(1 - alpha/2) * sd(data$video) / (3/2))^2
ceiling(n)

# Bootstrap CI
B = 100000
Tstar = numeric(B)
for(i in 1:B)
  Tstar[i] = mean(sample(data$video, replace=T))
c(2*mean(Tstar) - quantile(Tstar, 1 - alpha/2), 2*mean(Tstar) - quantile(Tstar, alpha/2))


# b)
t.test(data$video, mu=50, alternative="greater")
t.test(data$video, mu=51, alternative="greater")

# c)



















