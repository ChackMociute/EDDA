# Exercise 1
data <- read.csv("Ice_cream-1.csv")

# a)
summary(data$video)

par(mfrow=c(1, 2))
hist(data$video)
qqnorm(data$video)
shapiro.test(rnorm(1000))

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
binom.test(sum(data$video>50), length(data$video), alternative='g')
wilcox.test(data$video, mu=50, alternative='g')

sum(data$video<42)/length(data$video) <= 0.25


# d)
# Bootstrap test
B = 10000
t = min(data$video)
Tstar = numeric(B)
means = NULL
for(m in 0:100){
  for(i in 1:B)
    Tstar[i] = min(rnorm(length(data$video), mean=m, sd=10))
  if(2*min(sum(Tstar<t)/B, sum(Tstar>t)/B) > 0.05)
    means = c(means, m)
}
length(min(means):max(means)) == length(means)
range(means)

# Kolmogorov-Smirnov test
means = NULL
for(m in 0:100){
  if(ks.test(data$video, pnorm, m, 10)[[2]] > 0.05)
    means = c(means, m)
}
length(min(means):max(means)) == length(means)
range(means)

# More precise KS test
means = NULL
for(m in seq(51, 54, by=0.001)){
  if(ks.test(data$video, pnorm, m, 10)[[2]] > 0.05)
    means = c(means, m)
}
length(seq(min(means), max(means), by=0.001)) == length(means)
range(means)


# e)
fvideo = data$video[data$female == 1]
mvideo = data$video[data$female == 0]

t.test(mvideo, fvideo, alternative='g')
wilcox.test(mvideo, fvideo, alternative='g')
ks.test(mvideo, fvideo, alternative='l')

par(mfrow=c(2, 2))
qqnorm(fvideo); qqnorm(mvideo)
hist(fvideo); hist(mvideo)
shapiro.test(fvideo); shapiro.test(mvideo)


# f)
cor.test(data$video, data$puzzle)
cor.test(data$video, data$puzzle, method='spearman')

qqnorm(data$video); qqnorm(data$puzzle)
hist(data$video); hist(data$puzzle)
shapiro.test(data$video); shapiro.test(data$puzzle)
