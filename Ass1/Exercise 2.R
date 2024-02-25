data <- read.table("hemoglobin-1.txt", header=T)
data$rate <- as.factor(data$rate)
data$method <- as.factor(data$method)

# a)
I=4; J=2; N=10
data.frame(fish=sample(1:(N*I*J)), rate=rep(1:I, each=N*J), method=rep(1:J, N*I))


# b)
anova(lm(hemoglobin~rate*method, data=data))


# c)
anova(lm(hemoglobin~rate+method, data=data))
summary(lm(hemoglobin~rate+method, data=data))

mean(data$hemoglobin[data$rate==3])
which.max(aggregate(hemoglobin ~ rate, data=data, mean)$hemoglobin)


# d)
hemoaov = lm(hemoglobin ~ rate, data=data)
anova(hemoaov)


# e)
kruskal.test(hemoglobin ~ rate, data=data)

par(mfrow=c(1, 1)); plot(fitted(hemoaov), residuals(hemoaov))

    