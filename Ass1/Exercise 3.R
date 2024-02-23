library(lme4)

data <- read.table("cream-1.txt", header=T)
data$batch <- as.factor(data$batch)
data$position <- as.factor(data$position)
data$starter <- as.factor(data$starter)

# a)
aciditylm = lm(acidity ~ batch + position + starter, data)
anova(aciditylm)
summary(aciditylm)


# b)
aciditylm = lm(acidity ~ batch + starter, data)
anova(aciditylm)
summary(aciditylm)


# c)
friedman.test(acidity ~ starter | batch, data)


# d)
lm1 = lmer(acidity ~ starter + (1|batch) + (1|position), data)
lm2 = lmer(acidity ~ (1|batch) + (1|position), data)
anova(lm1, lm2)
summary(lm1)
par(mfrow=c(1,))
plot(fitted(lm1), residuals(lm1))
