library(ggplot2)

data <- read.table('fruitflies.txt', header = TRUE)
data$loglongevity <- log(data$longevity)


# a)
gridExtra::grid.arrange(
  ggplot(data, aes(x = activity, y = loglongevity)) + geom_boxplot() +
    labs(title = 'Effect of Sexual Activity on Longevity'),
  ggplot(data, aes(x = thorax, y = loglongevity, color = activity)) + geom_point() +
    labs(title = 'Longevity vs Thorax with Color Coding of Activity'), ncol=2)

loglongaov = lm(loglongevity ~ activity, data=data)
anova(loglongaov)
tapply(data$longevity, data$activity, mean)


# b)
loglongaov = lm(loglongevity ~ thorax + activity, data=data)
anova(loglongaov)
summary(loglongaov)

average_thorax <- mean(data$thorax)
predictions <- data.frame(activity=unique(data$activity), thorax=average_thorax)
rbind(predictions$activity, round(predict(longaov, newdata=predictions), 3))


# c)
gridExtra::grid.arrange(
  ggplot(data, aes(x = thorax, y = loglongevity, color = activity)) +
    geom_point() +
    facet_wrap(~activity) +
    labs(title = 'Scatter Plot of Longevity vs Thorax, Faceted by Activity',
         x = 'Thorax Length',
         y = 'Log longevity') +
    theme_minimal(),
  ggplot(data, aes(x = thorax, y = loglongevity, color = activity)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "lm", se = FALSE, linetype = "dashed", aes(group = 1)) +
    labs(title = 'Interaction Plot of Longevity vs Thorax by Activity',
         x = 'Thorax Length',
         y = 'Log longevity') +
    theme_minimal(), ncol=2)

interaction_model <- lm(loglongevity ~ thorax * activity, data=data)
anova(interaction_model)


# d) with thorax


# e)
longaov = lm(longevity ~ thorax + activity, data=data)
anova(longaov); summary(longaov)

par(mfrow=c(2, 2)); hist(data$longevity); qqnorm(data$longevity)
hist(data$loglongevity); qqnorm(data$loglongevity)
shapiro.test(data$longevity)[2]; shapiro.test(data$loglongevity)[2]

qqnorm(residuals(loglongaov)); plot(fitted(loglongaov), residuals(loglongaov))
qqnorm(residuals(longaov)); plot(fitted(longaov), residuals(longaov))

