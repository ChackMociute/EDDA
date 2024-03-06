library(ggplot2)

rdata <- read.table('fruitflies.txt', header = TRUE)
data$loglongevity <- log(data$longevity)


# a)
ggplot(data, aes(x = activity, y = loglongevity)) + geom_boxplot() +
  labs(title = 'Effect of Sexual Activity on Longevity')
ggplot(data, aes(x = thorax, y = longevity, color = activity)) + geom_point() +
  labs(title = 'Longevity vs Thorax with Color Coding of Activity')

longaov = lm(loglongevity ~ activity, data=data)
anova(longaov)
tapply(data$longevity, data$activity, mean)


# b)
longaov = lm(loglongevity ~ thorax + activity, data=data)
anova(longaov)
summary(longaov)

average_thorax <- mean(data$thorax)
predictions <- data.frame(activity=unique(data$activity), thorax=average_thorax)
rbind(predictions$activity, round(predict(longaov, newdata=predictions), digits=3))


# c)
ggplot(data, aes(x = thorax, y = longevity, color = activity)) +
  geom_point() +
  facet_wrap(~activity) +
  labs(title = 'Scatter Plot of Longevity vs Thorax, Faceted by Activity',
       x = 'Thorax Length',
       y = 'Longevity') +
  theme_minimal()
ggplot(data, aes(x = thorax, y = longevity, color = activity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", aes(group = 1)) +
  labs(title = 'Interaction Plot of Longevity vs Thorax by Activity',
       x = 'Thorax Length',
       y = 'Longevity') +
  theme_minimal()

cor.test(data$longevity, data$thorax, method='spearman')

interaction_model <- lm(longevity ~ thorax * activity, data=data)
anova(interaction_model)












