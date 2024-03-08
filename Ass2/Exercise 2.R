library(car)
library(glmnet)

data <- read.csv("BirthWeight.csv")
data$lowbwt <- as.factor(data$lowbwt)
data$mage35 <- as.factor(data$mage35)
data$smoker <- as.factor(data$smoker)
potential <- data[,!names(data) %in% c('ID', 'lowbwt', 'mage35', 'smoker')]


# a)
model_all = lm(Birthweight ~ ., data=potential)
cook <- as.data.frame(cooks.distance(model_all))

vif(model_all)


# b)
reduced_model <- model_all
drop = NULL
repeat{
  p_values = summary(reduced_model)$coefficients[,c(0, 4)]
  max_p = max(p_values)
  if(max_p <= 0.05) break
  var = which.max(p_values)
  drop = c(drop, names(var))
  reduced_model <- update(reduced_model, as.formula(paste(". ~ . -", names(var))))
}
summary(reduced_model)
drop

vif(reduced_model); paste("# of outliers: ", sum(cooks.distance(reduced_model)>1))
par(mfrow=c(1, 2))
qqnorm(residuals(reduced_model)); plot(fitted(reduced_model), residuals(reduced_model))


# c)
average_values <- data.frame(Headcirc = mean(data$Headcirc),
                             Gestation = mean(data$Gestation))
predict(reduced_model, newdata = average_values, interval = "confidence")[,]
predict(reduced_model, newdata = average_values, interval = "prediction")[,]


# d)
mse <- function(pred, y){
  return(mean(pred - y)^2)
}

x = as.matrix(potential[,names(potential) != "Birthweight"])
y = potential$Birthweight

df <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(
  NULL, c("lambda.min", "lambda.1se", "step.down"))))

for(i in 1:1000){
  train = sample(1:nrow(x), 0.67*nrow(x))
  x.train = x[train,]; y.train = y[train]
  x.test = x[-train,]; y.test = y[-train]
  
  lasso.model = glmnet(x.train, y.train, alpha=1)
  lasso.cv = cv.glmnet(x.train, y.train, alpha=1, type.measure="mse", nfolds=5)
  df[nrow(df) + 1,] = c(
  mse(predict(lasso.model, s=lasso.cv$lambda.min, newx=x.test), y.test),
  mse(predict(lasso.model, s=lasso.cv$lambda.1se, newx=x.test), y.test),
  mse(predict(lm(reduced_model$call[[2]], data=potential[train,]),
              newdata=potential[-train,]), potential[-train,]$Birthweight))
}
anova(lm(values ~ ind, data=stack(df)))


# e)
uwt <- data
uwt$smoker = factor(uwt$smoker, levels=c(0, 1), labels=c("Non smoker", "Smoker"))
uwt$mage35 = factor(uwt$mage35, levels=c(0, 1), labels=c("Not over 35", "Over 35"))
uwt$lowbwt = as.numeric(uwt$lowbwt) - 1


# Number of underweight babies
tapply(uwt$lowbwt, uwt$smoker, function(x) paste(sum(x), "out of", length(x)))
tapply(uwt$lowbwt, uwt$mage35, function(x) paste(sum(x), "out of", length(x)))

xtabs(lowbwt ~ smoker + mage35, data=uwt)

par(mfrow=c(1, 1))
barplot(xtabs(lowbwt ~ smoker + mage35, data=uwt), main="Low birth weight",
        ylab="Number of underweight babies", legend=unique(uwt$smoker))


# f)
logistic.model = glm(lowbwt ~ Gestation + smoker + mage35, data=data, family="binomial")
summary(logistic.model)


# g)
logistic.smoker = glm(lowbwt ~ Gestation * smoker + mage35, data=data, family="binomial")
summary(logistic.smoker)

logistic.mage35 = glm(lowbwt ~ Gestation * mage35 + smoker, data=data, family="binomial")
summary(logistic.mage35)

anova(logistic.model, logistic.mage35)


# h)
new = data.frame(Gestation=40, mage35=unique(data$mage35),
                 smoker=rep(unique(data$smoker), each=2))
new$pred = predict(logistic.model, newdata=new, type="response")
new


# i)
fisher.test(table(data[,c("smoker", "lowbwt")]))
fisher.test(table(data[,c("lowbwt", "mage35")]))