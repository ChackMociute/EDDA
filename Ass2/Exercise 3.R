data <- read.table("awards.txt", header=TRUE)
data$prog <- as.factor(data$prog)

# a)
poisson.model = glm(num_awards ~ prog, data=data, family=poisson)
summary(poisson.model)






