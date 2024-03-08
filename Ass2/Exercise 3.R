data <- read.table("awards.txt", header=TRUE)
data$prog <- as.factor(data$prog)


# a)
poisson.model = glm(num_awards ~ prog, data=data, family=poisson)
summary(poisson.model)

# Average number of awards per program
aggregate(num_awards ~ prog, data=data, mean)


# b)
kruskal.test(num_awards ~ prog, data=data)


# c)
poisson.full = glm(num_awards ~ prog*math, data=data, family=poisson)
poisson.nint = glm(num_awards ~ prog + math, data=data, family=poisson)
summary(poisson.full)
summary(poisson.nint)

newdata = data.frame(math=56, prog=factor(1:3))
predict(poisson.nint, newdata=newdata, type="response")
