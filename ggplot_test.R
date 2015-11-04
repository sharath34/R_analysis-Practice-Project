set.seed(1234)
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)), 
                  rating = c(rnorm(200),rnorm(200, mean=.8)))
# View first few rows
head(dat)

library(ggplot2)
ggplot(dat, aes(x=rating)) + geom_histogram(binwidth=.5)


x <- c(1,2,2,2,2,2,3,3,34,4)
plot(x)
