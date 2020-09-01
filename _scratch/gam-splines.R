
library(nlme)
library(mgcv)
library(tidyverse)


airquality$i <- seq_len(nrow(airquality))

m <- gam(Temp ~ s(i, bs = "cr"), data = airquality, method = "REML")

basis <- model.matrix(m)[, -1]


matplot(basis)

try <- cbind(airquality, model.matrix(m)) %>%
  select(Temp, i:`s(i).9`) %>%
  pivot_longer(cols = c(-i, -`(Intercept)`, -Temp))

library(lme4)

mm <- lmer(Temp ~ 1 + (0 + value | name), try)

summary(m)
summary(mm)
coef(m)
coef(mm)

smoothCon(s(i, bs = "cr"), data = airquality, 20)


smoothCon()
model.frame(Temp ~ s(Month), data = airquality)

model.matrix(gam(Temp ~ s(Month, k = 4), data = airquality))

sm <- smoothCon(s(Month,k=5),data=airquality,knots=NULL)[[1]]


Rail$r <- as.numeric(as.character(Rail$Rail))

plot(gam(travel ~ s(r, 10), data = Rail))
summary(gam(travel ~ s(r, 10), data = Rail))

Rail$test1 <- predict(gam(travel ~ s(r, k = 6), data = Rail))

Rail$test2 <- predict(gam(travel ~ s(Rail, bs = "re"), data = Rail))
summary(gam(travel ~ s(Rail, bs = "re"), data = Rail))
plot(gam(travel ~ s(Rail, bs = "re"), data = Rail))


?s
ggplot(Rail) +
  aes(x = as.numeric(as.character(Rail)), y = travel) +
  geom_point() +
  geom_point(aes(y = test1), color = "blue", size = 3) +
  geom_point(aes(y = test2), color = "red", size = 3)


default.knots <- function(x,num.knots) {
  if (missing(num.knots))
    num.knots <- max(5,min(floor(length(unique(x))/4),35))
  return(quantile(unique(x),seq(0,1,length=(num.knots+2))[-c(1,(num.knots+2))]))}

x <- airquality$i
y <- airquality$Temp

knots <- default.knots(x)

z <- outer(x, knots, "-")
#Number of obs. X Number of knots
z <- z * (z > 0)
#All negative elements of z set to 0

basis

group <- rep(1, length(x))
#Create the dummy grouping variable
temp.dataframe <- data.frame(x, y)
#Combine the raw data into a data frame
model.data <- groupedData(y~x|group, data=temp.dataframe)
#Create the grouped data object#for the fixed effects
pdIdent(~-1+basis)
library(nlme)
fit <- lme(y~x, random=pdIdent(~basis), data=model.data)
plot(fit)
summary(fit)
anova(fit)
anova(t)

t <- gam(Temp ~ i + s(i, bs = "cr", m = 0), data = airquality, method = "REML")
sigma(t)
coef(t)
coef(fit)
