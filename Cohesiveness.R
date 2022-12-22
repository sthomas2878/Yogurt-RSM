# RSM Package for modeling
install.packages("rsm")
library(rsm)

# Reading in data
dat <- read.csv("YogurtRSMData.csv", header = TRUE)
attach(dat)

# Creating Second Order model
mod <- rsm(Cohesiveness ~ SO(ST, LB, BB, LA))
summary(mod)

# Residuals
res <- residuals(mod)
plot(res)
qqnorm(res)
qqline(res)

# Reduced Model based on results of original model
modred <- rsm(Cohesiveness ~ FO(ST, LB, BB, LA) + 
                TWI(ST, LB) + TWI(LB, BB) + 
                PQ(ST, LB, BB, LA))
summary(modred)

# Comparison of reduced model and full model
anova(modred, mod)

# Residuals of reduced models
resred <- residuals(modred)
par(mfrow=c(1,1))
plot(resred, main = "Residual Plot", ylab = "Residual")
qqnorm(resred)
qqline(resred)

# Setting colors
colfunc <- colorRampPalette(c("green", "yellow",
                            "orange", "red", "white"))
colfunc(100)

# Contour plots
par(mfrow=c(2,3))
persp(modred, ~ ST + LB + BB + LA, col=colfunc(100),
      at=summary(mod)$canonical$xs)

# 3D Plots
par(mfrow=c(2,3))
contour(modred, ~ ST + LB + BB + LA, 
        at=summary(mod)$canonical$xs, image=TRUE)

# Original Paper Results
ST <- 0.79
LB <- 0.75
BB <- 0.79
LA <- 0.95
0.092 + 0.169*ST + 0.157*LB + 0.054*BB + 
  0.082*LA - 0.030*ST*LB + 0.020*LB*BB - 
  0.040*(ST^2) - 0.045*(LB^2) - 0.025*(BB^2) - 0.025*(LA^2)
