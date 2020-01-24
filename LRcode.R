# LINEAL REGRESSION & MULTIPLE LINEAL REGRESSION BASICS

# installing packages
library(tidyverse)
install.packages("datasets")
install.packages("GGally")
install.packages("scatterplot3d")
library(scatterplot3d)

# trees dataset
data("trees")
head(trees)

# we need to know wich variable is strongly related to volume
GGally::ggpairs(trees, 1:3) # we can do this
pairs(trees) # or this
# hypothesis : volume & girth are strongly related

# modeling
model1 <- lm(Volume ~ Girth, trees)
summary(model1) # is the hypothesis supported ? does the model fit well the data ?
# to answer these questions look at p-value , r-squared , residuals etc
# this also helps to answer the second question : 
# ggplot(trees, aes(model1$residuals))+geom_histogram(binwidth = 1)

# how the model does fit in a plot
ggplot(trees, aes(Girth, Volume))+geom_point()+stat_smooth(method = "lm" , se = FALSE)

# predicting
predict(model1 , data.frame(Girth = 18.2))
# it should have returned 46 but instead returned 55
# it´s not a bad prediction because it was relativily close
# but we need to improve it

###########################################################################################

# multiple linear regression
# if we use 2 variables (Height & Girth) instead of only Girth
# maybe we are gonna be able to make better predictions

# create another model
model2 <- lm(Volume ~ Girth + Height , trees)
summary(model2) # the model fits well the data

# creating vectors to make a 3d scatterplot
Girth_v <- seq(10,20, by=0.5) # girth vector
Height_v <- seq(60,90, by=0.5) #height vector
pred_grid <- expand.grid(Girth = Girth_v , Height = Height_v) # make a grid using the vectors
# predictions for volume based on the predictor variable grid
pred_grid$Volume2 <-predict(model2, new = pred_grid)
# 3d scatterplot
model2_3d <- scatterplot3d(pred_grid$Girth, pred_grid$Height, pred_grid$Volume2, angle = 60, color = "dodgerblue", pch = 1, ylab = "Hight (ft)", xlab = "Girth (in)", zlab = "Volume (ft3)" )
# print 3d scatterplot
model2_3d$points3d(trees$Girth, trees$Height, trees$Volume, pch=16)


# make predictions with the new model
predict(model2 , data.frame(Girth = 18.2, Height = 72))
# it should have return 46 instead of 52
# but the good news are that this time the result was closer
# that means that we improved

# model 3
model3 <- lm(Volume ~ Girth * Height, trees)
# this represents the interaction between 2 variables
# a * b is a shorthand for a + b + a*b
summary(model3) # the model fits well the data

# to see model 3 in 3d scatterplot
pred_grid$Volume3 <-predict(model3 , new = pred_grid)
model3_3d <- scatterplot3d(pred_grid$Girth, pred_grid$Height, pred_grid$Volume3, angle = 60, color = "dodgerblue", pch = 1, ylab = "Hight (ft)", xlab = "Girth (in)", zlab = "Volume (ft3)")
model3_3d$points3d(trees$Girth, trees$Height, trees$Volume, pch=16)

# and now we make the prediction again
predict(model3 , data.frame(Girth = 18.2, Height = 72))
# it returned 45.8
# really close to 46 !

# we made it !
# This was a step by step lineal regression tutorial
# I hope that this will help you