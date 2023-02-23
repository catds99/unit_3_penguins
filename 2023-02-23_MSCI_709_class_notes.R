# 2023-02-22
# CDS

#linear regression
#there are lots of assumptions, take them with a  grain of salt
#we'll use these:
  #linear relationship
  #normality of model residuals (residuals = predicted y - true y, how good is model at predicting the actual value)
    #can't check this until after you've run the model
  #no/little multicolinearity (if you have multiple dependent variables that are tightly correlated, 
    #that can cause trouble for explaining how each of them can independently be used to predict y)  
  #no auto-correlation (independent samples) (not taking multiple samples from same subject)
    #this can be a problem with time series data
  #homoscedasticity (homogeneity of variance in residuals across independent variables)
    #again looking at residuals, does the value of the residual change across different values of y?
    # maybe you have high residuals with shorter beaks, but low residuals with longer beaks - 
      #the model is better at predicting y with longer beaks than shorter beaks - we don't want this!

#don't have to follow assumptions to a t, but it's helpful to check the assumptions to make sure the model is the best fit

#and if you don't meet the assumptions, there are other things you can do - transform data, use a different model

library(palmerpenguins)
library(tidyverse)
library(GGally)
library(broom)

#today we are looking at bill depth as a function of length (depth~length)

head(penguins)

penguins %>%
  select(species, bill_depth_mm, bill_length_mm, flipper_length_mm, body_mass_g) %>%
  GGally :: ggpairs(aes(color = species))

penguins %>%
  select(bill_depth_mm, bill_length_mm) %>%
  GGally :: ggpairs()

# linear model

lm_1 = lm(bill_depth_mm ~ bill_length_mm, data = penguins)
class(lm_1)
#class is "lm" r has a specific class for linear models

summary(lm_1)
#we want the coefficients table here, this is what you'd want in your paper
#small p-values means our y intercept and slope are significant

#but just becuase we have a small p-value it doesn't mean we have a good discovery 
    #remember we've combined the three species so this doesn't actually tell us much
#take a look ar the r^2 and adjusted r^2 - 0.05 this means that 5% of variation comes from our independent variable, this isn't loking very good

ggplot(data = penguins, aes(x= bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

plot(lm_1)
#first shows residuals as a functino of y, then qq plot, then the square root of the residuals as a function of y, then residuals vs leverage (which is how much weight each point has on the estimate of model parameters)
#you want residuals vs leverage to start wide then narrow, with the most weighted point being at residual = 0)
#check the point with high leverage 

#let's do a better linear model now, looking at just one penguins species

gentoo = penguins %>%
  filter(species == "Gentoo")

gentoo %>%
  select(bill_depth_mm, bill_length_mm) %>%
  GGally :: ggpairs()

lm_2 = lm(bill_depth_mm ~ bill_length_mm, data = gentoo)

summary(lm_2)

plot(lm_2)
#homoscedasticity in first plot = okay
#qq plot is pretty fine
#square root of residuals - smoothing line is pretty flat, again homoscedasticity looks okay
#leverage plot looks fine

ggplot(data = gentoo, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

#OR could do

ggplot() +
  geom_point(data = gentoo, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_smooth(data = gentoo, aes(x = bill_length_mm, y = bill_depth_mm), method = "lm")

#could also do

ggplot(data = gentoo, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_point(data = penguins %>% filter(species == "Adelie"), 
             aes(x = bill_length_mm, y = bill_depth_mm), color = "red")

#gentoo points will be plotted in black (default, adelie will be added as red points)

#now lets put models for each penguin separately, but on the same plot as all of the penguins together:

ggplot(data = penguins) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_smooth(aes(x = bill_length_mm, y = bill_depth_mm, color = species), method = "lm") +
  geom_smooth(aes(x = bill_length_mm, y = bill_depth_mm), color = "black", method = "lm") +
  theme_bw()

#Simpsons paradox - when you group things together that shouldn't be grouped, you can get the opposite relationship of what it should be  


#Exercise:
#Build a linear model predicting Gentoo bill depth as a function of flipper length. 
#Plot the predictions. 
#Which explanatory variable (bill length vs. flipper length) does a better job of predicting bill depth? 
#What is your evidence?

lm_3 = lm(bill_depth_mm ~ flipper_length_mm, data = gentoo)

lm_3

summary(lm_3)

ggplot(data = gentoo, aes(x = flipper_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

plot(lm_3)

lm_4 = lm(bill_depth_mm ~ bill_length_mm, data = gentoo)

summary(lm_4)

ggplot(data = gentoo, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

#which explains the relationship better

#r^2 for flipper length is 0.49, for bill length it's 0.41, so flipper length is probably better