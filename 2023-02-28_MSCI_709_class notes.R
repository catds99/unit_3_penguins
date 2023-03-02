# 2023-02-28
# CDS

# multiple regression

# so we've spent time creating models, but now we want to use them to make predictions

#categorical variables = factor, nominal, discrete
  #in r we assign these as a factor variable - as.factor(penguins$year) to change it from numerical to a factor

library(palmerpenguins)
library(tidyverse)
library(ggiraph)
library(ggiraphExtra)

head(penguins)

#how does bill length vary with bill depth and species?

penguins_lm_3 = penguins %>%
  filter(!is.na(bill_depth_mm),
         !is.na(bill_length_mm),
         !is.na(species))

head(penguins_lm_3)
dim(penguins_lm_3)

#now let's build the model:

lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data = penguins_lm_3)
class(lm_3)
summary(lm_3)

#all of our p-values are significant, so all of our variables are important for the model

coef(lm_3) #gives vector of estimates of coefficients, use this to access them don't type them out
anova(lm_3)
my_results = broom::tidy(lm_3)
my_results$estimate #this is another way to save estimates of coefficients as a vecotr, this is a better way to recall coefficicnets

my_results = broom::tidy(lm_3, conf.int = TRUE, conf.level = 0.95) %>%
  mutate_if(is.numeric, round, 2)
my_results

#now let's visualize this:
#easy way/cheat way:

ggPredict(lm_3, se = TRUE, interactive = TRUE)

#se = TRUE -> includes standard error, interactive = TRUE let's you hover over points in the viewer tab

#let's look at a few more ways to visualize this:

lm_3_predictions = predict(lm_3) #this is giving us predicted bill depth
head(lm_3_predictions) #this will give us the predictions for the first row
head(penguins_lm_3)

lm_3_predictions = predict(lm_3, interval = "confidence", level = 0.95)
head(lm_3_predictions)

#now let's combine this with original data:

penguins_lm_3_predictions = cbind(penguins_lm_3, lm_3_predictions)
head(penguins_lm_3_predictions)

#now we can generate the figure:

ggplot(data = penguins_lm_3_predictions, aes(x = bill_length_mm, 
                                             y = bill_depth_mm, 
                                             color = species)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species, color = NULL), alpha = 0.25) +
  geom_point() +
  geom_line(aes(y = fit)) #this is the fit we just looked at

#in this case the predictions are based just off data given, the fit doesn't extend through the whole graph like the cheat visualization 
  #this is probably better, you don't really want to predict outside of the range of your data
#but if you do:

#generate new data:

newdata_bill_length_mm = seq(min(penguins_lm_3$bill_length_mm), 
                             max(penguins_lm_3$bill_length_mm),
                             by = 0.1)

head(newdata_bill_length_mm)
tail(newdata_bill_length_mm)

newdata = expand.grid(bill_length_mm = newdata_bill_length_mm, 
                      species = unique(penguins_lm_3$species))
head(newdata)
tail(newdata)
summary(newdata)

#now rerun prediction with this new expanded dataset we made:

newdata_predict_lm_3 = cbind(newdata, predict(lm_3, newdata = newdata, interval = "confidence"))
head(newdata_predict_lm_3)

ggplot() +
    geom_point(data = penguins_lm_3, aes(x = bill_length_mm, y = bill_depth_mm, color = species))+
  geom_ribbon(data = newdata_predict_lm_3, aes(ymin = lwr, ymax = upr, x = bill_length_mm, fill = species),
              alpha = 0.25) +
  geom_line(data = newdata_predict_lm_3, aes(y = fit, x = bill_length_mm, color = species))


#tidyverse way to generate predictions
lm_3_predict = lm_3 %>%
  broom::augment(data = penguins_lm_3, se_fit = TRUE, interval = "confidence")

glimpse(lm_3_predict)

ggplot() +
  geom_point(data = penguins_lm_3, aes(x = bill_length_mm, y = bill_depth_mm, color = species))+
  geom_ribbon(data = newdata_predict_lm_3, aes(ymin = .lower, ymax = .upper, x = bill_length_mm, fill = species),
              alpha = 0.25) +
  geom_line(data = lm_3_predict, aes(y = fit, x = bill_length_mm, color = species))

########################################################
#2023-03-02
########################################################

penguins_lm_3 = penguins %>%
  filter(!is.na(bill_depth_mm),
         !is.na(bill_length_mm),
         !is.na(species))

lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data = penguins_lm_3)

summary(lm_3)

lm_3_predict = lm_3 %>%
  broom::augment(data = penguins_lm_3, se_fit = TRUE, interval = "confidence")

glimpse(lm_3_predict)

ggplot() +
  geom_point(data = penguins_lm_3, aes(x = bill_length_mm, y = bill_depth_mm, color = species))+
  geom_ribbon(data = newdata_predict_lm_3, aes(ymin = .lower, ymax = .upper, x = bill_length_mm, fill = species),
              alpha = 0.25) +
  geom_line(data = lm_3_predict, aes(y = fit, x = bill_length_mm, color = species))

#generate new data:

newdata = penguins_lm_3 %>%
  tidyr::expand(bill_length_mm, species) #give us every possible combo of these in this data set
#expand will drop columns you didn't pass
head(newdata)

lm_3_predict = lm_3 %>%
  broom::augment(newdata = newdata, 
                 se_fit = TRUE, 
                 interval = "confidence")

head(lm_3_predict)

#visualize:

ggplot() +
  geom_point(data = penguins_lm_3, aes(x = bill_length_mm, y = bill_depth_mm, color = species))+
  geom_ribbon(data = newdata_predict_lm_3, aes(ymin = .lower, ymax = .upper, x = bill_length_mm, fill = species),
              alpha = 0.25) +
  geom_line(data = lm_3_predict, aes(y = fit, x = bill_length_mm, color = species))

#now we're going to add an interaction term:
#allows the model to vary the slope for each species

lm_4 = lm(bill_depth_mm ~ bill_length_mm + species + bill_length_mm:species, data = penguins_lm_3)
#make bill depth a function of bill length, species, and any interaction between bill length and species
#can also see it written like this:
lm_4 = lm(bill_depth_mm ~ bill_length_mm * species, data = penguins_lm_3)
#this way is shorter (so less room for mistakes), and makes sure it gets at the individual variables and the interaction

summary(lm_4)
#interaction terms are not significant - the slopes for each species are pretty close to parallel, adding interaction is not worth while
#adj r^2 is 0.7662 

summary(lm_3)
#adj r^2 is 0.7669, slightly higher bc a less complex model than lm_4, but really not that different

AIC(lm_3, lm_4)
#AIC is a way to estimate model fitness, smaller AIC is better ("more fit," 2 units of AIC smaller is a significantly better model) 
#lm_3 AIC is 3 units smaller than lm_4 AIC, so lm_3 is def a better fit

step(lm_4)
#pass the most complex model you have and step will check each part of the model to find the least important parts
#gets you to the model with the best fit for the lowest complexity 

best_model = step(lm_4)
summary(best_model)
#in this case the best model is what we already did with lm_3

#these have been good tools for model comparisons

#we're gonna og ahead and keep using lm_4 anyway

lm_4_predict = lm_4 %>%
  broom::augment(interval = "confidence")
head(lm_4_predict)

ggplot(data = lm_4_predict) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_line(aes(y = .fitted, x = bill_length_mm, color = species)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, x = bill_length_mm, fill = species), alpha = 0.25)

# now let's do depth as a function if bill length and flipper length (a second continuous term = now we have two slopes)

library(car) #we want the function vif, a stat to check whether variables are multicolinear

gentoo = penguins %>%
  filter(species == "Gentoo")

lm_gentoo_1 = lm(bill_depth_mm ~ bill_length_mm, data = gentoo)
lm_gentoo_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data = gentoo)
lm_gentoo_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data = gentoo)

summary(lm_gentoo_1)
summary(lm_gentoo_2)
summary(lm_gentoo_3)

AIC(lm_gentoo_1, lm_gentoo_2, lm_gentoo_3)
step(lm_gentoo_3)
#this told us to keep this model, this was the best model, removing variables doesn't make it a better fit

vif(lm_gentoo_3) #variance inflation factor

head(penguins_lm_3)

#if we just plot bill depth vs bill length, that doesn't show the interaction with flipper length and body mass
#so what we can do is visualize bill depth vs bill length for a certain flipper length (maybe the median) and body mass

newdata = gentoo %>%
  select(bill_length_mm) %>%
  mutate(flipper_length_mm = median(gentoo$flipper_length_mm,
                                    na.rm = TRUE),
         body_mass_g = median(gentoo$body_mass_g,
                              na.rm = TRUE))

head(newdata)

lm_gentoo_3_predict = lm_gentoo_3 %>%
  broom::augment(newdata = newdata, interval = "confidence")

head(lm_gentoo_3_predict)

ggplot(data = lm_gentoo_3_predict) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm), data = gentoo) +
  geom_line(aes(y = .fitted, x = bill_length_mm)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, x = bill_length_mm), alpha = 0.25) +
  annotate("text", x = 57, y = 13.5, label = paste0("flipper length = ", 
                                                    median(gentoo$flipper_length_mm, na.rm = TRUE),
                                                    " mm")) +
  annotate("text", x = 57, y = 13.9, label = paste0("body mass = ", 
                                                    median(gentoo$body_mass_g, na.rm = TRUE),
                                                    " g"))
# exercise 5.3 
#Plot the model predictions from our model lm_gentoo_3 
#so that we can see the variation in bill depth vs. flipper length 
#while holding bill length and body mass constant at their medians.

newdata_2 = gentoo %>%
  select(flipper_length_mm) %>%
  mutate(bill_length_mm = median(gentoo$bill_length_mm,
                                    na.rm = TRUE),
         body_mass_g = median(gentoo$body_mass_g,
                              na.rm = TRUE))

head(newdata_2)

lm_gentoo_4 = lm(bill_depth_mm ~ flipper_length_mm + bill_length_mm + body_mass_g, data = gentoo)


lm_gentoo_4_predict = lm_gentoo_4 %>%
  broom::augment(newdata = newdata_2, interval = "confidence")

head(lm_gentoo_4_predict)

ggplot(data = lm_gentoo_4_predict) +
  geom_point(aes(x = flipper_length_mm, y = bill_depth_mm), data = gentoo) +
  geom_line(aes(y = .fitted, x = flipper_length_mm)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, x = flipper_length_mm), alpha = 0.25) +
  annotate("text", x = 225, y = 13.5, label = paste0("bill length = ", 
                                                    median(gentoo$bill_length_mm, na.rm = TRUE),
                                                    " mm")) +
  annotate("text", x = 225, y = 13.9, label = paste0("body mass = ", 
                                                    median(gentoo$body_mass_g, na.rm = TRUE),
                                                    " g")) +
  theme_classic()


###############################################
#ANOVA
###############################################

penguin_lm = lm(body_mass_g ~ species + sex, data = penguins)
anova(penguin_lm)

#could also explicitly make an anova this way:

penguin_ANOVA = aov(body_mass_g ~ species + sex, data = penguins)

summary(penguin_ANOVA)

#this will tell you that species and sex are significant predictors of body mass, but won't tell you what species or sex is heavier
# you can look at mean for sex since there are only two, need to run a posthoc test for species since ther are three

TukeyHSD(penguin_ANOVA)






