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




