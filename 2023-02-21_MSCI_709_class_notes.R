# 2023-02-21
# CDS

library(palmerpenguins)
library(tidyverse)
library(rstatix)

head(penguins)

ggplot(data = penguins) +
  geom_histogram(aes(x = body_mass_g, fill = species))

# one-sample t test:

gentoo = penguins %>% #pipe to say please only give me the species that = Gentoo
  filter(species == "Gentoo")
head(gentoo)

ggplot(data = gentoo) +
  geom_histogram(aes(x = body_mass_g))

#assumptions for t-test: data are normally distributed, 
#are data are somewhat normally distributed, but since we have so many samples it's probs ok
#look for outliers make sure you didn't make a mistake

#to look at normal distribution more formally make a qq plot

ggplot(data = gentoo) +
  stat_qq(aes(sample = body_mass_g))

#what you want to see in your qq plot (if your sample is normally distributed) is a line with a slope of 1, 
#ours looks pretty good

gentoo %>%
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm = TRUE),
            sd_body_mass_g = sd(body_mass_g, na.rm = TRUE))

#encyclopedia of life: search for species, click on it, get details about organism, click on data for however many attributes, see if they have data for what you're looking for (body mass in this case)
#use this for the one-sample t-test to get a (literature derived) value to use to compare your mean from your data to

#run t-test:

t.test(gentoo$body_mass_g, mu = 5500) #mu is where you give it the literature derived value (from encyclopedia of life)

#out put:
#One Sample t-test

#data:  gentoo$body_mass_g
#t = -9.3276, df = 122, p-value = 6.051e-16
#alternative hypothesis: true mean is not equal to 5500
#95 percent confidence interval:
 # 4986.034 5165.998
#sample estimates:
 # mean of x 
#5076.016 


#here the t-test has a very small p-value, this means that our mean is very different from the literature derived value 
#our null hypothesis is false, means are not similar --> turn to our alternative hypothesis

#more tidyverse freidnly way to run the t-test:

t_test_results = gentoo %>%
  t_test(body_mass_g ~ 1, mu = 5500) # "~ 1" = "as a function of 1" = instructions that this is a one-sample t-test

t_test_results

#so why are our results different? maybe skewed sex 
#(maybe our data is more males while the encyclopedia of life value is mostly female, or same thing with adults vs juveniles)
#maybe there was different prey availability at the times of the different studies

############################################
# two-sample t-test
############################################

data_for_t_test = penguins %>%
  filter(species %in% c("Gentoo", "Adelie"),
         !is.na(body_mass_g)) %>% #to get rid of NAs
  select(species, body_mass_g) %>% #if you only want to show certain columns
  droplevels() #to drop anything that's no longer represented in your data (chinstraps in this case) 

head(data_for_t_test)
summary(data_for_t_test)

data_for_t_test %>%
  group_by(species) %>% #first group by species
  summarize(mean = mean(body_mass_g),
            sd = sd(body_mass_g)) #then give us the mean and sd for each species

#check normality assumptions with qqplot:

ggplot(data = data_for_t_test) +
  stat_qq(aes(sample = body_mass_g)) +
  facet_wrap(~ species, scales = "free") #this will separate the qq plot so you get one for each species, scales = "free" means that the scales will adjust for each graph


#looks good, qqplots are good, not outliers, next assumption
#equality of variances between data sets, should be similar for the two of them
#we sort of looked at this when we found sd, but use levene test to get a better idea:

#check equality of variances with Levene's test in rstatix:

data_for_t_test %>%
  levene_test(body_mass_g ~ species) #look to see if variance in body mass of adelie and body mass of gentoo is similar

#if our p-value is significant, we aren't meeting the assumption (use welchs t-test)
#but our p-value here is 0.159 so the variances are similar (accept null hypothesis), so we can run students t-test

#now we can run t-test:

t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species)

#our p value is < 2.2e-16, so it's less than 0.05, reject null hypothesis, 
#accept alternative hyp that the groups do not have a similar mean

#Gentoo penguins have a significantly higher body moss than Adelie penguins (t = -23.386, p = 2.2e-16)

#note that R ran a Welch's test, even though we were clear to use a students t-test
#welch's t-test is more conservative, so that's definitely okay to use
#but if we really want to use the student's t-test:

t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species, var.equal = TRUE)


################################################
# correlations
################################################


ggplot(data = gentoo) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm))

ggplot(data = gentoo) +
  stat_qq(aes(sample = bill_length_mm))

ggplot(data = gentoo) +
  stat_qq(aes(sample = bill_depth_mm))

cor(x = gentoo$bill_length_mm, y = gentoo$bill_depth_mm, use = "complete.obs") # use = "complete.obs" to get rid of rows with NA, i.e. give us complete observations

#this gives Pearsons r of 0.6433839
#r of 0 = no correlation, 1 = strong positive correlation, -1 = strong, negative correlation

cor.test(x = gentoo$bill_length_mm, y = gentoo$bill_depth_mm, use = "complete.obs")

# this gives us the test statistic and p-value, plus the pearsons r

gentoo %>%
  cor_test(bill_length_mm, bill_depth_mm)

#the output of this is a nice table

#if data aren't normal there are non-parametric tests 
#you would just add something in the parenthesis like ", method = kendall (or whatever the test is)" check documentation

head(gentoo)

######################################
#correlation matrix:
######################################

#running all the tests to see if all variables are correlated in one line of code:

cor(gentoo[ , c(3:6)], use = "complete.obs") #[ , c(3:6)] means skip rows, do it for columns 3-6

library(GGally)

gentoo %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs()

#this give us correlation r values, scatter plots, and density distribution for each variables (like a smoothed histogram)
#asterisks indicate statistical significance (more asterisks = smaller p value)

penguins %>%
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  GGally::ggpairs(aes(color = species))

#now the r values are for each species individually and for all species added together (all of them together is not a good thing to have scientifically bc why would we not group by species, but weh ave it anyway)
#note that differences in correlations aren't necessarily indicative of biologically relevant differences, could be a methods problem like more data for one species)

 



