# 2023-02-09
# CDS

library("tidyverse")

#the conflicts occur when one function you load is overriding another function 
  #that was already loaded
  #here, the filter function in dplyr is covering the filter function in stats
  #so it you call the function filter, it will be dplyr version not stats

library("palmerpenguins")

tidyverse_packages()
#not all of these attached automatically, but they are still part of the tidyverse

head(iris) #iris is a data frame that exists in the datasets package that's available to play with

head(penguins)
summary(penguins)
glimpse(penguins)
class(penguins)

#when there's some function that doesn't like tibbles:

mean_bill_depth = mean(as.data.frame(penguins$bill_depth_mm, na.rm=TRUE))

#using dplyr:

#filter by species

head(penguins)

gentoo = filter(.data=penguins, species=="Gentoo")
head(gentoo)
summary(gentoo)

#filter by species and sex:

gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")
head(gentoo_ladies)
summary(gentoo_ladies)

#pipe %>% 
  #for example if first you want to square it, then find the mean, then round 
  #doing all of that can be hard to keep track of
  #use pipe to deal with that - it's more readable
  # can do pipe x %>%
  #             f() %>%
  #             g() %>%
  #             h() 
  #instead of h(g(f(x)))

gentoo_ladies = penguins %>%
  filter(species=="Gentoo") %>%
  filter(sex=="female")

head(gentoo_ladies)
summary(gentoo_ladies)


#what is the mean mass of all of the female penguins

mean_ladies_mass = penguins %>%
  filter(sex=="female") %>%
  summarize(mean_mass_g = mean(body_mass_g))

#in base R this would look like

mean_ladies_mass = mean(penguins$body_mass_g[penguins$sex=="female"], na.rm=TRUE)
mean_ladies_mass

penguins %>%
  filter(!is.na(sex)) %>% #where is sex not equal to NA, just give me those rows
  group_by(sex) %>% #please treat each sex as it's own group
  summarize(mean_mass_g = mean(body_mass_g)) #now give me the mean mass for each of those groups
 
#na.rm=true is a parameter specific to the mean function

#now we want to compare sexes in each different species:

penguins %>%
  filter(!is.na(sex)) %>% 
  group_by(species, sex) %>% 
  summarize(mean_mass_g = mean(body_mass_g))

species_sex_mass_year = penguins %>%
  filter(!is.na(sex)) %>% 
  group_by(species, sex, year) %>% 
  summarize(mean_mass_g = mean(body_mass_g))

#^this is a good way to get summary statistics

write_csv(x=species_sex_mass_year, file="data/species_sex_mass_year.csv") #write.csv is base R, write_csv is in tidyverse, can probably use them interchangably

species_sex_count = penguins %>%
  filter(!is.na(sex)) %>% 
  group_by(species, sex) %>% 
  summarize(count = n())

species_sex_count

species_count = penguins %>%
  # filter(!is.na(sex)) %>% doing this will mean that the nas aren't taken out
  group_by(species) %>% 
  summarize(count = n())

species_count

penguins_for_america = penguins %>%
  mutate(body_mass_lb = body_mass_g * 0.0022)
head(penguins_for_america)
glimpse(penguins_for_america)

penguins %>%
  distinct(island)

for_my_advisor = penguins %>%
  select(species, sex) #to only include these
for_my_advisor
 
for_my_advisor = penguins %>%
  select(-bill_length_mm, -bill_depth_mm) #to only remove these
for_my_advisor

penguins %>%
  arrange(body_mass_g) #to go from smallest to largest

penguins %>%
  arrange(desc(body_mass_g)) #to go from largest to smallest

#Exercise 1.3
#What is the mean bill length (in inches) of Adelie penguins found on either 
#Dream island or Biscoe island? What is the standard deviation? 
#Is the mean larger or smaller than the mean bill length of Adelie penguins found on Torgersen island?

bill_length_Adelie_DorB = penguins %>%
  filter(!island=="Torgerson") %>%
  filter(species=="Adelie") %>%
  summarize(mean_Adelie_bill_length = mean(bill_length_mm, na.rm=TRUE)) %>%
  mutate(bill_length_in = mean_Adelie_bill_length * 0.039)
bill_length_Adelie_DorB

#as a class:

penguins %>%
  filter(species=="Adelie", 
         island %in% c("Biscoe", "Dream"),
         !is.na(bill_length_mm)) %>%
  mutate(bill_length_in = bill_length_mm * 0.039) %>% #conversion factor 0.039 in/mm
  summarize(mean_bill_length_in = mean(bill_length_in))

penguins %>%
  filter(species=="Adelie", 
         island %in% c("Biscoe", "Dream"),
         !is.na(bill_length_mm)) %>%
  mutate(bill_length_in = bill_length_mm * 0.039) %>% #conversion factor 0.039 in/mm
  summarize(sd_bill_length_in = sd(bill_length_in))

penguins %>%
  filter(species=="Adelie", 
         island %in% c("Biscoe", "Dream"),
         !is.na(bill_length_mm)) %>%
  mutate(bill_length_in = bill_length_mm * 0.039) %>% #conversion factor 0.039 in/mm
  summarize(mean_bill_length_in = mean(bill_length_in), 
            sd_bill_length_in = sd(bill_length_in))

penguins %>%
  filter(species=="Adelie", 
         island =="Torgerson",
         !is.na(bill_length_mm)) %>%
  mutate(bill_length_in = bill_length_mm * 0.039) %>% #conversion factor 0.039 in/mm
  summarize(mean_bill_length_in = mean(bill_length_in), 
            sd_bill_length_in = sd(bill_length_in))


species_sex_mass_year = penguins %>%
  filter(!is.na(sex)) %>% 
  group_by(species, sex, year) %>% 
  summarize(mean_mass_g = mean(body_mass_g))

#^this is a good way to get summary statistics

write_csv(x=species_sex_mass_year, file="data/species_sex_mass_year.csv") #write.csv is base R, write_csv is in tidyverse, can probably use them interchangably

