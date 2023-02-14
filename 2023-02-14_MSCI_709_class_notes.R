# 2023-02-14
# CDS

# ggplot

library(tidyverse)
library(palmerpenguins)

head(penguins)

find("filter") #this will tell you where r is looking for that function (which package)
stats::filter() #do this to identify/call the package you want to use it from
dplyr::select() #do this to identify/call the package you want to use it from

# create a scatterplot

ggplot(data = penguins) +
  geom_point(aes(x = flipper_length_mm, y = body_mass_g)) #geom_point -> i want scatter plot, aes -> aesthetic, where you can map your variables to various aesthetics, turn it into something i can visualize
#error that there are NAs
#could use filter functions to remove NAs

penguins_without_NAs = penguins %>%
  filter(!is.na(flipper_length_mm))
ggplot(data = penguins) +
  geom_point(aes(x = flipper_length_mm, y = body_mass_g))
  

my_plot = ggplot(data = penguins) +
  geom_point(aes(x = flipper_length_mm, y = body_mass_g, color = species, shape = sex)) +
  geom_smooth(aes(x = flipper_length_mm, y = body_mass_g)) + #by default is uses a lo est smoother, see changes in the help feature when you search this command
  xlab("Flipper Length (mm)") +
  ylab("Body Mass (g)") +
  ggtitle("Penguins are cute") +
  theme_bw() #this gets rid of the gray background
ggsave(filename = "figures/flipper_vs_mass.png", width = 7, height = 5, units = "in", dpi = 300) #do this to save the figure, by default it will do the last plot you made
#safer way would be to name your plot and 
#stack overflow is great for minute aesthetic changes

#safer way would be to name your plot and then save it:
ggsave(my_plot, filename = "figures/flipper_vs_mass.png", width = 7, height = 5, units = "in", dpi = 300) #do this to save the figure, by default it will do the last plot you made

#png is a good way to save images to retain quality across interfaces (moving it from computer to computer, moving it into powerpoints, etc)
#basically avoid jpeg and pdf if possible, the quality doesn't transfer as well


head(penguins)

#let's say we wanna know how many penguins we caught each year - let's make a time series

penguin_ts = penguins %>% #remember %>% is a pipe, passes data from one into the next
  group_by(year) %>% #separate by year
  summarize(num_penguins = n()) #now give me the number for each year

penguin_ts

penguin_ts = penguins %>%
  group_by(year, species) %>% #separate by year
  summarize(num_penguins = n()) #now give me the number for each year

penguin_ts

ggplot(data = penguin_ts) +
  geom_line(aes(x = year, y = num_penguins, color = species)) #color = species indicates that we want a separate line for each species

#now let's make a histogram

ggplot(data = penguins) +
  geom_histogram(aes(x = flipper_length_mm, fill = species))
#this gives us three histograms stacked onto each other, could unstack:
ggplot(data = penguins) +
  geom_histogram(aes(x = flipper_length_mm, fill = species), position = "identity")
#but now a lot of data is covered, let's make some transparent:
ggplot(data = penguins) +
  geom_histogram(aes(x = flipper_length_mm, fill = species, color = species), 
                 position = "identity", alpha = 0.5)
#how to change colors:
ggplot(data = penguins) +
  geom_histogram(aes(x = flipper_length_mm, fill = species, color = species), 
                 position = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4"))

#now let's make a boxplot: 
ggplot(data = penguins) +
  geom_boxplot(aes(x = species, y = flipper_length_mm)) +
  geom_jitter(aes(x = species, y = flipper_length_mm, color = species), width = 0.2) + #this will add points and randomly jiggle from side to side so it's easier to see the distribution
  theme_bw()

#now let's make a bar chart:
ggplot(data = penguins) +
  geom_bar(aes(x = sex, fill = species)) +
  facet_wrap(~species, nrow = 3) +
  theme_bw()

ggplot(data = penguins) +
  geom_bar(aes(x = island, fill = species)) +
  facet_wrap(~species, nrow = 3) +
  coord_flip() #to filp from vertical to horizontal





