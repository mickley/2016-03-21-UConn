## University of Connecticut Software Carpentry - Day 2
### dplyr and ggplot2
### Chris Hamm

# Objectives for dplyr tutorial - learn basic data manipulation without using []


# First things first, I always like to set my working directory:

setwd("~/Desktop")

# Next we install the package. There are a number of ways to install a package, I will cover a few here:

# from the CRAN (first select the mirror)
install.packages("dplyr", depend = TRUE)

# from a file - find it on the web
install.packages("dplyr_0.4.3.tar.gz", type = "source", repos = NULL)

# from GitHub
install.packages("devtools", depend = TRUE)
devtools::install_github("hadley/dplyr")

library("dplyr") # To operate a package you must install it AND then load it. 
sessionInfo()
# or
packageDescription("dplyr")
# or
packageDescription("dplyr")$Version

# Challenge #1 - create an object that called "sessInfo" that contains your sessionInfo(), and write your code so that you create the object AND see the output.





# Answer:
(sessInfo <- sessionInfo())

# Cool, so we have dplyr installed. What is it? dplyr is a package that provides easy tools for the most common data manipulation tasks. the d stands for data frames. It is written in C++ and is very fast.
# Lets load our portal rodents data

surveys <- read.csv("Combined.csv", header = T)
head(surveys)
# It is important to spell out TRUE and FALSE

T <- FALSE
surveys <- read.csv("Combined.csv", header = T)
head(surveys)
TRUE <- FALSE

surveys <- read.csv("Combined.csv", header = TRUE)
head(surveys)
str(surveys)


# The most commonly used functions in dplyr:
# select(), filter(), mutate(), group_by(), and summarise()

# select grabs columns of data
# first argument is data object
# get help with ?
select(surveys, plot_id, species_id, weight)

# filter grabs rows
# How do we grab only year 1995? 

filter(surveys, year == 1995)


# Pipes %>% - a specific type of pipe (not like a Unix pipe) that comes from the magrittr package. Allow you to select and filter at the same time. 

surveys %>% filter(weight < 5) %>% select(species_id, sex, weight)

# challenge: create an object that contains species_id, weight, and sex for animals that are equal or greater than 200. 


# Subset the data to include rows before 1995, retain columns year, sex, weight






# answer
s2 <- surveys %>% filter(year < 1995) %>% select(year, sex, weight)
dim(s2)
head(s2)



### Mutate - creates new columns based on values in existing columns

surveys %>% mutate(weight_kg = weight / 1000) 

# This runs off the screen. Bleurgh. If only there was a way to look at the first few lines...

surveys %>% mutate(weight_kg = weight / 1000) %>% head
# The first few rows are NA

surveys %>% filter(!is.na(weight)) %>% mutate(weight_kg = weight / 1000) %>% head

# split-apply-combine and summarise. Putting it all together
# This means, split the data into groups, apply a function, and combine the results. We split the data into groups with group_by().

surveys %>% group_by(sex) %>% tally()
# group_by collapses a group into a single row - awesome for generating summaries. 

surveys %>% group_by(sex) %>% summarise(mean_weight = mean(weight, na.rm = TRUE))

# Group by multiple columns too!
surveys %>% group_by(sex, species_id) %>% summarize(mean_weight = mean(weight, na.rm = TRUE))

# NaN is Not a Number, looks like a lot of these were not measured.
surveys %>% group_by(sex, species_id) %>% summarize(mean_weight = mean(weight, na.rm = TRUE)) %>% filter(!is.nan(mean_weight))

# You can summarise multiple columns at the same time!
surveys %>% group_by(sex, species_id) %>% summarize(mean_weight = mean(weight, na.rm = TRUE), min_weight = min(weight, na.rm = TRUE)) %>% filter(!is.nan(mean_weight))


# Challenge
# How many times was each plot surveyed?

# Use group_by() and summarise() to find mean, min and max hindfoot lenght for each species.







# answers
surveys %>% group_by(plot_id) %>% tally()

surveys %>% filter(!is.na(hindfoot_length)) %>% group_by(species) %>% summarise(mean_hl = mean(hindfoot_length), min_hl = min(hindfoot_length), max_hl = max(hindfoot_length))

surveys %>% group_by(species) %>% filter(!is.na(hindfoot_length)) %>% summarise(mean_hl = mean(hindfoot_length), min_hl = min(hindfoot_length), max_hl = max(hindfoot_length))



# BREAK!

## ggplot2
### Grammar of Graphics

# Learning objectives:
# visual data
# understand how to plot data with ggplot2
setwd("~/Desktop/UCONN_SWC_day2")


install.packages("ggplot2", depend = TRUE)

library("ggplot2")
library("dplyr")


surveys_raw <- read.csv("surveys.csv")
# surveys_raw <- read.csv("https://ndownloader.figshare.com/files/2292172") #loads directly from FigShare
head(surveys_raw)
str(surveys_raw)

# if you want to see the objects loaded into memory in R, you can print a list
ls()

# to remove something, use rm
rm(surveys)

# let's clean the data a bit using our new found dplyr skills
summary(surveys_raw) # species_id has missing values!?!?!

surveys_complete <- surveys_raw %>% filter(species_id != "")
summary(surveys_complete)

# There were also NAs for hindfoot_length and weight, so lets write dplyr code to clean all that up.

surveys_complete <- surveys_raw %>% filter(species_id != "") %>% filter(!is.na(weight)) %>% filter(!is.na(hindfoot_length))
summary(surveys_complete)

# right now this is in R's memory. What if you want to quit R? You don't want to re-run all these steps every time, especially if you have some analtical results that took a long to to achieve. So let us save these data.

save(list = "surveys_complete", file = "Surv_comp.RData") 

#what if you want to save every object in R's memory? R wants a list of name.
save(list = ls(), file = "Surv_comp.RData")
load(file = "Surv_comp.RData")
ls()
sessionInfo() # still have to re-load packages, but don't need to reinstall. 

# ok, back to it. 
# count records per species
species_counts <- surveys_complete %>% group_by(species_id) %>% tally
head(species_counts)

# get names of those frequent species
frequent_species <- species_counts %>% filter(n >= 10) %>% select(species_id) 
head(frequent_species)

# now, let's make an object of the commonly occuring species
surveys_complete <- surveys_complete %>% filter(species_id %in% frequent_species$species_id)

# Make simple scatter plot of hindfoot_length (in millimeters) as a function of weight (in grams), using basic R plotting capabilities.

plot(x = surveys_complete$weight, y = surveys_complete$hindfoot_length)

plot(x = surveys_complete$weight, y = surveys_complete$hindfoot_length, pch = 19)

plot(x = surveys_complete$weight, y = surveys_complete$hindfoot_length, pch = 19, xlab = "Weight", ylab = "Hindfoot Length")

plot(x = surveys_complete$weight, y = surveys_complete$hindfoot_length, pch = 19, xlab = "Weight", ylab = "Hindfoot Length", col = "blue")


## plotting with ggplot2
# To build a ggplot we need to:
# bind the plot to a specific data frame using the data argument
ggplot(data = surveys_complete)

# define aesthetics ( aes), that maps variables in the data to axes on the plot or to plotting size, shape color, etc.,
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length))

# add geoms – graphical representation of the data in the plot (points, lines, bars). To add a geom to the plot use + operator:
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) + geom_point()

# Now lets modify these plots even more!!!
# adding transparency (alpha)
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) + geom_point(alpha = 0.1)

# adding colors
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) + geom_point(alpha = 0.1, color = "blue")

# Boxplot Visualising the distribution of weight within each species.
ggplot(data = surveys_complete, aes(x = species_id, y = weight)) + geom_boxplot()

# By adding points to boxplot, we can see particular measurements and the abundance of measurements.
ggplot(data = surveys_complete, aes(x = species_id, y = weight)) + geom_jitter(alpha = 0.3, color = "tomato") + geom_boxplot(alpha = 0)


## Challenges

# Create boxplot for hindfoot_length.


## Plotting time series data
# Let’s calculate number of counts per year for each species. To do that we need to group data first and count records within each group.

yearly_counts <- surveys_complete %>% group_by(year, species_id) %>% tally
summary(yearly_counts)

# Timelapse data can be visualised as a line plot with years on x axis and counts on y axis.
ggplot(data = yearly_counts, aes(x = year, y = n)) + geom_line()

# Unfortunately this does not work, because we plot data for all the species together. We need to tell ggplot to split graphed data by species_id

ggplot(data = yearly_counts, aes(x = year, y = n, group = species_id)) + geom_line()

# We will be able to distinguish species in the plot if we add colors.
ggplot(data = yearly_counts, aes(x = year, y = n, group = species_id, color = species_id)) + geom_line()

# Faceting - ggplot has a special technique called faceting that allows to split one plot into multiple plots based on some factor. We will use it to plot one time series for each species separately.
ggplot(data = yearly_counts, aes(x = year, y = n, color = species_id)) + geom_line() + facet_wrap(~species_id)


# Challenges:
# filter the dataframe so that we only keep records with sex “F” or “M”s

# group by year, species_id, sex

# make the faceted plot splitting further by sex (within single plot)


# Answers
sex_values <- c("F", "M")
surveys_complete <- surveys_complete %>% filter(sex %in% sex_values) 


yearly_sex_counts <- surveys_complete %>% group_by(year, species_id, sex) %>% tally


ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = species_id, group = sex)) + geom_line() + facet_wrap(~ species_id)

# I find that plots with white background looks more readable in the publications. We can set the background to white using function theme_bw().
ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = species_id, group = sex)) + geom_line() + facet_wrap(~ species_id) + theme_bw()

# We can improve the plot by coloring by sex instead of species (species are already in separate plots, so we don’t need to distinguish them better)
ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex, group = sex)) + geom_line() + facet_wrap(~ species_id) + theme_bw()


# plot average weight of each species over the course of the years
yearly_weight <- surveys_complete %>% group_by(year, species_id, sex) %>% summarise(avg_weight = mean(weight, na.rm = TRUE))
ggplot(data = yearly_weight, aes(x=year, y=avg_weight, color = species_id, group = species_id)) + geom_line() + theme_bw()


# make separate plots per sex since weight of males and females can differ a lot
ggplot(data = yearly_weight, aes(x=year, y=avg_weight, color = species_id, group = species_id)) + geom_line() + facet_wrap(~ sex) + theme_bw() 


# Now, let’s change names of axes to something more informative than ‘year’ and ‘n’ and add title to this figure:
ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex, group = sex)) + geom_line() + facet_wrap(~ species_id)+ labs(title = 'Observed species in time', x = 'Year of observation', y = 'Number of species') + theme_bw()

# Thanks to our efforts, axes have much more informative names, yet quite small so it could be hard to read them. Let’s change their size (and font just in sake of fun):
ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex, group = sex)) + geom_line() + facet_wrap(~ species_id) + labs(title = 'Observed species in time', x = 'Year of observation', y = 'Number of species') + theme(text = element_text(size = 16, family = "Arial")) + theme_bw()

# After our manipulations we notice that the values on the x-axis are still not properly readable. Let’s change
the orientation of the labels and adjust them vertically and horizontally so they don’t overlap. You can use
a 90 degree angle, or experiment to find the appropriate angle for diagonally oriented labels.
ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex, group = sex)) + geom_line() + facet_wrap(~ species_id) + theme_bw() + theme(axis.text.x = element_text(colour="grey20", size=12, angle=90, hjust=.5, vjust=.5), axis.text.y = element_text(color = "grey20", size = 12), text=element_text(size = 16, family = "Arial")) + labs(title = 'Observed species in time', x = 'Year of observation', y = 'Number of species')

# Now, labels became bigger and readable, but there are still few things that one could be improved.
# Please, take another five minutes and try to add another one or two things, to make it look even more beautiful. Use ggplot2 cheat sheet, which we linked earlier for inspiration.
# Here are some ideas:
# See if you can change thickness of the lines.
# Can you find a way to change the name of the legend? What about its labels?

ggsave("observed_species_in_time.png", width=15, height=10)



