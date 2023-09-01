# intro to working with the Carolina bay zooplankton data

# reading in data ####
# set working directory
setwd("/Users/katieschroeder/Downloads")
zoop <- read.csv("cleaned_SRS_zoop_data.csv")

# intro to Tidyverse ####
# Tidyverse is a collection of packages in R that a lot of ecologists and data 
# scientists like to use. It contains helpful functions and options for graphing 
# data that can be  more user friendly and faster than some of the methods  
# we've used so far.

# if you've never used Tidyverse, you'll need to install it
install.packages("tidyverse")

# to use Tidyverse pacakges, you need to call it from your library
library(tidyverse)

# let's look at the data
str(zoop) 
# str is short for structure and gives you an overview of your data
View(zoop) #another option if you'd rather look at it as a dataframe

# the date column read in as a character instead of a date, so reformat as 
# a date
zoop$sample_date<-mdy(zoop$sample_date)

# get to know these data
# the file SRS_zoop_metadata.csv contains information about each of the columns
# of this dataframe

# if we want to ask how many different taxa are in this data frame, we
# can use the unique function. It gives a list of the unique entries of a 
# specified column
unique(zoop$taxa)

# if we want to see how many times each taxa appears in these data, we can
# use the table function. The table function takes each taxa and counts
# the frequency
table(zoop$taxa)
# to see the same list in order, you can use sort
sort(table(zoop$taxa),decreasing=TRUE)

# the column 'broad group', groups these taxa and we can use it to get a 
# more summarized version
sort(table(zoop$broad_group),decreasing=TRUE)

#similar to seeing how many taxa there are, we can see how many different 
# bays there are
unique(zoop$bay)

# let's look at the unique number of taxa in one bay, bay 78
# to look at one bay using Tidyverse notation
# the %>% symbol is called a pipe and is an operator that allows for sequential
# operations (i.e. you can do things one after another)
# in this example, we're going to filter our data to just bay 78 and then
# find the number of unique taxa on each sample data
zoop %>% filter(bay=='78') %>% group_by(sample_date) %>%
  summarize(richness=n())

# we can save this as its own data frame by using an <- operator
bay78 <- zoop %>% filter(bay=='78') %>% group_by(sample_date) %>%
  summarize(richness=n()) # n gives the number of rows (taxa) for each sample_date

# graphing richness over time ####
# an intro to ggplot
# if we want to graph the richness over time, we can use plot like we have 
# previously, or we can use a Tidyverse package, ggplot
# what we've done previously
plot(x = bay78$sample_date, y = bay78$richness, 
     xlab="Richness", ylab="Sample Date")

# in ggplot
bay78 %>% ggplot(aes(x = sample_date, y=richness)) +
  geom_point() +   # adds points to the data
  theme_classic()  # changes the theme, removes background

# if we want to ask if there's a seasonal change, we could look at the 
# month of each observation and separate them by year

# to add a new column to our bay 78 dataframe, we use mutate in Tidyverse
# this makes two new columns, one with the month and one with the year
bay78 <- bay78 %>% mutate(month = month(sample_date),
                          year = year(sample_date))

# let's graph this with month on the x-axis and different colors for each group
bay78 %>% ggplot(aes(x = month, y = richness, color = as.factor(year))) +
  geom_point() +
  theme_classic() +
  scale_x_continuous(breaks=seq(1,12,1)) # labels the x-axis for each month

# if we want to look at the average abundance of one of the most observed 
# species in these data in a couple of the bays, we can use Tidyverse to do
# all of our manipulations and get a plot
# we'll use Acanthocyclops robustus 
zoop %>% filter(taxa=="Acanthocyclops robustus") %>%
  filter(bay=="78"|bay=='4'|bay=='26') %>%
  ggplot(aes(x=sample_date,y=abund,group=year)) +
    geom_point() +
    geom_line(aes(group=year)) +
    facet_wrap(.~bay) +
    theme_classic()
  


# Tidyverse cheatsheets: ####
# data wrangling: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf 
# data tidying: https://rstudio.github.io/cheatsheets/tidyr.pdf 
# graphing: https://rstudio.github.io/cheatsheets/data-visualization.pdf
# data transformation: https://rstudio.github.io/cheatsheets/data-transformation.pdf 

