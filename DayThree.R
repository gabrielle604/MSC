## Day 3 : R for Statistics II
# Sophia Arabadjis
# September 2023

# ---- Set Up ----

# Libraries
library(tidyverse)
library(ggplot2)
library(lubridate)

# Read in the data
d <- read_csv("/Users/gabriellebenoit/Documents/GitHub/MSC/Output/RnR_clean.csv")

# Make sure it loads correctly:
head(d)
class(d$Time_clean)
head(d$Time_clean)
class(d$Sex)
head(d$Sex)
head(d$Age)
class(d$Age)

## Recall that yesterday we worked on histograms / frequency graphs for Finish Times and Age.
# Today we are going to look at how Age and Finish Time of participants vary by sex.

# ---- Competitor Sex and Age ----

# Make a frequency table:
(sex_t <- d %>%
   group_by(Sex) %>%
   summarize(Count = n())) # the parentheses on the outside of the command tell R to run it AND output it!

## Question: How many total competitors are there?
sum(sex_t$Count)

# Let's calculate some basic statistics by sex:
stat_tab <- d %>%
  group_by(Sex)%>%
  summarize(mean_age = mean(Age, na.rm=T), # the summarize command is creating a bunch of new variables
            median_age = median(Age, na.rm=T),
            age_25th = quantile(Age, p=.24,na.rm=T),
            age_75th = quantile(Age,p=.75,na.rm=T),
            min_age = min(Age,na.rm=T),
            max_age = max(Age,na.rm=T))

# Let's make histogram of just the male competitors ages

d %>%
  filter(Sex == "Male")%>%
  ggplot(aes(Age))+
  geom_histogram(fill = "steelblue",binwidth = 2)+ # aggregating by 2 year intervals
  theme_classic()+
  ggtitle("Histogram of Ages of Male Competitors")+
  geom_vline(aes(xintercept = mean(Age, na.rm = T)), lwd=1.1)+
  geom_vline(aes(xintercept = median(Age, na.rm = T)),lwd=1.1, col = "violet")+
  geom_vline(aes(xintercept = quantile(Age, prob = .25, na.rm = T)),
             col = "orange", lty=2)+
  geom_vline(aes(xintercept = quantile(Age, prob = .75, na.rm = T)),
             col = "orange", lty=2)

# Let's visualize another way -- box plots
d %>%
  ggplot(aes(Sex, Age))+
  geom_boxplot(fill = c("sandybrown","steelblue"))+
  theme_classic()+
  ggtitle("Age of Reef 'n Run Competitors by Sex")

## Question: How do we read these box plots?
  # bottom of box is 25%, 75% is the top line, the black horizontal line is the median
## Question: What do the box edges represent?
## Question: What does the geom_jitter line do?

# We could also use a violin plot:

d %>%
  filter(!is.na(Age))%>%
  ggplot(aes(Sex, Age))+
  geom_violin(scale = "count", aes(fill = Sex),
              draw_quantiles = c(0.25, 0.75),
              lty = 'dashed')+
  geom_violin(fill = "transparent",scale = 'count', draw_quantiles = 0.5)+
  theme_classic()+
  geom_jitter(height = 0, width = 0.1,size = .2)+
  ggtitle("Age of Reef 'n Run Competitors by Sex")

## Question: How do we read this plot?
## Question: What does the geom_jitter() command do?

# Using the violin plot / box plot, make a sketch of the distribution of the ages of the
# female-identifying competitors.
  # point of this exercise, is that violin plots are great, but interpret with care, because it
    # requires guessing a bit -- make a histogram to be more exact

d %>%
  filter(Sex == "Female")%>%
  ggplot(aes(Sex, Age))+
  geom_violin(scale = "count", aes(fill = Sex),
              draw_quantiles = c(0.25, 0.75),
              lty = 'dashed')+
  geom_violin(fill = "transparent",scale = 'count', draw_quantiles = 0.5)+
  theme_classic()+
  geom_jitter(height = 0, width = 0.1,size = .2)+
  ggtitle("Age of Reef 'n Run Competitors by Sex")

# Make a histogram of the ages of the female identifying competitors. 

d %>%
  filter(Sex == "Female")%>%
  ggplot(aes(Age))+
  geom_histogram(fill = "darksalmon",binwidth = 2)+ # aggregating by 2 year intervals
  theme_classic()+
  ggtitle("Histogram of Ages of Female Competitors")+
  geom_vline(aes(xintercept = mean(Age, na.rm = T)), lwd=1.1)+
  geom_vline(aes(xintercept = median(Age, na.rm = T)),lwd=1.1, col = "goldenrod")+
  geom_vline(aes(xintercept = quantile(Age, prob = .25, na.rm = T)),
             col = "darkseagreen", lty=2)+
  geom_vline(aes(xintercept = quantile(Age, prob = .75, na.rm = T)),
             col = "darkseagreen", lty=2)

## Question: How does your sketch compare with your histogram?



## ---- Competitor Sex by Swim Time ---- 

(stat_tab_time <- d %>%
   group_by(Sex)%>%
   summarize(mean_time = mean(Time_clean, na.rm=T),
             median_time = median(Time_clean, na.rm=T),
             time_25th = quantile(Time_clean, p=.24,na.rm=T),
             time_75th = quantile(Time_clean,p=.75,na.rm=T),
             min_time = min(Time_clean,na.rm=T),
             max_time = max(Time_clean,na.rm=T)))

## Make a violin plot of the competitor swim times by sex:
d %>%
  filter(!is.na(Time_clean))%>%
  ggplot(aes(Sex, Time_clean))+
  geom_violin(scale = "count", aes(fill = Sex),
              draw_quantiles = c(0.25, 0.75),
              lty = 'dashed')+
  geom_violin(fill = "transparent",scale = 'count', draw_quantiles = 0.5)+
  theme_classic()+
  geom_jitter(height = 0, width = 0.1,size = .2)+
  ggtitle("Finish Times of Reef 'n Run Competitors by Sex")


## Question: Who has the bigger spread?
  # Inner quartile range of females is wider; 50% of the data is occuring 
    # across a wider range than for the male competitors

# Variance is a measure of the distribution of data points about the mean.
# Specifically we measure variance by summing the distance from each point to the 
# mean, squaring it, and dividing it by the total number of points we observed less one.
# We can write a function to do this:

my_var <- function(data){
  data <- data[!is.na(data)] #filter any NA values
  m <- mean(data) # take the mean
  temp <- (data-m)^2 #subtract the mean from each value and square
  temp <- sum(temp) # sum the values
  temp <- temp/(length(data)-1) # divide by the number of observations - 1
  return(temp) # return our variance
}

my_var(data=d$Age)
my_var(data = d$Age[d$Sex=="Male"])
my_var(data = d$Age[d$Sex=="Female"])

my_var(data = d$Time_clean[d$Sex=="Male"])
my_var(data = d$Time_clean[d$Sex=="Female"])

## Question: Do male- or female- identifying competitors have a broader age distribution?
  # Male competitors have more variation in age
## Question: Do male- or female- identifying competitors have a broader finish time distribution?
  # Female competitors have more variation in finish time (though males and females are close)
## Question: What units is the variance measured in?
## Question: What is the standard deviation?
  # square root of the variance; has units

sd(d$Age, na.rm=T)
sqrt(my_var(d$Age)) # gives you the same answer

# We can make assumptions about data that looks normally distributed using the standard deviation
  # we can make assumptions of data, for example, 2 SD from the mean

sd(d$Time_clean, na.rm = T)

# using the variance function
var(d$Time_clean, na.rm = T)^(1/2)

## ---- Age & Swim Time ----

d %>%
  filter(!is.na(Age))%>%
  ggplot(aes(Age, Time_clean))+
  geom_point()+
  theme_classic()+
  labs(x = "Competitor Age", y = "Finish Time", 
       title = "Competitor Finish Time by Age")

## Question: Do you see a pattern? - Maybe we can add some visuals:
d %>%
  filter(!is.na(Age))%>%
  ggplot(aes(Age, Time_clean))+
  geom_point()+
  geom_smooth(method = "loess", se=F, color = "burlywood2")+ #Local polynomial regression fitting
  geom_smooth(method = "lm", color = "tomato3", se=F)+ # linear, straight line method
  theme_classic()+
  labs(x = "Competitor Age", y = "Finish Time", 
       title = "Competitor Finish Time by Age")

# Try changing se=F to se=T, what happens?
  # shows the standard error

## Let's add an indicator of competitor sex to the plot:
fig1 <- d %>%
  filter(!is.na(Age))%>%
  ggplot(aes(Age, Time_clean, col = Sex))+
  geom_point(aes(shape = Sex))+
  theme_classic()+
  labs(x = "Competitor Age", y = "Finish Time", 
       title = "Competitor Finish Time by Age and Sex")

fig1 + 
  geom_smooth(data=filter(d, Sex == "Female"),method = "lm",se=F)+
  geom_smooth(data=filter(d, Sex == "Female"),method = "loess",se=F)+
  geom_smooth(data=filter(d, Sex == "Male"),method = "lm", se=F)+
  geom_smooth(data=filter(d, Sex == "Male"),method = "loess", se=F)

## Anything more remarkable?
  # nope... 

# ---- What about distance of swims? ----

## How many unique distances are there?
  # three! 500m, 1000m, 1 mile (1600m)
unique(d$distance)

## How many competitors competed in each distance?
d %>% 
  group_by(d$distance) %>% 
  summarize(Number = n())

# the most popular swim, THE MILE

## What class is the distance variable?
class(d$distance)

d <- d %>%
  mutate(distance = factor(distance, levels = c(500,1000,1600)))

# We can add this to the plot too:

fig2 <- d %>%
  filter(!is.na(Age))%>%
  ggplot(aes(Age, Time_clean, col = distance))+
  geom_point(aes(shape = Sex))+
  theme_classic()+
  labs(x = "Competitor Age", y = "Finish Time", 
       title = "Competitor Finish Time by Age, Sex, and Distance")

# Let's add some. lines to this to see what we get:

fig2 + 
  geom_smooth(data=filter(d, Sex == "Female" & distance == 500),method = "lm",se=F)+
  geom_smooth(data=filter(d, Sex == "Female" & distance == 1000),method = "lm",se=F)+
  geom_smooth(data=filter(d, Sex == "Female" & distance == 1600),method = "lm",se=F)+
  geom_smooth(data=filter(d, Sex == "Male" & distance == 500),method = "lm",se=F,lty=5)+
  geom_smooth(data=filter(d, Sex == "Male" & distance == 1000),method = "lm",se=F, lty=5)+
  geom_smooth(data=filter(d, Sex == "Male" & distance == 1600),method = "lm",se=F, lty=5)

# Simpson's Paradox is a statistical phenomenon where an association between two variables
  # in a population emerges, disappears or reverses when the population is divided into subpopulations.

## Question: What is covariance? Covariance is used to calculate correlation

cov(d$Time_clean,!is.na(d$Age))

## Question: What is correlation?
  # correlation ranges between -1 and 1
  # correlation = standardized version of covariance

cor(d$Time_clean,is.na(d$Age))

## Question: How does covariance relate to correlation?

cov(d$Time_clean,!is.na(d$Age))/sqrt(var(d$Time_clean)*var(!is.na(d$Age)))



# how to cite packages: 
citation("tidyverse")