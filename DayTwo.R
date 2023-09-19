## Day 2 : R for Statistics
# Sophia Arabadjis
# September 2023

# navigating around R... 
list.files() # shows what is in this working directory
# could be useful for when working with loops: list.files(paste(getwd(), "Desktop", sep = "/"))


# ---- Set Up ----

# Libraries
library(tidyverse)
library(ggplot2)
library(lubridate) # allows you to work with dates in R (which is difficult
    # because you have to have a reference point)

# Read in the data
d <- read_csv("/Users/gabriellebenoit/Documents/GitHub/MSC/Data/RnR.csv")

# Lets look at the data:

head(d)
colnames(d) # gives us the column names

View(d[1:20,])

## Comment: How does the data look? Provide a description. (What does the NA stand for?)
# Reef & Run race results! 
  # divPl/Div is coded wrong
  # no wetsuit is difficult to code

## Question: What class is the column `Firstname'?
class(d$Firstname)

## Question: What class is the column `Time'?
class(d$Time)
  # hours minutes seconds ("hms") in diff time format
  # currently says 7 hours not minutes... need to do data cleaning

## ---- Data Cleaning ----

# Time_minute = str_sub(Time, 1L,2L) >>> take the first 2 digits of the time column 
# the ":" counts as the third digit (3L)

# We want the Time column to be Minutes Seconds 
d <- d %>%
  mutate(Time_minute = str_sub(Time, 1L,2L), #extract the "minutes" # string sub - take out the pattern I am telling you to look for
         Time_minute = as.numeric(Time_minute), #convert to numeric
         Time_second = str_sub(Time,4L,5L), #extract the "seconds"
         Time_second = as.numeric(Time_second)/60, #make numeric and a decimal
         Time_clean = Time_minute+Time_second)%>% #make new variable to put the pieces together
  select(-c(Time_minute,Time_second,Time)) #remove unnecessary variables

## Question: What class is the column `Time_clean' ? 
# check
class(d$Time_clean)


# We also want to create an indicator variable for "male" v "female" contestants
# This information is given in the "Group" character variable. Let's see how many
# unique "groups" there are:

unique(d$Group) # there are 25 unique groups; interestingly, no NA groups

## Question: How many unique groups are there? 
## Question: Do you have any comments on the age spacing?
## Question: What class is the "Group" variable?


# Let's use a regular expression to extract the first character.
d <- d %>%
  mutate(Sex = str_sub(Group,start = 1L, end = 1L),
         Sex = factor(Sex, levels = c("M","F"), labels = c("Male","Female")))

d$Sex[1:10]
# ---- Data Exploration ----

# Let's do some exploratory data analysis and visualization:

# summary command:
summary(d)

## Question: What information does the summary command provide?


# Let's look at Age: What is the range of people that compete?

range(d$Age) #Whoops -- something is wrong here...

range(d$Age, na.rm = T) # now it works! range: 7-80

## Question: What is the age range?

## Question: What are the unique ages of the competitors?

sort(unique(d$Age))

# Suppose we want to know how many people compete at each age:
# Two ways to do this:
# Simple: Frequency Table
table(d$Age)

freq_tab <- table(d$Age) %>% #call the table command
  data.frame()%>% # make it into a data frame
  rename(Age = Var1) # rename the variables so they make sense

# Slightly more complex: grouping
## looks basically the same

freq_tab2 <- d %>% # create a new object
  group_by(Age)%>% # select a grouping variable
  summarize(Freq = n()) # summarize by the number (n()) of rows with that value

# Compare:
head(freq_tab)
head(freq_tab2)

## Question: How many people age 12 competed? 10
## Question: How many people age 36 competed? 5
## Question: How many competitors are in the most common age group? 18
# Which age is the most common among competitors? 16

max(freq_tab$Freq)

freq_tab2[freq_tab2$Freq == max(freq_tab2$Freq),] # "," means to "return all the columns"

# We could export the table:
write_csv(freq_tab, file = "/Users/gabriellebenoit/Documents/GitHub/MSC/Output/Age_Freq.csv")
# We can check to make sure it is there:
list.files("/Users/gabriellebenoit/Documents/GitHub/MSC/Output")

# But what if we want to visualize the number of competitors at each age, all at once?

## ---- Univariate Data Visualization ----

freq_tab2 %>%
  ggplot(aes(Age,Freq))+
  geom_segment(aes(x = Age, y = 0, xend=Age, yend=Freq), col = "skyblue")+
  theme_classic()+
  labs(y = "Frequency",title = "Ages and Frequency of Competitors at Reef 'n Run")

# Let's add some lines to help with our interpretation:
  # creating numbers that I am storing as values to use later.
mean_age <- mean(freq_tab2$Age,na.rm = T) # calculate the mean age
median_age <- median(freq_tab2$Age,na.rm=T) # calculate the median age
age_25th <- quantile(x = d$Age,probs = .25, na.rm=T) # calculate the 25th quantile 
age_75th <- quantile(x = d$Age,probs = .75, na.rm=T) # calculate the 75th quantile

## Question: What do we mean by age_25th? 
  # 25 percent of the data is less than that point
  # 75th, 75% of the data is less than that observed value

## geom_vline >> plot me a vertical line

freq_tab2 %>%
  ggplot(aes(Age,Freq))+
  geom_hline(yintercept = c(5,10,15), col = "lightgrey", lty = 3, lwd=.5)+
  geom_segment(aes(x = Age, y = 0, xend=Age, yend=Freq), col = "skyblue", lwd = 1)+
  theme_classic()+
  labs(y = "Frequency",title = "Ages and Frequency of Competitors at Reef 'n Run")+
  geom_vline(xintercept = mean_age, col = "black")+
  geom_vline(xintercept = median_age, col = "violet")+
  geom_vline(xintercept = c(age_25th,age_75th), col = "orange", lty = 2)

## Question: How many competitors are 20? ~4
## Question: What is the third most common age of competitors? 52
age_75th # equals 52 y.o.
## What happened to our mean_age line? it's essentially on top of the median

d %>%
  ggplot(aes(Age))+
  geom_hline(yintercept = c(5,10,15), col = "lightgrey", lty = 2, lwd=.5)+
  geom_histogram(fill = "skyblue", binwidth = 1)+
  theme_classic()+
  geom_vline(xintercept = mean_age, col = "black")+
  geom_vline(xintercept = median_age, col = "violet")+
  geom_vline(xintercept = c(age_25th,age_75th), col = "orange", lty = 2)+
  ggtitle("Histogram of Competitor Age at Reef 'n Run")

## Question: Is there any difference between the plot above, and the previous plot?
# no difference, but the bars are bigger (slightly more readable)

## Question: What is a histogram? 
 # frequency distribution of the data

## Change the binwidth to 2 in the plot above -- what happens? 
  # it is putting two ages together; aggregating

d %>%
  ggplot(aes(Age))+
  geom_hline(yintercept = c(5,10,15), col = "lightgrey", lty = 2, lwd=.5)+
  geom_histogram(fill = "skyblue", binwidth = 2)+ ### this is where you change the binwidth !!!!!!!!!
  theme_classic()+
  geom_vline(xintercept = mean_age, col = "black")+
  geom_vline(xintercept = median_age, col = "violet")+
  geom_vline(xintercept = c(age_25th,age_75th), col = "orange", lty = 2)+
  ggtitle("Histogram of Competitor Age at Reef 'n Run")

## Question: How would we need to change the freq_table to reflect this change? (in English)
# you would need to create a new category if you want to change what you're seeing

# Let's do the same sort of thing with the competitor times:

head(d$Time_clean)

# Question: how many distinct values of `Time_clean` are there?
length(unique(d$Time_clean)) # the same number of participants; well two people tied

# So it isn't going to help us to group by unique times... but we can still:
range(d$Time_clean,na.rm=T)
mean_Time <- mean(d$Time_clean,na.rm=T)
(median_Time <- median(d$Time_clean, na.rm = T))
Time_25th <- quantile(d$Time_clean, probs = .25)
Time_75th <- quantile(d$Time_clean, probs = .75)

d %>%
  ggplot(aes(Time_clean))+
  geom_hline(yintercept = c(5,10,15),col = "lightgrey", lty=3)+
  geom_histogram(fill = "cadetblue", binwidth = .5)+ #bin width 0.5 minutes
  theme_classic()+
  labs(x = "Competitor Times", title = "Histogram of Competitor Finish Times")+
  geom_vline(xintercept = mean_Time, col = "black", lwd = 1.2)+
  geom_vline(xintercept = median_Time, col = "violet", lwd= 1.2)+
  geom_vline(xintercept = c(Time_25th,Time_75th), col = "orange", lty = 2, lwd= 1.1)

  # we ARE seeing a separate mean and median now

## Question: what does the binwidth 0.5 indicate? What happens if you change it to 1?

## Let's finish by writing out our cleaned dataset:

write_csv(d, "/Users/gabriellebenoit/Documents/GitHub/MSC/Output/RnR_clean.csv")
