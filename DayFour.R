## Day 4 : R for Statistics III
# Sophia Arabadjis
# September 2023

# ---- Set Up ----

# Libraries
library(tidyverse)
library(ggplot2)

# Read in the data
d <- read_csv("/Users/gabriellebenoit/Documents/GitHub/MSC/Output/RnR_clean.csv")

# Yesterday we saw how looking at all the swimmers together may have obscured
# underlying patterns or differences in swim time and ages. Today, let's
# split the data by distance.

d_500 <- d %>%
  filter(distance == 500)%>%
  filter(!is.na(Age) & !is.na(Time_clean))
d_1600 <- d %>%
  filter(distance == 1600)%>%
  filter(!is.na(Age) & !is.na(Time_clean))
d_1000 <- d %>%
  filter(distance == 1000)%>%
  filter(!is.na(Age) & !is.na(Time_clean))

# Let's plot one of these distances:

fig_16 <- d_1600 %>%
  ggplot(aes(Age, Time_clean))+
  geom_point(aes(shape = Sex))+
  labs(y = 'Finish Time', x = "Age", 
       title = "Finish Time by Age and Sex for Mile Competitors")+
  theme_classic()

fig_16 +
  geom_smooth(data = filter(d_1600, Sex == "Female"), method = "lm", lty = 1, se=F)+
  geom_smooth(data = filter(d_1600, Sex == "Male"), method = "lm", lty=5, se = F)

## Question: Using a visual assessment, is there any correlation between Age and Finish time for the mile?
## Question: What direction is the correlation?

# Let's assess the correlation for each group separately

d_1600 %>%
  filter(Sex == "Male")%>%
  select(Age, Time_clean)%>%
  cor()

d_1600 %>%
  filter(Sex == "Female")%>%
  select(Age, Time_clean)%>%
  cor()

# Let's also make sure we have a handle on the how the distributions might differ:

# Finish Time
d_1600 %>%
  group_by(Sex)%>%
  summarize(mean_time = mean(Time_clean),
            median_time = median(Time_clean),
            time_25th = quantile(Time_clean, probs = .25),
            time_75th = quantile(Time_clean, probs = .75),
            sd_time = sd(Time_clean),
            var_time = var(Time_clean))

# Age:
d_1600 %>%
  group_by(Sex)%>%
  summarize(mean_age = mean(Age),
            median_age = median(Age),
            age_25th = quantile(Age, probs = .25),
            age_75th = quantile(Age, probs = .75),
            sd_age = sd(Age),
            var_age = var(Age))

## Question: Describe the distribution of competitor finish times by sex.
## Question: Describe the distribution of Competitor ages by sex.

# Let's see if we can visualize these differences:

d_1600 %>%
  group_by(Sex)%>%
  ggplot(aes(x=Time_clean, fill=Sex)) + geom_histogram(alpha=0.35,binwidth = .75)+
  theme_classic()+
  labs(x = "Competitor Count", y = "Finish Time", 
       title = "Histograms of 1600m Competitors' Finish Times by Sex")+
  geom_vline(xintercept = 28, col = "violet")+ #Female
  geom_vline(xintercept = 27.1, col = "cadetblue") # Male

# We can also look at this as a Density:

d_1600 %>%
  group_by(Sex)%>%
  ggplot(aes(x=Time_clean, fill=Sex)) + geom_density(alpha=0.35)+
  theme_classic()+
  labs(y = "", x = "Finish Time", 
       title = "Densities of 1600m Competitors' Finish Times by Sex")+
  geom_vline(xintercept = 28, col = "violet")+
  geom_vline(xintercept = 27.1, col = "cadetblue")

# A density in this sense refers to an empirical probability density function
# for the values of the "Finish Time" variable. More on this tomorrow, but we use
# PDFs frequently to understand/calculate the probability of observing a value greater
# than or less than some point. 
    # probability of observing a value greater or less than "some" point *the mean for instance
    # area under each of the curves will be one


## Question: how much do these densities overlap? (Do we see much difference?)

# Create a density visual using competitor age and sex:

d_1600 %>%
  group_by(Sex)%>%
  ggplot(aes(x=Age, fill=Sex)) + geom_density(alpha=0.35)+
  theme_classic()+
  labs(y = "", x = "Age (Yrs)", 
       title = "Densities of 1600m Competitors'Age by Sex")+
  geom_vline(xintercept = 31.1, col = "violet")+
  geom_vline(xintercept = 40.8, col = "cadetblue")


## Question: How evenly are the density of swimmers distributed between the groups?
  # a lot less overlap; many more young women than young men competing 

# ---- Hypothesis testing ---- #

# To build some intuition, do we think, on average, that the age distributions of female 
# and male competitors in the 1600m race are different? How about the means?

# Formalize this with Math:
# H_null = No difference between the mean age by sex
# H_alternative = mean_age_male is not equal to mean_age_female

# In order to test these hypotheses, we need to know how precisely the ages were
# measured. I don't mean the precision in the measurement (11.5 years versus 11.2 years).
# I mean the spread of the observed values... Variance!

# In this case we can do a T-Test:

t.test(d_1600$Age ~ d_1600$Sex)

## Question: Interpret
# we are very sure of something we're not seeing; so we "reject" the null 
# hypothesis, rather than "accept" the alternative hypothesis
# age is the outcome variable, by sex
# outcome t, which is the test statistic ... the value we will compare to a known distribution
# df = degrees of freedom
# p-value = is it less than 0.05 (or 0.001) ... to indicate significance
# alternative hypothesis = "true difference in means between group Female and
# group Male is not equal to 95 percent confidence interval" ... but we don't know where
# the difference is... the sample estimates tell us 


# Now let's run another t-test on the finish times
## Question: do we think the mean finish times between male and female identifying
# competitors will be different? Why or why not?


t.test(d_1600$Time_clean ~ d_1600$Sex)

## Question: Interpret



