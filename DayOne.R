# Day 1 : R for Geographers

# September 2023

# ----- Set Up -------

## rearrange console:
# view -> panes -> pane layout -> console upper right

# save -> to folder

# load code libraries

library(tidyverse)
library(sf) # Support for simple features, a standardized way to encode spatial 
              # vector data. Binds to 'GDAL' for reading and writing data, to 
              # 'GEOS' for geometrical operations, and to 'PROJ' for projection 
              # conversions and datum transformations.
library(ggplot2)
library(car) 

# ----- MPG Cars Data -------

# call some pre-existing data

data("mpg") # if you run it once, it says "<promise>", but then if you either 
              # click on it, or run it again, then it works (it already knows where it is) 
?mpg()

head(mpg) # shows six rows by 11 columns
view(mpg) # opens a new tab displaying all of the data all at once *good for 
            # small datasets, will crash computer with big data sets
            # chr = character string
mpg[1:12,] # shows the first twelve rows of ALL the columns; R always indexes from zero
              # the blank indicated you want all the columns
mpg[1:8, 3] # shows the first eight rows of the thrid column
colnames(mpg) # pulls the column names

# call a column specifically
head(mpg$manufacturer) # the first six are audi
is.numeric(mpg$manufacturer) # how the data is being stored and read by R *important for reading in CSV files
                                # zip codes are wanted in character strings, NOT NUMERIC
head(mpg$cty) # city miles per gallon
is.numeric(mpg$cty) 
is.character(mpg$cty)

dim(mpg) # the number of rows by the number of columns: 234 x 11
            # 234 observations of 11 variables

# ----- Data Manipulation -------

head(mpg$trans)

unique(mpg$trans) # indicates there are 10 different transmission types

# make a new column with transmission:
  # because we are only interested in if it is automatic or manual

mpg <- mpg %>%
  mutate(transmission = ifelse(str_starts(trans,"auto"),1,0),
         transmission = factor(transmission,
                               levels=c(0,1), 
                               labels = c("manual", "automatic")))

head(mpg$transmission)

# we are just adding to the original dataset
# str_starts = string starts with... 
# if you took out "mpg <- " then it wouldn't save anywhere


# ---- Visualize ----

mpg %>%
  ggplot(aes(transmission,hwy), group = transmission) +
  geom_point(aes(col = transmission))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(y = "Average City MPG", x = "Transmission Type",
       title = "City MPG by Transmission Type Across Cars")

# VIGNETTE shows you all the things that you could do with the R command!
# super useful !

vignette("ggplot2-specs")

# saving a photo --> point size is trial and error
png(filename = "/Users/gabriellebenoit/Documents/GitHub/MSC/Output/Day1_Fig1.png",
    width = 480, height = 480, units = "px", pointsize = 12)

## do you have to FIRST run the above line 88 and 89 code, and SECOND run the figure code? then it saves?
## what does dev.off() do? 
    ## shuts down the specified (by default the current) device

mpg %>%
  ggplot(aes(transmission,hwy), group = transmission) +
  geom_point(aes(col = transmission))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(y = "Average City MPG", x = "Transmission Type",
       title = "City MPG by Transmission Type Across Cars")
dev.off()

## ------- Working with spatial data ----------

# Call the Data
demo(nc, ask=F, echo=F)

# Get the Data Background
?nc # North Carolina sudden infant death syndrome

# Look at the Data Structure
head(nc)

# in the preamble
  # dimension "Z" would be elevation
  # CRS = coordinate reference system

nc$geom[1][[1]] # the shape of a polygon contained in one cell

# Let's plot it
plot(st_geometry(nc$geom[1]))

# plot the whole geometry column:
plot(st_geometry(nc), 
     col = "cadetblue",
     main = "Counties of North Carolina",
     font.main = 1)

# plot with ggplot
nc %>%
  ggplot(aes())+
  geom_sf(fill = "cadetblue") +
  theme_classic()+
  ggtitle("Counties of North Carolina")

# Let's change the projection:
nc_new <- nc %>%
  st_transform(crs = 9004) # Geodetic World Coordinate Reference System

nc_new %>%
  ggplot(aes())+
  geom_sf(fill = "cadetblue")+
  theme_classic()+
  ggtitle("Counties of North Carolina")


# Let's plot some variables:

nc %>%
  ggplot(aes())+
  geom_sf(aes(fill = BIR74))+
  theme_classic()+
  theme(legend.title = element_blank())+
  ggtitle("1974 Births per County")

nc %>%
  ggplot(aes())+
  geom_sf(aes(fill = SID74))+
  theme_classic()+
  theme(legend.title = element_blank())+
  ggtitle("1974 SID per County")

nc %>%
  ggplot(aes())+
  geom_sf(aes(fill = SID74/BIR74*100000))+
  theme_classic()+
  theme(legend.title = element_blank())+
  ggtitle("1974 SID rate per County")+
  labs(subtitle = "Sudden Infant Death per 100,000")

## Which county has the highest SID rate?
# rate of SIDS per 100,000

nc %>%
  mutate(SID_rate = SID74/BIR74*100000)%>%
  filter(SID_rate == max(SID_rate))

## ---- Other Cool things to do with Spatial Data ----

# Extract the Centroids!

plot(st_geometry(nc), col = "cadetblue",
     main = "Centroids of Each County in North Carolina",
     font.main = 1)
plot(st_geometry(st_centroid(nc)), col = "orange", add=T, pch = 16, cex = .8)

# Calculate the pairwise distances between centroids:
# Transform into meaningful distance measures
nc_feet <- nc %>%
  st_transform(crs = 2264)

# Calculate the distance
dist_nc_feet <- st_distance(st_centroid(nc_feet$geom), by_element=F)
# Take the distance from Anson county, and convert to Miles
anson_dist_nc_miles <- as.vector(dist_nc_feet[,85]/5280)

# Create a new column in the data set
nc_feet <- nc_feet %>%
  mutate(dist_Anson = anson_dist_nc_miles)

# Visualize! 
nc_feet %>%
  ggplot(aes())+
  geom_sf(aes(fill = dist_Anson))+
  scale_fill_viridis_c(option = "magma",begin = 0.1)+
  theme_classic()+
  theme(legend.title = element_blank())+
  labs(title = "Distance from Anson County")



  
