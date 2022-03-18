########################################
########################################
## Data Wrangling ######################
########################################
########################################

#### Data Frame ----------------------------------------------------------------

# Let's create a data frame 
trial01 <- data.frame(
  variety = c("G01-US234", "G05-BT456", "Ind01","G11-DR234", "Check"), 
  yield = c(6323.3, 2515.2, 5611, 7729, 7843.25),
  height = c(123.30, 95.2, 113, 89.45, 145.67)
  )

# Let display the data frame
trial01  ## display the object in Q2
View(trial01) ## display the object in Q1

# We can extract the first three rows: 
# object[1:nrows, 1:ncolumns]
trial01[1:5, 1:3]
trial01[1:3, ] # the three first rows and all columns

# We can extract the first two columns
trial01[, 1:2]

# We can extract "from 3rd to 5th row" with "2nd and 3rd column"
trial01[3:5, 2:3]

# We can list the column names
names(trial01)
colnames(trial01)

# We can extract specific column from a data frame using column name
trial01$yield
trial01$Yield  ## R is case sensitive, yield is different to Yield
mean(trial01$yield)

# We can add a column vector using a new column name
trial01$flowering <- c(87, 101, 88, 120, 90)

trial01

#### The tidyverse package ----------------------------------------------------

library(tidyverse)

# Let's look at the structure of trial01
str(trial01) ## structure of trial01: what's trial01?

# we can convert trial01 to a tibble and save the new created object into trial01.tibble
trial01.new <- as_tibble(trial01) 
trial01.new
# dbl(double) and int(integer) are numeric

#### Data Import ---------------------------------------------------------------

# Read csv file: supply the path to a file and you get the data into R
mydata <- read_csv("/Users/idieng/OneDrive - CGIAR/_IITA/Courses and Seminars/given/2021/R/Ibadan/R-November-2021/Practical/Example-02.csv")
mydata
View(mydata)
# If a project is created and we are working within the project,
mydata1 <- read_csv("Example-02.csv") # Tab keyboard

## Read xlsx file
library(readxl)

## Read Excel file:
mydata <- read_excel("Example-03.xlsx", sheet = "whitecorn")

#### Data Transformation -------------------------------------------------------
library(tidyverse)

example02 <- read_csv("Example-02.csv")
example02
names(example02)
str(example02) ## really don't need when you have tibble

# I want to display the number of years
example02$year
unique(example02$year)
unique(example02$loc) # having the number of locations 

## The pipe %>% ---------
# Pipes are a powerful tool for clearly expressing a sequence of multiple operations.
# object %>% (object is usually a tibble, a data)
# function(argument1, argument2, ...)

## Filter ---------------

# We can filter the data for 1970

example02.70 <- example02 %>% 
  filter(year == 1970)

example02.70
# Filtering using the pipe is equivalent to below but not nice
example02.70.not.nice <- example02[example02$year==1970, ]

example02.70.not.nice


# We can filter the data for one location :
example02.Lawes <- example02 %>% 
  filter(loc == "Lawes")

example02.Lawes

# We can filter the data with multiple criteria (several arguments)
example02.Lawes3.2 <- example02 %>% 
  filter(yield > 3.2, loc == "Lawes")

example02.Lawes3.2

# What does the following command do?
example02 %>% 
  filter(gen == "G01" | gen == "G02")


## arrange --------------

# We can arrange example02 by yield in ascending order
example02 %>% 
  arrange(yield)
# We can arrange example02 by yield in descending order
example02 %>% 
  arrange(desc(yield))

# We can arrange example02 by year, loc, gen
example02 %>% 
  arrange(year, loc, gen)

example02 %>% 
  arrange(desc(year), loc, gen)


# When working with many variables, it can be a good practice to narrow
# the dataset and consider only few variables for analysis. Let's only
# consider the location, year, genotype, yield and height
example02.short <- example02 %>% 
  select(loc, year, gen, yield, height)

example02.short

# We can be interested to move one or more variables to the start of the
# data frame. For that, we can use select() and everything()
example02 %>% 
  select(year, everything())

example02 %>% 
  select(year, loc, gen, everything())


## select refers to columns: select (yield, loc, gen) -- choose the columns
example02 %>% 
  select (yield, loc, gen)
## arrange refers to rows: arrange(loc, desc(yield)) -- sort

## will not work because there is no variable named "desc(yield)"
example02 %>%        
  select (loc, gen, desc(yield))

# But what we want is:
 # 1. select the three variables, AND
 # 2. sort the yield in desc order

## Rhis is camn be done with:
example02 %>% 
  select (loc, gen, yield) %>% 
  arrange(desc(yield))


## Add New Variables ----

# We can add new columns that are functions of existing columns with
# mutate() which always adds new columns at the end of the data
## new column
example02.short %>% 
  mutate(yield_kg_ha = yield * 1000)
## replace an existing column
example02.short %>% 
  mutate(yield = yield * 1000)

## Summarize ------------
# we can be instrested to have the mean of yield
mean(example02$yield, na.rm = TRUE)

a <- c(2,3,4)
a
mean(a)
mean(a, na.rm = TRUE)

b <- c(2,3,4,NA)
b
mean(b)
mean(b, na.rm = TRUE)

# But we prefer using summarize from tidyverse
# summarize() collapses a data frame to a single or few row(s)
example02 %>% 
  summarize(yield_mean = mean(yield, na.rm=TRUE))


# summarize by group

unique(example02$loc)
example02 %>%
  group_by(loc) %>%
  summarise(yield_loc = mean(yield, na.rm = TRUE))

example02 %>%
  group_by(loc, year) %>%
  summarise(yield_loc = mean(yield, na.rm = TRUE))

example02 %>%
  group_by(gen) %>%
  summarise(yield = mean(yield, na.rm = TRUE)) %>% 
  arrange(desc(yield))

example02 %>%
  group_by(gen, loc) %>%
  summarise(yield = mean(yield, na.rm = TRUE)) %>% 
  arrange(desc(yield))

## Count ----------------
## count the number of observations
example02 %>%
  group_by(loc) %>%
  summarise(n = n())

## How to get the missing data if any
example02.missing <- read_csv("Example-02-missing.csv")

example02.missing %>% 
  group_by(loc) %>% 
  summarize(Nmissing=sum(is.na(yield)))

# Select the obs with a missing yield
example02.missing %>% 
  filter(is.na(yield))

# The total number of obs is 464
example02.missing %>% 
  filter(!is.na(yield))

## 464 obs = 461 valid + 3 missing
  
is.na(example02.m$yield)
sum(is.na(example02.m$yield))

## Factors --------------

# The function as.factor() convert a variable to a factor
example02$env <- as.factor(example02$env)
example02$loc <- as.factor(example02$loc)
example02$gen <- as.factor(example02$gen)

# This is equivalent to the code below using mutate() and the pipe %>%
example02 <- example02 %>%
  mutate(
    env=factor(env),
    loc=factor(loc),
    gen=factor(gen)
  )

# To display the levels of a factor
levels(example02$loc)

# To get the number of levels of a factor
nlevels(example02$loc)
