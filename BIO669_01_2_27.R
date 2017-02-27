# -------------------------------------------------------------------------------
# BIOL 669 - Theory of Ecological Communities
# 
# This code helps introduce some basic concepts in R including for loops and
# sampling, as well as some basic data manipulation, using class-collected
# data on the prevalence of different types of ecological processes investigated
# in the literature.
#
# Allen Hurlbert


library(gsheet)
library(dplyr)
library(tidyr)

# URL copied from address bar
url = "https://docs.google.com/spreadsheets/d/1KMU6Sx1A3nlMORnEAydqXnv41PEuy8xxBDyfa_vPAWM/edit#gid=22700915"
fullread = gsheet2tbl(url)

# Subset to the relevant columns
df = fullread[, c("Journal", "Year", "Selection", "Drift", "Dispersal", "Speciation")]
head(df)

# Replace NAs with 0
df[is.na(df)] = 0

# Check for math errors
rowSums(df[,3:6])

df = df[rowSums(df[,3:6]) > 0, ] #all rows where total is 100
str(df)
summary(df)
unique(df$Journal) #7 entries for 4 journals! gross! 


# Standardize journal names using gsub(). What about 'oikos' vs 'Oikos'?
df$Journal = gsub("American Naturalist", "Am Nat", df$Journal)
df$Journal = gsub("Ecology ", "Ecology", df$Journal)
df$Journal = gsub("EcologyLetters", "Ecology Letters", df$Journal)
df$Journal = gsub("oikos", "Oikos", df$Journal)
df$Year = as.numeric(gsub("2016", "2017", df$Year))
table(df$Journal)

# Total number of papers examined (just copy from Google sheet)
names(df)
nums = df[1:4,c("Journal.1", "X1985", "X2002", "X2017")]
names(nums)[1] = "Journal"
nums

j.yr = gather(nums, key = Year, value = ntot, -Journal)
j.yr$Year = as.numeric(substr(j.yr$Year, 2, 5)) #could also gsub "" for "x"


####Data summary and exploration####

# Analysis - what are the questions?


# First, how about a tally of the number of *community ecology* papers by Journal/Year
j.yr.ct = df %>% 
  count(Journal, Year) %>% 
  full_join(j.yr) %>%
  mutate(pct = n/ntot)


#what about relative dist bet/4 cats in ecology 
#-----------------------------------------------------------------------------------
### R Skillz

## I. for loops

# For each journal, do a simple analysis

jnames = unique(df$Journal) #journal names

for (j in jnames) {
  j.df = subset(df, Journal == j)
  j.means = apply(j.df[, 3:6], 2, mean)
  print(j)
  print(j.means)
}

# TASK 1: rewrite the loop above so that the values are stored in a single dataframe
# with columns for journal, selection, drift, dispersal, and speciation

t1_df = data.frame(journal = NULL, selection = NULL, drift = NULL, dispersal = NULL, speciation = NULL)

for (j in jnames) {
  j.df = subset(df, Journal == j)
  j.means = apply(j.df[, 3:6], 2, mean)
  temp = data.frame(journal = j, 
                    selection = j.means[1],
                    drift = j.means[2],
                    dispersal = j.means[3],
                    speciation = j.means[4])
  t1_df = rbind(temp, t1_df) #reconsider using rbind for big datasets
  
}
row.names(t1_df) =t1_df$journal


# TASK 2: use a for loop to calculate the average prevalence of each of the 4 processes
# by journal and by time period, and store output in a dataframe like above but with
# a year column as well.

# TASK 2a (optional): Perform TASK 2 without using loops! (maybe dplyr...)


## II. sampling
rand = round(100*runif(100), 0) #round to 0 #let's say these are spp ids 
table(rand) #spp and abun in spp pool, we want to sample at ran from spp pool 
#sample from the vector rand
rand = round(rlnorm(100, 3, 1), 0)


sample(rand, 10, replace = TRUE) #2nd arg is just the size of our sample 
#want to consider if you are sampling with or without replacement 

# sample_n() is used to randomly draw a specified number of elements from a dataframe or table
# with or without replacement (default is without).
# replace = TRUE for simulation modeling, which needs to be specified, to keep samples independent 


sample_n(df, 1)
sample_n(df, 1)
sample_n(df, 3)

# TASK 3: Use a for loop and sample_n to calculate the average percent selection
# expected in a sample of 20 Ecology papers. Note: to get the mean expectation, you
# will want to try sampling 10 papers many times.

# TASK 4: Re-do TASK 3, but use 'replace = TRUE'. Do you get the same answer? When
# should it matter?


