Lesson IV-Intermediate R-Functionals
library(readr)
library(dplyr)
scores<-read_csv("D:/Climate X/Indian Ocean Society/Data Science/Course-15 R Intermediate/scores.csv")
# Base R includes a family of functionals, known as the "apply" family
 ## the output of these functionals is not standardized, hence we will use the purrr package from tidyverse
library(purrr)
 ## map functional : map returns a list after application of elements
match_results <- list(c(3,1), c(1,0), c(1,5), c(3,1), c(3,0))
names(match_results) <- c("match 1", "match 2", "match 3", "match 4", "match 5")
match_results
min_scores<-map(match_results,min)
min_scores # confirm results
# Using functionals to apply custom functions to more than one variable
percentage_of_total <- function(x) { 
  x/sum(x) * 100
} # custom function
library(dplyr)
library(purrr)
per_goals_output<-scores %>%
  select(home_goals,away_goals) %>%
  map(percentage_of_total)
per_goals_output # check output; map functional always returns a list and retains the names and structure of its inputs
# Functionals to return vectors of specified types
 ## map_lgl() returns a logical vector
 ## map_int() returns an integer vector
 ## map_dbl() returns a double vector
 ## map_chr() returns a character vector
 ## If we apply map_dbl, it will return numeric output
sum_match<-map_dbl(match_results,sum)
sum_match
typeof(sum_match) # check data type
 ## Apply map_chr
sum_chr<-map_chr(match_results,sum)
sum_chr
typeof(sum_chr) # data tyepe returned is character
# Functionals for 2 variable functions map2()
percentage_no_na <- function(x,y) {
  if(x + y > 0) {
    xy_total = x + y 
    (x/xy_total) * 100
  } else {
    0
  }
} # custom function with 2 variables
percent_goals_home<-map2(scores$home_goals,scores$away_goals,percentage_no_na)
percent_goals_home # returns a list
# Functionals to return vectors of specified types with 2 variables functions  . We can use:
 ## map2_lgl() returns a logical vector
 ## map2_int() returns an integer vector
 ## map2_dbl() returns a double vector
 ## map2_chr() returns a character vector
 ## Use map2_dbl to create a new variable containing the percentage of total goals scored by the home team for each match.
percentage_no_na <- function(x,y) {
  if(x+y > 0) {
    total_goals = x+y 
    (x/total_goals) * 100
  } else {
    0
  }
} # custom function with 2 variables
percent_goals_home<-map2_dbl(scores$home_goals,scores$away_goals,percentage_no_na)
scores_2<-scores %>%
  mutate(percent_goals_home=percent_goals_home) # New dataframe with newly created vector
View(scores_2) # confirm results
# Functionals to return vectors of specified types with more than 2 variables functions  . We can use:
 ## pmap() functional is somewhat different from working with map() and map_2() because function arguments are provided as a list
 ## We can also use pmap varaints to return vectors of specific types:
  ### pmap_lgl() returns a logical vector
  ### pmap_int() returns an integer vector
  ### pmap_dbl() returns a double vector
  ### pmap_chr() returns a character vector
adjust_yz_2 <- function(x,y,z) {
  if (x >= 5 & x <= 10) {
    y * 2 
  } else {
    z - 1
  }
} # create a 3 variable custom function
args_list<-list(x=scores$match_id,y=scores$home_goals,z=scores$away_goals) # create an arguments list for use with pmap_dbl()
adjusted_scores<-pmap_dbl(args_list,adjust_yz_2) # store results in a vector
adjusted_scores # confirm results
