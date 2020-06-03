# Lesson V-IntermediateR-String Manipulation
library(readr)
library(dplyr)
scores<-read_csv("D:/Climate X/Indian Ocean Society/Data Science/Course-15 R Intermediate/scores.csv")
# install tidyverse stringr package
install.packages("stringr")
library(stringr)
# Subsetting strings by position str_sub (1..n) 0r (-1...-n)
 ## Extract a vector containing month when match was played from match_date variable
match_month<-str_sub(scores$match_date,-4,-1) #indexing form end of string
match_month
# Above code won't work if months are of different length
# Splitting Strings
 ## str_split() function is used to split a string into pieces. The place where the string is split, such as a space, a comma, or another character or characters, is referred to as the delimeter
 ## str_split() output is a list
 ## Using the kwarg simplify = TRUE, the output can be simplified to a matrix
 ## We will  use the str_split() function to separate the match_date variable of the scores data frame into day and month.
match_matrix<-str_split(scores$match_date," ",simplify=TRUE)
scores<-scores %>%
  mutate(match_month=match_matrix[,2]) #add match_month column from match_matrix
# Calculating average goals per month using group_by and summarize
total_goals<-scores$home_goals+scores$away_goals
scores<-cbind(scores,total_goals) # adding total_goals column to scores dataframe
avg_monthly_goals<-scores %>%
  group_by(match_month) %>%
  summarize(mean(total_goals))
avg_monthly_goals # confirm results
# Combining Strings with str_c(); use sep="xxx" to insert chrs between combining strings
 ##  Create a new variable, lose_country, for the losing team.
lose_func<-function(x,y,z) {
  if_else (x==y,z,y)
} # write custom function  
library(purrr)
arg_list<-list(x=scores$win_country,y=scores$home_country,z=scores$away_country)
lose_country<-pmap_chr(arg_list,lose_func) # apply function to create a vector
scores<-cbind(scores,lose_country) # add column to scores
team_results<-str_c(scores$win_country," beat ",scores$lose_country) #combine string using str_c
# Alternate code using dplyr
# scores <- scores %>% 
#    mutate(lose_country = if_else(win_country == home_country, away_country, home_country))

#team_results <- str_c(scores$win_country, " beat ", scores$lose_country)
team_results #confirm results
# String manipulation for reformatting match dates
 ## Extract match day
match_matrix <- str_split(scores$match_date, " ", simplify = TRUE) # simplify=True returns a matrix
match_day<-match_matrix[,1] 
 ## Padding strings i.e. adding 0 to singlle digits using str_pad
 ## str_pad adds characters to an existing string to make it a specified length.
 ## e.g. We can use str_pad() to specify addition of a 0 to the left of every string with a length of less than two.
 ## The function takes as arguments:
   ### The string you're working with
   ### The number of digits you want the string to be
   ### The side you want to add characters to
   ### The character(s) you want to add
   ### e.g. vector_2 <- str_pad(vector_1, 2, side = "left", pad = "0")
   ### We can check the length of all strings in a vector by using the function str_length()
match_matrix <- str_split(scores$match_date, " ", simplify = TRUE)
match_day <- match_matrix[,1]
match_day<-str_pad(match_day,2,side="left",pad="0") # use str_pad
scores<-scores %>%
  mutate(match_day=match_day)
match_day_length<-str_length(scores$match_day) #check if length of match_day is 2 characters
match_day_length #confirm results
# We have to create a match summary of the following pattern:
 ## "Argentina beat Switzerland on July 01 2014 with scores of 1 to 0"
 ## From above, we only don't have the victory margin: "scores of 1 to 0"
 ## We will Create new variables, win_goals, containing the winning country's goals for each match,and;
 ## lose_goals, containing the losing country's goals for each match.
 ## And add both variables to scores dataframe
margin_func<-function(w,x,y,z){
  if_else(w==x,y,z)
}
args_list_win=list(w=scores$win_country,x=scores$home_country,y=scores$home_goals,z=scores$away_goals)
args_list_lose=list(w=scores$win_country,x=scores$home_country,y=scores$away_goals,z=scores$home_goals)
win_goals<-pmap_dbl(args_list_win,margin_func) # win goals vector
lose_goals<-pmap_dbl(args_list_lose,margin_func) # lose goals vector
scores<-scores %>%
  mutate(win_goals=win_goals,lose_goals=lose_goals) # add to scores dataframe
 ## Alternately, we can use following code:
  ### scores <- scores %>% 
  ### mutate(win_goals = if_else(home_country == win_country, home_goals, away_goals),
  ###     lose_goals = if_else(home_country == win_country, away_goals, home_goals))
View(scores)
# Creating match summary using str_c on folloing format:
##  "Argentina beat Switzerland on July 01 2014 with scores of 1 to 0"
library(stringr)
match_summary<-str_c(scores$win_country,'beat', scores$lose_country,'on',scores$match_month,scores$match_day,'2014 with scores of',scores$win_goals,'to',scores$lose_goals,sep=' ')
match_summary #confirm results
