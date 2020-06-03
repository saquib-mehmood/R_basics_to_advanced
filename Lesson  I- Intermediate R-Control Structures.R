# Lesson - Intermediate Level R Programming - I 

# control Structures
# Install tidyverse Packages
library(readr)
library(dplyr) 
# Import data file 'scores.csv) from (https://www.kaggle.com/fivethirtyeight/world-cup?select=scores.csv)
scores<-read_csv('D:/Climate X/Indian Ocean Society/Data Science/Course-15 R Intermediate/scores.csv')
View(scores)
# For 1st Five Rows - Add new column whether home team won or not
scores_five <- scores[1:5,]
# create a vector
home_team_won <- c('home team won','home team won','home team did not win','home team won','home team won')
# Add vector as column to dataframe scpres_five
scores_five<-scores_five %>%
  mutate(home_team_won=home_team_won)
# check whether column added
View(scores_five)
# Writing Conditional Statements and For Loops
if (scores$home_goals[3] > scores$away_goals[3]) 
{  
  print('home team won') 
} else { 
  print('home team did not win') # note positions of braces & indents
}
 ## Writing conditional statement for entire dataframe using For loop
 ## First practice writing a for loop for `home country` column
for (c in scores$home_country) {
  print(c)
}    
 ## Performing for loop on rows of dataframe to determine victory/defeat margin
for (i in 1:nrow(scores)) {
  print(scores$home_goals[i]-scores$away_goals[i])
}  
# Nested Control Structures (Combining for loop and conditional statement)
for (i in 1:nrow(scores)) {
  if (scores$home_goals[i]>scores$away_goals[i]) {
    print(TRUE)
  }   else {
    print(FALSE)
  }
}    ## Note positions of braces and indents
     ## The outer set of braces correspond with the for-loop, and everything in that loop between the braces is indented once.
     ## Inside that, there are braces for the if statement (with the code inside them indented a second time), 
     ## and braces for the else statement (with the code inside them indented a second time).
# Storing nested output in objects
home_team_won<-c() # empty vector
for (i in 1:nrow(scores)){
  if (scores$home_goals[i]>scores$away_goals[i]){
    home_team_won<-c(home_team_won,TRUE)
  } else{
    home_team_won<-c(home_team_won,FALSE)
    }
}    
home_team_won # check output
# We can add the vector as a column to dataframe:
  ## By either using `cbind` from base, or
  ## using `mutate` from `dplyr'
scores<-scores %>% 
  mutate(home_team_won=home_team_won)
View(scores) # check if column is added to dataframe
# Nested outputs for more than two cases using `else if` statement
 ## Third case is when match was a tie
 ## Check code for 3rd row of scores
if (scores$home_goals[3]>scores$away_goals[3]){
  print('win')
} else if (scores$home_goals[3]<scores$away_goals[3]) {
  print('lose') 
  } else if (scores$home_goals[3]==scores$away_goals[3]) {
  print('tie')  
    }  
 ## Write a nested for loop for `scores` to cater for all 3 conditions
home_team_result<-c() # empty vector
for (i in 1:nrow(scores)){
  if (scores$home_goals[i]>scores$away_goals[i]){
    home_team_result<-c(home_team_result,'win')
  } else if (scores$home_goals[i]<scores$away_goals[i]){
    home_team_result<-c(home_team_result,'lose')
  } else if (scores$home_goals[i]==scores$away_goals[i]){
    home_team_result<-c(home_team_result,'tie') 
  }
}    
scores<-cbind(scores,home_team_result) # use `cbind` instead of mutate this time.
# Summary
# We've learned to use two types of control structures to direct the flow of our code:
`# If-else` statements
# `For`-loops
# We've been empowered to write code that saves time and provides you with increased flexibility for solving data analysis problems. If-else statements and for-loops are programming tools common to practically all programming languages, and it's important to understand how they work and when to use them.

# However, R is characterized by some special features we
#'ll often find superior to for-loops when it comes to repeating the same operation on multiple elements through use of `vectorized functions`

