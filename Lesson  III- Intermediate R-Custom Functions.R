# Lesson-III-Intermediate R-Writing Custom Functions
library(readr)
library(dplyr)
scores<-read_csv("D:/Climate X/Indian Ocean Society/Data Science/Course-15 R Intermediate/scores.csv")
# Write a basic function
x_100<-function(x){
  x*100
}
away_by_100<-x_100(scores$away_goals)
away_by_100 # check results
# Write a simple function on scores dataframe
percentage_of_total<-function(x){
  x/sum(x)*100
}
home_goals_percentage<-percentage_of_total(scores$home_goals)
away_goals_percentage<-percentage_of_total(scores$away_goals)
home_goals_percentage # Confirm results
away_goals_percentage
# Function with two variables
 ## calculate the percentage of total goals scored by a team for each match
percentage_goals<-function(x,y){
  x/(x+y)*100
}
home_percent<-percentage_goals(scores$home_goals,scores$away_goals)
away_percent<-percentage_goals(scores$away_goals,scores$home_goals)
home_percent # check results
away_percent
# Writing conditional functions
 ## Above function yields some NAN values due to 0 in denominator
 ## We will write a function wuth conditional execution to return 0 instead of NAN
 ## functions for conditional execution can't be called on vectors — just individual values
percentage_no_na<-function(x,y){
  if ((x+y)>0){
    x/(x+y)*100
  } else {
    0
  }
}
percentage_no_na(scores$home_goals[17],scores$away_goals[17]) # check on 17th match where no goal was scored.
# Functions with more than 2 arguments
adjust_yz<-function(x,y,z){
  if (x>=5 & x<=10){
    y*2
  } else{
    z-1
  }
}
adjust_yz(scores$match_id[1],scores$home_goals[1],scores$away_goals[1])
match_one<-scores$away_goals[1] # check results
adjust_yz(scores$match_id[5],scores$home_goals[5],scores$away_goals[5])
match_five<-scores$home_goals[5] # check results
