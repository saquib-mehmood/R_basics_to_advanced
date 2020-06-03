# Lesson II-Intermediate R-Vectorized Functions
# install readr and dplyr packages and read the csv file
library(readr)
library(dplyr)
scores<-read_csv("D:/Climate X/Indian Ocean Society/Data Science/Course-15 R Intermediate/scores.csv")
View(scores)
dim(scores)# checking dimensions of dataframe
# An example: comparing elapsed time for calculating sum without and with vetorized functions
 ## Calculate sum of numbers from 1 to 1 million
  ### Using a for loop
system.time({
  sum_nums<-0
  for (i in 1:1000000){
    sum_nums<-sum_nums+i
  }  
})
  ### Using vectorized function
system.time({
  sum(1:1000000)
})  
# Using sum function is faster than using a for loop
# Vectorized selection control using if-else from dplyr package
tied_matches<-if_else(scores$home_goals==scores$away_goals,'tie','did not tie')
tied_matches
# Nested if-else function to cater for multiple conditions
 ## Create a vector home_team_result based on following conditions:
home_team_result<-if_else(scores$home_goals>scores$away_goals,"win", if_else(scores$home_goals<scores$away_goals,"lose", if_else(scores$home_goals==scores$away_goals,"tie",""))) 
# add column to score and save dataframe as score_2

scores_2<-cbind(scores,home_team_result)
View(scores_2)
# Calculate total wins, losses and ties for home teams using for loop
win_total <- 0
lose_total <- 0
tie_total <- 0
for (i in 1:nrow(scores_2)) {  
  if (scores_2$home_team_result[i] == "win") {
    win_total <- win_total + 1 
  } else if (scores_2$home_team_result[i] == "lose") {
    lose_total <- lose_total + 1 
  } else if (scores_2$home_team_result[i] == "tie") {
    tie_total <- tie_total + 1
  }
}
win_total # display results
lose_total
tie_total
# Alternately calculate total wins, losses and ties for home teams using sum function

sum(scores_2$home_team_result=="win")
sum(scores_2$home_team_result=="lose")
sum(scores_2$home_team_result=="tie")
# Split, Apply, Combine Problems - Grouping and Summarizing dataframes
 ## Using group_by and summarize functions of dplyr
 ### After group_by, we can use n(), which counts the number of data frame rows in each group. 
 ### The n() function is specific to dplyr, and can only be used within dplyr functions
 ### summarize () can be followed by n() function orothers such as mean() or sum()
# group_by home_country variable and summarize total home_goals using sum
home_goals_sum<-scores_2 %>%
  group_by(home_country) %>%
  summarize(sum(home_goals))
scores_2 %>%
  ungroup() # Ungroup after performing operations
# group_by variable home_team_result and count wins, loses and ties and mean
scores_2 %>%
  group_by(home_team_result) %>%
  summarize(total=n()) %>%
scores_2 %>%
  ungroup()
scores_2 %>%
  group_by(home_team_result) %>%
  summarize(avg_home_goals=mean(home_goals))
scores_2 %>%
  ungroup()
# group_by and summarize with multiple variables
 ## Variable columns are arranged in the output data frame in the order that they are listed in group_by()
 ## If we want to calculate wins, loses and ties for each home_country
scores_2 %>%
  group_by(home_country,home_team_result) %>%
  summarize(total=n())
scores_2 %>%
  ungroup()
## We can also specify multiple operations within the summarize() function at once
scores_2 %>%
  group_by(home_country) %>%
  summarize(min = min(home_goals), 
            max = max(home_goals), 
            avg = mean(home_goals))
# Create a dataframe away_results containing total (sum), average, minimum, and maximum away goals for each away country.
away_results<-scores_2 %>%
  group_by(away_country) %>%
  summarize(total=sum(away_goals),average=mean(away_goals),min=min(away_goals),max=max(away_goals))
away_results # check results
# Chaining Functions together using pipe operator %>%
 ## pipe %>% allows us to write code so that the output of a function is passed to the next function from left to right.
# Create dataframe brazil_goals in which total goals (home and away) are arranged in descending order
brazil_goals<-scores %>%
  filter(home_country=="Brazil" | away_country=="Brazil") %>% 
  mutate(total_goals=home_goals+away_goals)%>%
  arrange(desc(total_goals))
View(brazil_goals) # confirm
