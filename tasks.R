## Project Autumn 2022 ## Task 1
## Samir kandel
## SID- 19932024

library(tidyverse)

## Reading the csv file that we are analyzing and assigning it to a variable "d"

d <- read.csv("~/AnalyticsProgramming/APproject/competitionResults.csv")

## Info:
## Computing total score by each team for each match
## Note that each goal is worth 10 points....
## and the team that caught the snitch is awarded an extra 50 points. 
## TO-DO----------->>>>>>>
## Result in a table showing: 
## season, round number, match number, home team, away team, home score, away score

## Using factor to convert round and Season variables into categorical variable

d$round <- as.factor(d$round)
d$Season <- as.factor(d$Season)

## Function

## Function: Calculate individual team score (without snitch score)

calculatescore <- function(goals){
    return(goals * 10)
} 


## Adding totalHomeScore and totalAwayScore columns while producing new dataset(modifyd).

modifyd <- d %>% mutate(totalHomeScore = calculatescore(homeGoals), totalAwayScore= calculatescore(awayGoals))

## Including points for snitch in the scoreboard. 

## count function counts the number of rows of data in the dataset.
  ## Then storing it in count variable. 

count <- count(modifyd)

## Going through each row in the "modifyd" dataset to include points for "snitch".

for (num in 1:count$n){
  
## Comparing teams whether who caught "snitch" 
  
  ## Adding a match number column to identify each match of the season
  modifyd$matchNumber[num] <- num
  
  ## If a team caught snitch, incrementing its scoreboard by 50 points.
  if (modifyd$homeTeam[num] == modifyd$snitch[num]){
    modifyd$totalHomeScore[num] = modifyd$totalHomeScore[num] + 50
  }
  if (modifyd$awayTeam[num] == modifyd$snitch[num]){
    modifyd$totalAwayScore[num] = modifyd$totalAwayScore[num] + 50
  }
}

## Result in a table showing: 
## season, round number, match number, home team, away team, home score, away score

modifyd %>% select(Season, round, homeTeam, awayTeam, totalHomeScore, totalAwayScore) 


########################Task 2################################

## INFO:
## Pick the season who's last digit that correspond to your SID's final digit (4) 
## Team score 3 points for a win and 1 point for a draw
## TO-DO:
## Compute each team's points after 5 "rounds" of matches and then "all" matches.
## Result in a table: 2 tables
## 1st table, a results ladder with each team's points after 5 "rounds" of matches
## 2nd table, a results ladder with each team's points after all matches 
## For  team: 

## Loop to calculate home and away team points for win, draw or lose.

for (num in 1:count$n){
  
  
  ## If a team caught snitch, incrementing its scoreboard by 50 points.
  if (modifyd$totalHomeScore[num] > modifyd$totalAwayScore[num]){
    modifyd$hometeam_points[num] = 3
    modifyd$awayteam_points[num] = 0
  }
  else if (modifyd$totalHomeScore[num] < modifyd$totalAwayScore[num]){
    modifyd$hometeam_points[num] = 0
    modifyd$awayteam_points[num] = 3
  }
  else {
    modifyd$hometeam_points[num] = 1
    modifyd$awayteam_points[num] = 1
  }
}

## Final digit of my SID corresponds with the 2014 season
## Creating a data set "modifySeason" containing all the match stats only from the 2014 season. 

modifySeason <- modifyd %>% filter(Season == 2014)

## creating a list of team names 

team_name = unique(modifySeason$awayTeam)

## creating empty list "team_points" having equal length as " team_name" list 

team_points = rep(0, length(team_name))


############## 1st table: task 2 #####################################

## Creating a loop for each team to get total points on both home and away games
for (n in 1:length(team_name)){
  
  ## Finding the index for home and away team
  awayteam_pos <- which(modifySeason$awayTeam == team_name[n])
  hometeam_pos <- which(modifySeason$homeTeam == team_name[n])
  
  ## team_pos stores indexes of matches when a specific team played 
  team_pos <- sort(c(hometeam_pos, awayteam_pos))
  
  ## Storing a list of home/away points for specific team 
  team_match_point <- c()
  
  ## Looping to pull up specific points for specific team
  ## *****Only for 5 rounds of matches********
  
  for (pos in 1:5){
    
    ##Picks specific match played by a team
    checknum <- team_pos[pos]
    
    ## checking if that specific match was a home game or away
    if(checknum %in% hometeam_pos){
      
      ## Storing point obtained in a home game of a specific team
      match_point <- modifySeason$hometeam_points[checknum]
    }
    else {
      ## Storing point obtained in a away game of a specific team
      match_point <- modifySeason$awayteam_points[checknum]
    }
    
    ## Combining all the points (both home & away) acquired by specific team 
    team_match_point <- c(team_match_point, match_point)
  }
  ## Adding all individual team point to store it as a total team points
  team_points [n] <- sum(team_match_point[])
}

## Adding team names into the list of total team points.
names(team_points) <- team_name

## Result: 2nd table, a results ladder with each team's points after all matches 

sort(team_points, decreasing = TRUE)

################## Task 2: table 1 complete ##############

  ################## Task 2: table 2 ##############

## Creating a loop for each team to get total points on both home and away games
for (n in 1:length(team_name)){
  
  ## Finding the index for home and away team
  awayteam_pos <- which(modifySeason$awayTeam == team_name[n])
  hometeam_pos <- which(modifySeason$homeTeam == team_name[n])
  
  ## Storing sorted indexes of matches when a specific team played 
  team_pos <- sort(c(hometeam_pos, awayteam_pos))
  
  ## Storing a list of home/away points for specific team 
   team_match_point <- c()
  
  ## Looping to pull up specific points for specific team
  
  for (pos in 1:length(team_pos)){
    
    ##Picks specific match played by a team
    checknum <- team_pos[pos]
    
    ## checking if that specific match was a home game or away
    if(checknum %in% hometeam_pos){
      
      ## Storing point obtained in a home game of a specific team
      match_point <- modifySeason$hometeam_points[checknum]
    }
    else {
      ## Storing point obtained in a away game of a specific team
      match_point <- modifySeason$awayteam_points[checknum]
    }
    
    ## Combining all the points (both home & away) acquired by specific team 
    team_match_point <- c(team_match_point, match_point)
  }
  ## Adding all individual team point to store it as a total team points
  team_points [n] <- sum(team_match_point[])
}

## Adding team names into the list of total team points.
names(team_points) <- team_name

## Result: 2nd table, a results ladder with each team's points after all matches 

sort(team_points, decreasing = TRUE)

################## Task 2: table 2 complete ##############
## Testing commands:
## modifySeason %>% filter(homeTeam == "") %>% select(Season, round, homeTeam, awayTeam, hometeam_points, awayteam_points)


########################## TASK 3 ###################################

## Info:
## Home game advantage? 
## Write "a" function: 
## 1. Return a count of home games each team has had
## 2. Total number of games each team has played in 
## 3. Total points scored in a home game 
## 4. Total number points the team scored 
## Provide these result in a data frame. 
## Run the function on the data and show the resulted table output

## Assigning a list of all team to a variable
team_name <- unique(modifyd$homeTeam)

## 1. Return a count of home games each team has had

## Function returning a count of home games each team played.
numTotal_homegame <- function(team_name){
  
  ## Empty list to start with
  countList <- c()
  
  ## Looping through each team 
  for (num1 in 1:length(team_name)){
    
    ## Variable for counting
    countVar = 0
    
    ## Assigning a team name to a variable based on index
    varTeam <- team_name[num1]
    
    ## Looping through the data set
    for (num2 in 1:count$n){
      if (varTeam == modifyd$homeTeam[num2]){
        countVar = countVar + 1
      }
    }
    countList <- c(countList, countVar)
    
  }
  return(countList)
}



## 2. Total number of games each team has played in 

numTotal_game <- function(team_name){
  
  countList <- c()
  
  for (num1 in 1:length(team_name)){
    
    countVar = 0
    varTeam <- team_name[num1]
    
    for (num2 in 1:count$n){
      if ((varTeam == modifyd$homeTeam[num2]) || (varTeam == modifyd$awayTeam[num2])){
        countVar = countVar + 1
      }
    }
    countList <- c(countList, countVar)
    
  }
  return(countList)
  
}



## 3. Total points scored in a home game 

pointsTotal_homegame <- function(team_name){
  countList <- c()
  
  for (num1 in 1:length(team_name)){
    
    countVar = 0
    varTeam <- team_name[num1]
    
    for (num2 in 1:count$n){
      if (varTeam == modifyd$homeTeam[num2]){
        countVar = countVar + modifyd$hometeam_points[num2]
      }
    }
    countList <- c(countList, countVar)
    
  }
  return(countList)
}



## 4. Total number points the team scored 

pointsTotal_game <- function(team_name){
  countList <- c()
  
  for (num1 in 1:length(team_name)){
    
    countVar = 0
    varTeam <- team_name[num1]
    
    for (num2 in 1:count$n){
      if (varTeam == modifyd$homeTeam[num2]){
        countVar = countVar + modifyd$hometeam_points[num2]
      }
      if (varTeam == modifyd$awayTeam[num2]){
        countVar = countVar + modifyd$awayteam_points[num2]
      }
    }
    countList <- c(countList, countVar)
  }
  return(countList)
}

## Creating a data frame 
df <- data.frame( 
  teamnames = team_name,
  totalHomeGame = numTotal_homegame(team_name),
  totalNumGame = numTotal_game(team_name),
  totalPointsHomeGame = pointsTotal_homegame(team_name),
  totalPointsGame = pointsTotal_game(team_name)
  )

## Resulted output:
df



###################### task 4 ################################

## TO-DO:
## Provide a table with the team statistics for all seasons
## Including but not limited to:

## Number of games, Number of wins, Winning Percentage
## Points scored, Points conceded, Number of Snitch catches
## Goals scored, number of tournament wins
## order by points scored

## Assigning a list of all team to a variable
team_name <- unique(modifyd$homeTeam)

## 1. Function returning total number of games played by each team.
numTotal_games <- function(team_name){
  
  ## Empty list to start with
  countList <- c()
  
  ## Looping through each team 
  for (num1 in 1:length(team_name)){
    
    ## Variable for counting
    countVar = 0
    
    ## Assigning a team name to a variable based on index
    varTeam <- team_name[num1]
    
    ## Looping through the data set
    for (num2 in 1:count$n){
      if ((varTeam == modifyd$homeTeam[num2]) || (varTeam == modifyd$awayTeam[num2])){
        countVar = countVar + 1
      }
    }
    countList <- c(countList, countVar)
    
  }
  return(countList)
}



##2. Function returning total number of wins by each team.

numTotal_win <- function(team_name){
  countList <- c()
  
  for (num1 in 1:length(team_name)){
    
    countVar = 0
    varTeam <- team_name[num1]
    
    for (num2 in 1:count$n){
      if (varTeam == modifyd$homeTeam[num2]){
        if(modifyd$hometeam_points[num2] == 3){
          countVar = countVar + 1
        }
      }
      if (varTeam == modifyd$awayTeam[num2]){
        if(modifyd$awayteam_points[num2] == 3){
          countVar = countVar + 1
        }
      }
    }
    countList <- c(countList, countVar)
  }
  return(countList)
}

##3.Function returning winning percentage for each team.

percent_win <- function(team_name){
  countList <- c()
  
  for (num1 in 1:length(team_name)){
    
    countVar = 0
    varTeam <- team_name[num1]
    
    for (num2 in 1:count$n){
      if (varTeam == modifyd$homeTeam[num2]){
        if(modifyd$hometeam_points[num2] == 3){
          countVar = countVar + 1
        }
      }
      if (varTeam == modifyd$awayTeam[num2]){
        if(modifyd$awayteam_points[num2] == 3){
          countVar = countVar + 1
        }
      }
    }
    countList <- c(countList, countVar/result$totalGame[num1])
  }
  return(countList)
}

##4. Function returning total points scored for each team.

pointsTotal_game <- function(team_name){
  countList <- c()
  
  for (num1 in 1:length(team_name)){
    
    countVar = 0
    varTeam <- team_name[num1]
    
    for (num2 in 1:count$n){
      if (varTeam == modifyd$homeTeam[num2]){
        countVar = countVar + modifyd$hometeam_points[num2]
      }
      if (varTeam == modifyd$awayTeam[num2]){
        countVar = countVar + modifyd$awayteam_points[num2]
      }
    }
    countList <- c(countList, countVar)
  }
  return(countList)
}

##5. Function returning total points conceded by each team.

points_conceded <- function(team_name){
  countList <- c()
  
  for (num1 in 1:length(team_name)){
    
    countVar = 0
    varTeam <- team_name[num1]
    
    for (num2 in 1:count$n){
      if (varTeam == modifyd$homeTeam[num2]){
        countVar = countVar + modifyd$awayteam_points[num2]
      }
      if (varTeam == modifyd$awayTeam[num2]){
        countVar = countVar + modifyd$hometeam_points[num2]
      }
    }
    countList <- c(countList, countVar)
  }
  return(countList)
}

##6. Function returning number of snitch catches by each team.

num_snitch <- function(team_name){
  countList <- c()
  
  for (num1 in 1:length(team_name)){
    
    countVar = 0
    varTeam <- team_name[num1]
    
    for (num2 in 1:count$n){
      if (varTeam == modifyd$snitch[num2]){
        countVar = countVar + 1
      }
    }
    countList <- c(countList, countVar)
  }
  return(countList)
}

##7: Function returning number of goal scored by each team.

num_goals <- function(team_name){
  countList <- c()
  
  for (num1 in 1:length(team_name)){
    
    countVar = 0
    varTeam <- team_name[num1]
    
    for (num2 in 1:count$n){
      if (varTeam == modifyd$homeTeam[num2]){
        countVar = countVar + modifyd$homeGoals[num2]
      }
      if (varTeam == modifyd$awayTeam[num2]){
        countVar = countVar + modifyd$awayGoals[num2]
      }
    }
    countList <- c(countList, countVar)
  }
  return(countList)
}

##8. Function returning number of tournament wins by each team.


## Creating a data frame
result <- data.frame( 
  
  teamnames = team_name,
  
  totalGame = numTotal_game(team_name),
  
  totalWin = numTotal_win(team_name),
  
  ##winPercentage = percent_win(team_name),
  
  teamPoints = pointsTotal_game(team_name),
  
  pointsConceded = points_conceded(team_name),
  
  numOfSnitch = num_snitch(team_name),
  
  totalGoals = num_goals(team_name)
  
  ##tournamentWin
  
)

##8.Function returning number of tournament win.

tournament = unique(modifyd$Season)
team_name = unique(result$teamnames)
winnerList = rep(0, length(team_name))

for (num1 in 1:length(tournament)){
  
  modifyT <- modifyd %>% filter(Season == tournament[num1])
  
  ## creating empty list "team_points" having equal length as " team_name" list 
  #team_points = rep(0, length(team_name))
  team_points = rep(0, length(team_name))

  
  ## Creating a loop for each team to get total points on both home and away games
  for (n in 1:length(team_name)){
    
    ## Finding the index for home and away team
    awayteam_pos <- which(modifyT$awayTeam == team_name[n])
    hometeam_pos <- which(modifyT$homeTeam == team_name[n])
    
    if (length(awayteam_pos) == 0){
      team_match_point [n]<- 0
      next
    }
    ## team_pos stores indexes of matches when a specific team played 
    team_pos <- sort(c(hometeam_pos, awayteam_pos))
    
    ## Storing a list of home/away points for specific team 
    team_match_point <- c()
    
    ## Looping to pull up specific points for specific team
    
    for (pos in 1:length(team_pos)){
      
      ##Picks specific match played by a team
      checknum <- team_pos[pos]
      
      ## checking if that specific match was a home game or away
      if(checknum %in% hometeam_pos){
        
        ## Storing point obtained in a home game of a specific team
        match_point <- modifyT$hometeam_points[checknum]
      }
      else {
        ## Storing point obtained in a away game of a specific team
        match_point <- modifyT$awayteam_points[checknum]
      }
      
      ## Combining all the points (both home & away) acquired by specific team 
      team_match_point <- c(team_match_point, match_point)
    }
    ## Adding all individual team point to store it as a total team points
    team_points [n] <- sum(team_match_point[])
  }
  
  ## Selecting the index of a season winner
  seasonWinnerIndex <- match(max(team_points), team_points[])
  winnerList <- c(seasonWinnerIndex, winnerList)
}

## Time to add data into the data frame for tournaments wins

for (var1 in 1:length(result$teamnames)){
  counter = 0
  
  for (var2 in 1:length(winnerList)){
    
    ## If a team have won tournaments, incrementing its scoreboard by 1 points.
    if (var1 == winnerList[var2]){
      counter = counter + 1
    }
    result$tournamentWin[var1] = counter
  }
}

################ Showing final result of scoreboard ################

result[order(result$teamPoints, decreasing = TRUE), ]

###################### End of Assessment ##########################

## Final Submission: APProject-COMP1013-19932024.pdf
