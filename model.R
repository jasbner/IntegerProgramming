library(tidyverse)
library(lpSolve)
set.seed(123)
N_players <- 1000
df <- data_frame(avg_points = sample(5:45,N_players, replace = T),
                 cost = sample(3:45,N_players, replace = T),
                 position = sample(c("P","C","1B","2B","3B","SS","OF"),N_players, replace = T),
                 team = sample(LETTERS,N_players, replace = T)) %>% mutate(id = row_number())

#get number of teams to set a new objective function that applies to the team variable
N_teams = length(unique(df$team))

#set the objective function (what we want to maximize)
# note we need 2 additional objective functions that deal with team size but will be set to 0
obj <- c(df$avg_points, rep(0, 2 * N_teams)) 

# set the constraint rows with added constants for y and z.
c1 <- t(model.matrix(~ position + 0,df))
c1 <- cbind(c1, 
            matrix(0, ncol = 2 * N_teams, nrow = nrow(c1)))
c2 = df$cost
c2 <- c(c2, rep(0, 2 * N_teams))
c3 = t(model.matrix(~ team + 0, df))
c3 <- cbind(c3, matrix(0, ncol = 2 * N_teams, nrow = nrow(c3)))
# Since you want to have at least 3 teams, you will first use y to count the number of players per team:
# This constraint counts the number of players per team.
# You sum up all players of a team that you have picked and substract the corresponding y variable per team.
# This should be equal to 0. (diag() creates the identity matrix, we do not worry about z at this point):
# should be x1...xn - y1...n = 0
c4_1 <- cbind(t(model.matrix(~team + 0, df)), # x
              -diag(N_teams), # y
              matrix(0, ncol = N_teams, nrow = N_teams) # z
) # == 0
# Since each y is now the number of players in a team, you can now make sure that z is binary with this constraint:
c4_2 <- cbind(t(model.matrix(~ team + 0, df)), # x1+...+xn ==
              -diag(N_teams), # - (y1+...+yn )
              diag(N_teams) # z binary
) # <= 1

# This is the constraint that ensures that at least 3 teams are picked:

c4_3 <- c(rep(0, nrow(df) + N_teams), # x and y
          rep(1, N_teams) # z >= 3
)

# This constraint is added to make sure all x are binary:

#all x binary
c5 <- cbind(diag(nrow(df)), # x
            matrix(0, ncol = 2 * N_teams, nrow = nrow(df)) # y + z
)


# Create the new constraint matrix

con <- rbind(c1,
             c2,
             c3,
             c4_1,
             c4_2,
             c4_3,
             c5)


#set the constraint values
rhs <- c(1,1,1,1,3,2,1,200, #exactly 3 outfielders 2 pitchers and 1 of everything else at a cost less than 200
         rep(6,N_teams), #max number from any team is 6
         rep(0, N_teams), # c4_1
         rep(1, N_teams), # c4_2
         3, # c4_3
         rep(1, nrow(df))# c5 binary
)


#set the direction of the constraints
dir <- c(rep("=",7), #C1
         "<=", #C2
         rep("<=",N_teams), #C3
         rep('==', N_teams), # c4_1
         rep('<=', N_teams), # c4_2
         '>=', # c4_3 
         rep('<=', nrow(df)) # c5
)

result <- lp("max",obj,con,dir,rhs,all.int = TRUE)


