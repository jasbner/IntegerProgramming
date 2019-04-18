library(tidyverse)
library(lpSolve)

df <- data_frame(avg_points = sample(5:45,1000, replace = T),
                 cost = sample(3:45,1000, replace = T),
                 position = sample(c("P","C","1B","2B","3B","SS","OF"),1000, replace = T),
                 one = 1) %>% mutate(id = row_number())

obj <- df$avg_points #set the objective function (what we want to maximize)
# set the constraint rows. contrary to tidy dataframes, each row is what you are summing over
con <- rbind(t(model.matrix(~ position + 0,df)), cost = df$cost) 

#view the constraints
rownames(con)
#set the constraint values
rhs <- c(1,1,1,1,3,2,1,200) #exactly 3 outfielders 2 pitchers and 1 of everything else at a cost less than 200

#set the direction of the constraints
dir <- c("=","=","=","=","=","=","=","<=")

result <- lp("max",obj,con,dir,rhs,all.bin = TRUE)

result
