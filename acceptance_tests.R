source("snake_functions.R")
source("test_functions.R")

###################
# Acceptance tests
###################

# #### TEST 1
board = c(4,3)
snake = list( c(2,2), c(3,2), c(3,1), c(3,0), c(2,0), c(1,0), c(0,0) )
test1 = numberOfAvailableDifferentPaths(board, snake, 3)
# result must be: 7

# #### TEST 2
board = c(2,3)
snake = list( c(0,2), c(0,1), c(0,0), c(1,0), c(1,1), c(1,2) )
test2 = numberOfAvailableDifferentPaths(board, snake, 10)
# result must be: 1

# #### TEST 3
board = c(10,10)
snake = list( c(5,5), c(5,4), c(4,4), c(4,5) )
test3 = numberOfAvailableDifferentPaths(board, snake, 4)
# result must be: 81