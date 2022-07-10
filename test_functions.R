generateSnake = function(board, snake_size)
{
  # generates an initial snake on the board
  #
  # :param board: Non-negative integer 2-array. First element corresponding to number of rows, second 
  # element to number of columns
  # :param snake_size: Non-negative integer. size of the snake.
  #
  # :return: list corresponding to the generated snake on the board 
  
  snake = list(c(round(runif(1, 0, board[1]-1)), round(runif(1,0,board[2]-1))))
  
  mov_derechas = TRUE
  
  while(snake_size != 1)
  {
    if(mov_derechas == TRUE)
    {
      if(snake[[length(snake)]][2] < board[2]-1)
      {
        snake = append(snake, list(c(snake[[length(snake)]][1], snake[[length(snake)]][2]+1)))
        snake_size = snake_size - 1
      }
      else if(snake[[length(snake)]][1] < board[1]-1)
      {
        snake = append(snake, list(c(snake[[length(snake)]][1]+1, snake[[length(snake)]][2])))
        snake_size = snake_size - 1
      }
      else
      {
        mov_derechas = FALSE  
      }
    }
    else
    {
      if(snake[[length(snake)]][2] > 0)
      {
        snake = append(snake, list(c(snake[[length(snake)]][1], snake[[length(snake)]][2]-1)))
        snake_size = snake_size - 1
      }
      else if(snake[[length(snake)]][1] > 0)
      {
        snake = append(snake, list(c(snake[[length(snake)]][1]-1, snake[[length(snake)]][2])))
        snake_size = snake_size - 1
      }
      else
      {
        mov_derechas = TRUE  
      }
    }
  }
  
  return(snake)
}

testSnake = function(board, snake_length, max_depth, trace=FALSE)
{
  # test function to check how works the implemented algorithm 
  #
  # :param board: Non-negative integer 2-array. First element corresponding to number of rows, second 
  # element to number of columns
  # :param snake_size: Non-negative integer. size of the snake.
  # :max_depth: Non-negative integer. Maximum length of the path which can reach the snake.
  # :trace: Boolean. If true prints information of the different depths tests.
  #
  # :return: Data-frame. Different execution times (in seconds) for different depth sizes 
  
  if( board[1] < 1 || board[1] > 10 ) stop('board error: board first dimmension not between 1 and 10')
  if( snake_length < 3 || snake_length > 7 ) stop('snake error: size of snake not between 3 and 7')
  if( max_depth < 1 || max_depth > 20 ) stop('max_depth error: max_depth size not between 1 and 20')
  
  results_df = data.frame()

  snake = generateSnake(board, snake_length)
  
  for(depth_size in 1:max_depth)
  {
    startTime = Sys.time()
    
    n_paths = numberOfAvailableDifferentPaths(board,
                                              snake, 
                                              depth_size)
    
    total_time = difftime(Sys.time(), startTime, units="secs")[[1]] 
    total_time = round(total_time, digits=4)
    
    if(trace) print(paste("Depth size: ", depth_size, " | elapsed time (sec): ", total_time, sep = ""))
    
    results_df = rbind(results_df, 
                       data.frame(board=paste(board, collapse = ", "), 
                                  snake_length, 
                                  depth_size,
                                  n_paths,
                                  total_time))
  }
  
  return(results_df)
}
