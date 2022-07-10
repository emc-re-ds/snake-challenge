updateSnake = function(snake, cell)
{
  # move snake across the board
  #
  # :param cell: new cell where the snake will move 
  # :param snake: list of pairs which contains the coordinates of each cell
  # where the snake is placed
  #
  # :return: list corresponding to the new position on the board 
  
  snake = c(list(cell), snake[1:length(snake)-1])
  
  return(snake)
}

allowedCells = function(snake, board)
{
  # all possible cells where the snake is allowed to go
  #
  # :param board: Non-negative integer 2-array. First element corresponding to number of rows, second 
  # element to number of columns
  # :param snake: list of pairs which contains the coordinates of each cell
  # where the snake is placed
  #
  # :return: number of possible cells where the nake is allowed to go
  
  possible_cells = list()
  
  # the movements happen simultaneously
  # so the head could take the place of the tail
  for(i in c(-1,1))
  {
    # row movement
    row_move = i + snake[[1]][1]
    if(row_move >= 0 && row_move < board[1])
    {
      # check if there is self-intersection
      if(list(c(row_move, snake[[1]][2])) %in% snake[1:length(snake)-1] == FALSE)
      {
        possible_cells = append(possible_cells, list(c(row_move, snake[[1]][2])))
      }
    }
    
    # column movement
    col_move = i + snake[[1]][2] 
    if(col_move >= 0 && col_move < board[2])
    {
      # check if there is self-intersection
      if(list(c(snake[[1]][1], col_move)) %in% snake[1:length(snake)-1] == FALSE)
      {
        possible_cells = append(possible_cells, list(c(snake[[1]][1], col_move)))
      }
    }
  }
  
  return(possible_cells)
}

numberOfAvailableDifferentPaths = function(board, snake, depth)
{
  # using a recursive-backtracking approach to explore all possible solutions 
  #
  # :param board: Non-negative integer 2-array. First element corresponding to number of rows, second 
  # element to number of columns
  # :param snake: list of pairs which contains the coordinates of each cell
  # where the snake is placed
  # :param depth: Non-negative integer. Maximum length of the different paths that snake can take
  # from his initial position, other paths will be discarted
  #
  # :return: number of distinct valid paths that the snake can make
    
  result = 0
  
  # if there are no more movements to do, then the path has reach the end and
  # return back to check more paths
  if(depth == 0)
  {
    return(1)
  }
  else
  {
    # for each available cell where the snake can move into, the 
    # recursive call will be made.
    allowed_cells = allowedCells(snake, board)
    for(c in allowed_cells)
    {
      result = (result + numberOfAvailableDifferentPaths(board, updateSnake(snake, c), depth-1))%%(1000000007)
    }
  }
  
  return(result)
}
