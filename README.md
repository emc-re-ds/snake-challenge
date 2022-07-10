# snake-challenge
I did this challenge as an entry submission. Consist in finding distinct valid paths of length p for the snake game.

-----

Consider a rectangular board consisting of n × m cells. There is a snake lying on some of the
cells, and all of the other cells are empty. The snake consists of one or more cells such that
cells with consecutive numbers are either horizontally or vertically adjacent. The first cell is
the snake's head, and the last cell is the snake's tail.

On each turn, the snake's head moves to one of the horizontally or vertically adjacent cells,
the second cell of the snake moves to the cell where the head was situated, the third cell
takes the former place of the second cell, etc. All these movements happen simultaneously,
so the head could potentially take the place of the tail. There are two configurations of the
snake's cells that are prohibited: self-intersection (meaning that after each step any pair of
snake cells should have pairwise different coordinates), and crossing the board's border.
The path is a sequence of characters L, R, D, and U(corresponding to left, right, down, and up,
respectively) describing the movements of snake's head on each turn. How many distinct
valid paths of length p can the snake make on the board?
