# Mutant-Sudoku
Sudoku solver that can handle a number of different sudoku variants

To run use must have sbt installed. 

The program is currently configured to run all the "Outside Sudoku" puzzles that have been encoded, 
but that can easily be changed at the top of the Sudoku.scala. 

Each file is run against four different solvers. 
The solvers cover the combinations of the variable selectors Minimum Domain and Weighted Minimum Domain 
and the value selectors Minimum Remaining Value and Least Constrainting Value. 
