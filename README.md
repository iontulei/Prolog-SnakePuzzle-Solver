# Logic Programming Project - Snake Puzzle Solver

**Module 8 - Programming Paradigms, Bachelor of Technical Computer Science, University of Twente**

**Date:** 04-06-2024

**Contributors:**  
- [Dragoș Erhan](https://github.com/Headpoint2042)
- [Ion Tulei](https://github.com/iontulei)

## Project Overview

This project focuses on solving snake puzzles using logic programming. The puzzles involve placing a "snake" on a grid based on specific constraints such as row and column clues, ensuring the snake does not touch itself, and checking that the snake is fully connected. The solution was implemented in Prolog using a series of custom predicates to address the constraints defined in the puzzle.

### Core Predicate: `snake/4`

The main predicate, `snake/4`, orchestrates the solving of the puzzle by applying the following constraints:

1. **Row and Column Clues**: The number of snake body parts in each row and column must adhere to given clues.
2. **Non-Touching Constraint**: Snake body parts must not touch each other diagonally.
3. **Connectedness**: The snake must be a single connected entity.
4. **Head Count**: There must be exactly two snake heads in the grid.
5. **Neighbor Check**: Snake heads must have one neighbor, while body parts must have two neighbors.

## Task Division

- **Dragoș Erhan**:
  - Implemented the following predicates:
    - `checkRowClues/2`
    - `checkColClues/2`
    - `nonTouching/1`
  - Ensured compatibility of all predicates inside the `snake/4` predicate.

- **Ion Tulei**:
  - Implemented the following predicates:
    - `countNeighbors/1`
    - `snakeConnected/1`
    - `countHeads/1`
  - Ensured compatibility of all predicates inside the `snake/4` predicate.

## Solved Puzzles

The following puzzles were successfully solved by our program:

- **p2x2**: 0.00 sec
- **p3x3**: 0.00 sec
- **p3x3b**: 0.00 sec
- **pCycle**: 0.02 sec
- **p4x4**: 0.00 sec
- **p5x5a**: 0.02 sec
- **p5x5b**: 0.02 sec
- **p7x7**: 11.44 sec
- **p10x10**: N/A (takes too long)
- **p11x12**: N/A (unsolved)

## Technical Details

1. **Row and Column Clue Verification**: 
   - Predicates `checkRowClues/2` and `checkColClues/2` ensure that the number of snake body parts in each row and column matches the given clues.
   
2. **Non-Touching Constraint**: 
   - The `nonTouching/1` predicate enforces that no snake body parts touch each other diagonally.

3. **Neighbor Checking**: 
   - `countNeighbors/1` ensures that snake heads have exactly one neighbor and body parts have two neighbors.

4. **Snake Connectivity**: 
   - The `snakeConnected/1` predicate ensures that the snake is a single connected entity, allowing traversal from one head to the other without any disconnection.

5. **Head Count**: 
   - `countHeads/1` ensures there are exactly two heads in the puzzle grid.

## Remarks and Future Improvements

Our implementation is close to its peak performance for the given time constraints. While our solver efficiently handles puzzles up to size 7x7, it struggles with larger grids due to excessive backtracking during row and column clue verification.

### Possible Improvements:

A more efficient approach could be to construct the snake cell by cell from one head, checking all constraints simultaneously. This could reduce backtracking and eliminate the need for post-snake construction connectivity checks. Given more time, this method would likely improve the solver's ability to handle larger puzzles, such as 10x10 and beyond.

## Conclusion

This project successfully demonstrates the application of logic programming techniques to solve complex grid-based puzzles. Our solution is capable of solving most puzzles efficiently, and with additional optimizations, could be extended to solve larger puzzles in the future.