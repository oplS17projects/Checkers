# Checkers Game in Racket

## Michael Bertucci

### April 29 2017

# Overview

This code implements the classic game of Checkers with the standard U.S. rule set. It is playable by two people and uses the Racket REPL as an interface for reading user input and displaying the game board after each player's move.

The function `start` begins execution of the game loop. Turns alternate between player 1 (black pieces) and player 2 (red pieces). The following table shows the list of valid commands a player may input on his/her turn. The only commands that end the player's turn are **move** (assuming it was a valid move, otherwise the player is prompted to try again), **load** (the current turn is determined by the file), and **default-board** (the turn is reset to player 1).

Input | Result
----- | ------
(move (C R) DIRECTION) | Moves the player's piece on column letter C and row number R in the indicated DIRECTION, which must be one of [northwest, northeast, southeast, southwest].
(save "FILENAME") | Saves the current game (state of the board and player whose turn it is) to a .xlsx file specified by FILENAME.
(load "FILENAME") | Loads a game from the file specified by FILENAME.
(default-board)| Starts a new game.
(score) | Displays a count of pieces remaining on each side.
(exit) | Quits the game.

The source code can be divided into four main areas:
1) Handling of player turns and interpreting player commands
2) Piece movement mechanics
3) Representation of board data and drawing images of the board
4) Importing/exporting game data to/from files

Of these four areas, I contributed some of **(1)**, much of **(2)** and the entirety of **(3)**.

# Libraries Used

Two libraries are used in this code:
```
(require simple-xlsx)
(require 2htdp/image) ; this is the one that I personally worked with
```
* `simple-xlsx` is used for reading and writing data to and from .xlsx files. 
  * Note for anyone wanting to download and run this project: the simple-xlsx library does not come installed with Racket and must be installed separately using the raco utility.
* `2htdp/image` is for creating image objects that can be drawn in the REPL and is used to display the board. As drawing the board was part of my division of the work, this was the library that I personally made use of.

# Key Code Excerpts

## 1.

## 2.

## 3.

## 4.
