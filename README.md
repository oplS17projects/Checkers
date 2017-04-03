# Checkers

### Statement
The project is an interactive, two player game of checkers. 

We were interested in making something with a graphical representation that would update dynamically as its internal state changed. While trying to decide how to add interactivity to this idea, we decided upon making a Checkers game. In this context the board is the graphic that changes in response to state, which in turn changes in response to user input.

Our hope is that through this project we will learn how to make a dynamic, interactive system using the functional programming perspective required by Racket.

### Analysis
Concepts from class that will be used in the project:

- Recursion
We will use recursion in part to process that data given by the XML files and generate the list of tile objects. Checking the validity of moves will also require recursing through the tiles of the board.

- Map/filter/reduce
Will likely need to filter the game board (i.e. list of tile objects) for ones containing the player’s pieces, etc.

- Object Orientation
We will use object orientation for the internal representation of the game. The individual tiles of the checkerboard will be represented as objects that contain their position and state data. The board itself will also be an object, containing the list of tiles as well as other information (pieces remaining on each side, etc.).

- State Modification
State modification will be used within the tile and board objects. The status of each tile (empty/occupied, piece color, etc) will be changed via state modification when players move pieces and the state of certain components of the board, such as the number of pieces each player has, will change as well.

- Expression Evaluator
As user input comes from the keyboard, we will need to build an expression evaluator to parse the user's input. It will need to identify the command (save/load/move) and its arguments from the user’s input expression. So the user could input something like (move (B6 C5)) and the code will need to extract the two tile locations and check if the user whose turn it is has a piece on B6 and that it can legally move to C5.

### External Technologies
We will use Open XML format files (.xlsx file, the format that MS Excel saves spreadsheets in) to store data about the state of the game board. The simple-xlsx library will be used to get information in and out of these files.

### Data Sets or other Source Materials
We will not need to make use of data from any external source. We will, however, be creating some of our own data sets to test our program with. Specifically, we will create .xlsx files with data representing sample game configurations and make sure our program can properly parse and incorporate the data.

### Deliverable and Demonstration
The ultimate goal is to create a fully interactive game of Checkers. The game will not have an AI, but will be designed to be played by two users in alternating turns. At the live demo we will demonstrate playing the game. We will also be able to demonstrate saving and loading various instances of the game.

### Evaluation of Results
The ability to save and load game states is not only convenient for users but is also a perfect mechanism for testing that the program is working properly since it provides the ability to essentially print out (in a file) the internal state of the board. This makes it easy to check that the program does as expected after each user input. The state written to the file should also match up with the visual of the board within DrRacket. As long as these elements consistently match up, we will know that the program works correctly.

## Architecture Diagram
![Diagram](/fp4-architecture-diagram.png?raw=true)
    
The game board is represented internally by a list of 64 tile objects within an overarching board object. Each tile contains information on its position and current state (empty/red or black piece, etc).

At the start of the game and whenever a move is made, the board is drawn on-screen in accordance with the data contained in the tile objects. Each tile is drawn in its given position, and if its state specifies it has a piece on it then the graphic is overlayed with a circle of the specified color.

The player makes a move by first choosing a given piece under his/her control and a desired destination tile. When the player issues the move command, the tile is checked against the list of tiles to determine if the move is valid (e.g. the destination tile is not already occupied by a piece, the destination can be reached by moving on diagonals, etc.).

At any point during the game, the players can issue a command to save the game. This exports the data from each tile object to the corresponding data cell of an xlsx file. This data can then be loaded, setting the states of the tiles based on the data contained in the xlsx file.

## Schedule

### First Milestone
The program will be able to read from an XML file to create and display a checkerboard in the correct state.

### Second Milestone
The basic mechanics for piece movement and piece capture will be implemented. 

### Public Presentation
Score keeping and the “king" pieces will be implemented. Game will end when one player has no pieces remaining.

## Group Responsibilities

### Alexander Pilozzi @PRXela
-Inputting and exporting data to xlsx file
-Interpreting user input

### Michael Bertucci @mbertucci1
-Representing the game board object and drawing it on screen
-Representing tile objects and maintaining/updating their states based on user moves

### Both Responsible For
-Implementing the rules of the game and making sure user input and resulting changes in state are in accordance with the rules
