# Checkers

### Statement
The project is an interactive, two player game of checkers. 

We were interested in making something with a graphical representation that would update dynamically as its internal state changed. While trying to decide how to add interactivity to this idea, we decided upon making a Checkers game. In this context the board is the graphic that changes in response to state, which in turn changes in response to user input.

Our goal in this project was to learn how to make a dynamic, interactive system using the functional programming perspective required by Racket.

### Analysis
Concepts from class used in the project:

Recursion:
- Constructing the game board is a recursive process, since the board is a two dimentional list (a list of 8 rows, which are each lists of 8 tile objects). Because of its structure as a list, recursion is used a lot to loop through the board, such as when accessing an individual tile in order to check its state.
- All uses of mapping/filtering/folding are inherently recursive operations.
- Capturing pieces is a recursive process. Once a capture is made, the tile that was landed on becomes the new starting tile from which to make another capture if possible. The capture function makes the capture by changing the pieces on the necessary tiles, then checks if another capture can be made from the new location and if so calls the capture function again.
- Importing data from the XML spreadsheet was done by looking through each cell and recursively building a grid/list-of-lists out of the data.
- Exporting data from the board to an XML spreadsheet was done by calling the various tile predicates on each of the tiles within the board and building a grid out of the data to prepare it for export.
- Processing of the grid data to extract specific rows, columns or elements was done by recursively going through the list until the desired portion was reached.


Map/filter/reduce: 
- Reduce (foldr) is used most notably to create the image of the game board. Using `empty-image` as the base, the image of a row is created by folding a list of squares using the `beside` function. Then, the whole board is drawn by folding a list of such row images using the `above` function.
- Map is used in two main ways. First, it is used to apply image-generating functions to objects in the game board, such as in `(map square-to-image r)` for a given row `r` of tile objects. This creates the list of squares which may or may not have a black or red circle drawn over them depending on whether the corresponding tile object indicated it had that color piece on it, and this list is used in the fold operation described in the bullet above. Second, it is used to display all items in a list to the user in some format. In these cases it is used mainly for the side effect of calling the mapped function, and not for the resulting list.
- Filter is the most used of the higher order list operations in our code. There are many areas where the game board needed to be filtered for tiles meeting specific criteria, such as for finding all the tiles that are diagonal to a specific tile, and then further filtering the result to only those tiles that have opponent pieces on them.

Object Orientation: 
- Every one of the 64 tiles that make up the board is an object, constructed with a given row number, column number, piece color (or none), and boolean indicating whether the piece is a king or not. The board is a list of the `dispatch` procedures of these objects. The dispatch procedure accepts getters for each parameter of the object, and setters for all except the row and column numbers.

State Modification: 
- State modification is primarily used within the tile objects and only for the `piece` and `is-king` parameters, the values of which change as the player moves pieces around the board, or are set when the user loads a game state from a file.
- State modification is used in the xlsx library to get around some of the limitations of the simple-xlsx procedures.

#### Expression Evaluator
An expression evaluator was built for the interpretation of user input. On each turn, the player issues commands by inputting a list containing the desired command and any associated parameters. The evaluator then processes that list, separating the command from its parameters and validating both before executing the correct procedure. The evaluator then outputs a symbol that dictates how the game-loop will respond to the player's action (exchange turns, ask for another command from the same player, etc.).

#### External Technologies
We are using Open XML format files (.xlsx file, the format that MS Excel saves spreadsheets in) to store data about the state of the game board. The simple-xlsx library is what we used to get information in and out of these files.

#### Data Sets or other Source Materials
We do not need to make use of data from any external source. We have, however, created some of our own data sets to test our program with. Specifically, we created .xlsx files with data representing sample game configurations to make sure our program properly parses and incorporates the data. Some of these configurations allow for testing various features, and can be loaded on the fly from within the game loop itself. The [Test files folder](Test%20files) contains some of these.

#### Deliverable and Demonstration
Our deliverable is a fully interactive game of checkers played in the Racket REPL. The game does not have an AI, but is designed to be played by two users in alternating turns. At the live demo we will primarily demonstrate playing the game. We will also be able to demonstrate saving and loading various instances of the game. Various pre-made scenarios will be available to showcase some of the more complicated features.

#### Evaluation of Results
The ability to save and load game states is not only convenient for users but is also a perfect mechanism for testing that the program is working properly since it provides the ability to essentially print out (in a file) the internal state of the board. This makes it easy to check that the program does as expected after each user input. The state contained in the file must match up with the visual of the board within DrRacket and the options available to the player on that turn. As long as these elements consistently match up, we know that the program works correctly.

## Architecture Diagram
![Diagram](/fp4-architecture-diagram.png?raw=true)
    
The game board is represented internally by a list of 64 tile objects within an overarching board object. Each tile contains information on its position and current state (empty/red or black piece, etc).

At the start of the game and whenever a move is made, the board is drawn on-screen in accordance with the data contained in the tile objects. Each tile is drawn in its given position, and if its state specifies it has a piece on it then the graphic is overlayed with a circle of the specified color.

The player makes a move by first choosing a given piece under his/her control and a desired destination tile. When the player issues the move command, the tile is checked against the list of tiles to determine if the move is valid (e.g. the destination tile is not already occupied by a piece, the destination can be reached by moving on diagonals, etc.).

At any point during the game, the players can issue a command to save the game. This exports the data from each tile object to the corresponding data cell of an xlsx file. This data can then be loaded, setting the states of the tiles based on the data contained in the xlsx file.

## Schedule

### First Milestone
The program could read from an XML file and use the data to update the state of the tile objects, and the board could be displayed. The board could then be reverted do a default state through a hard-coded default-board state-list. Basic tile state-modification and predicate procedures were implemented.

### Second Milestone
The basic mechanics of the game were implemented. A game loop was created that allowed players move pieces, display the score, load the game from either a given file (checkers.xlsx) or from the default-board state and exit the game. When a player made a valid move, the loop would begin the other player's turn. Movement of normal pieces was implemented, and piece capture was mostly implemented.

### Public Presentation
The mechanics of the game were finalized. Piece capture was fully implemented to properly accommodate for scenarios where a player has multiple available captures following a preceding capture. "King" pieces were implemented mechanically and graphically. The game loop was refined, and keeps track of score, terminating the game with a victory message when the losing player has no pieces remaining. Saving and loading was refined to allow for user specified file names.

## Group Member Contributions

### Alexander Pilozzi @PRXela
- Procedures for exporting and importing data to and from xlsx files
- Primary creation of game loop and user command evaluation procedures
- Primary implementation of basic validation of move commands within loop
- Basics of piece-movement and single capture mechanics, with some validation

### Michael Bertucci @mbertucci1
- All functions related to drawing images of the board/tiles/pieces and row/column labels
- Implementation of tile objects and their interface through the `dispatch` function
- Implementation of the board as a 8x8 list of the tile objects
- Logic for advanced capture mechanics (recursive capturing, detecting available captures, forced capturing)

### Both Contributed To
- Making minor adjustments/additions to some parts of each other's code where appropriate
- Collaborating to ensure that user input and resulting changes in state are in accordance with the rules of the game
- Making sure the game loop handled alternating turns properly (e.g. if player entered an invalid move he can re-take his turn, loading a game set turn to the player whose turn it was when the game was saved, etc.)
