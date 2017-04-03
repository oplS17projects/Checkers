# Checkers

### Statement
The project is an interactive, two player game of checkers.
Why is it interesting? Why is it interesting to you personally? What do you hope to learn? 

### Analysis
Explain what approaches from class you will bring to bear on the project.

Be explicit about the techiques from the class that you will use. For example:

- Will you use data abstraction? How?

We will use recursion in part to process that data given by the XML files and generate the list of tile objects.

- Will you use map/filter/reduce? How?

We will use object orientation for the internal representation of the game. The individual tiles of the checkerboard will be represented as objects that contain their position and state data. The board itself will also be an object, containing the list of tiles as well as other information (score, etc.).

- Will you use functional approaches to processing your data? How?

State modification will be used within the tile and board objects. The status of each tile (empty/occupied piece color, etc) will be changed via state modification when players move pieces and the state of certain components of the board, such as the number of pieces each player has will change as well.

- Will you build an expression evaluator, like we did in the symbolic differentatior and the metacircular evaluator?
- Will you use lazy evaluation approaches?

The idea here is to identify what ideas from the class you will use in carrying out your project. 

**Your project will be graded, in part, by the extent to which you adopt approaches from the course into your implementation, _and_ your discussion about this.**

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

The player makes a move by first choosing a given piece under his/her control and a desired destination tile. When the player issues the move command, the tile is checked against the list of tiles to determine if the move is valid by checking that the tile is empty and is either adjacent, or has an opponent’s piece in the preceding tile.

At the start of the game or when a move is made, the board is drawn in accordance with the data contained in the tile objects. An image is drawn on the board corresponding to the state of the tile and the location of the tile.

At any point during the game, the players can opt to save the game. This exports the data from each tile object to the corresponding data cell of an xlsx file. This data can then be loaded, setting the states of the tiles based on the data contained in the xlsx file.
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
