# Checkers

### Statement
Describe your project. Why is it interesting? Why is it interesting to you personally? What do you hope to learn? 

### Analysis
Explain what approaches from class you will bring to bear on the project.

Be explicit about the techiques from the class that you will use. For example:

- Will you use data abstraction? How?
- Will you use recursion? How?
- Will you use map/filter/reduce? How? 
- Will you use object-orientation? How?
- Will you use functional approaches to processing your data? How?
- Will you use state-modification approaches? How? (If so, this should be encapsulated within objects. `set!` pretty much should only exist inside an object.)
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

Create several paragraphs of narrative to explain the pieces and how they interoperate.

## Schedule
Explain how you will go from proposal to finished product. 

There are three deliverable milestones to explicitly define, below.

The nature of deliverables depend on your project, but may include things like processed data ready for import, core algorithms implemented, interface design prototyped, etc. 

You will be expected to turn in code, documentation, and data (as appropriate) at each of these stages.

Write concrete steps for your schedule to move from concept to working system. 

### First Milestone (Sun Apr 9)
Which portion of the work will be completed (and committed to Github) by this day? 

### Second Milestone (Sun Apr 16)
Which portion of the work will be completed (and committed to Github) by this day?  

### Public Presentation (Mon Apr 24, Wed Apr 26, or Fri Apr 28 [your date to be determined later])
What additionally will be completed before the public presentation?

## Group Responsibilities
Here each group member gets a section where they, as an individual, detail what they are responsible for in this project. Each group member writes their own Responsibility section. Include the milestones and final deliverable.

Please use Github properly: each individual must make the edits to this file representing their own section of work.

**Additional instructions for teams of three:** 
* Remember that you must have prior written permission to work in groups of three (specifically, an approved `FP3` team declaration submission).
* The team must nominate a lead. This person is primarily responsible for code integration. This work may be shared, but the team lead has default responsibility.
* The team lead has full partner implementation responsibilities also.
* Identify who is team lead.

In the headings below, replace the silly names and GitHub handles with your actual ones.

### Susan Scheme @susanscheme
will write the....

### Leonard Lambda @lennylambda
will work on...

### Frank Funktions @frankiefunk 
Frank is team lead. Additionally, Frank will work on...   
