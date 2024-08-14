
## Ploy-Board-Game

Ploy-Board-Game is a Haskell implementation of the board game [Ploy](https://en.wikipedia.org/wiki/Ploy_(board_game)). This project includes both a Haskell-based game logic and a Python web server for interacting with the game through a web interface.

### Table of Contents

- [Project Description](#project-description)
- [Installation](#installation)
- [Usage](#usage)
- [Game Modes](#game-modes)
- [Test Commands](#test-commands)
- [Building Commands](#building-commands)
- [Project Structure](#project-structure)

### Project Description

Ploy is a strategic board game, and this project aims to provide a Haskell implementation of it. The game can be played through a web interface provided by a Python web server.

### Installation

To set up Ploy-Board-Game on your local machine, follow these steps:

1. Clone the repository:
    ```bash
    git clone https://github.com/antonhtmnn/Ploy-Board-Game.git
    ```
2. Navigate to the project directory:
    ```bash
    cd Ploy-Board-Game
    ```
3. Set up the Haskell environment:
    ```bash
    stack setup
    ```
4. Build the Haskell bot executable:
    ```bash
    stack build
    ```

### Usage

#### Running the Web Server

To run the web server, you need Python 3. Follow these steps:

1. Navigate to the webserver folder:
    ```bash
    cd ploy-webserver
    ```
2. First make sure that `ploy.py` contains the correct path to the bot, i.e. check line `13-14`. Then start the server:
    ```bash
    python ploy.py
    ```
3. Open the website in your browser:
    ```
    127.0.0.1:8000
    ```

### Game Modes

The Ploy server supports three gaming modes:

1. **Without validation:** A move from the GUI is not checked. Any move will be executed on the board.
2. **With validation:** A move is checked by the bot implementation. The executable needs to be built and embedded.
3. **Against Haskell bot:** Moves are checked similar to "with validation". The white player is based on the bot implementation.

### Test Commands

To run tests for the project, use the following commands:

- Run all tests:
    ```bash
    stack test
    ```
- Run validation tests:
    ```bash
    stack test ploy:validate
    ```
- Run unit tests:
    ```bash
    stack test ploy:units
    ```
- Run tests with coverage:
    ```bash
    stack test --coverage
    ```

### Building Commands

To build the project, use the following commands:

- Build the project:
    ```bash
    stack build
    ```
- Clean the project:
    ```bash
    stack clean
    ```

### Project Structure

The project is structured as follows:

```
Ploy-Board-Game/
├── ploy-webserver/
│   ├── README.md
│   ├── css/
│   │   ├── chessboard-0.3.0.css
│   │   ├── chessboard-0.3.0.min.css
│   │   ├── chessboard.css
│   │   ├── normalize-2.1.2.min.css
│   │   └── site2.css
│   ├── img/
│   │   └── background.png
│   ├── index.html
│   ├── js/
│   │   ├── jquery-1.10.1.min.js
│   │   ├── json3.min.js
│   │   ├── ployboard.js
│   │   └── prettify.js
│   ├── ploy.html
│   └── ploy.py
├── ploy/
│   ├── README.md
│   ├── app/
│   │   └── Main.hs
│   ├── package.yaml
│   ├── src/
│   │   ├── Board.hs
│   │   └── Ploy.hs
│   ├── stack.yaml
│   ├── test/
│   │   └── Spec.hs
│   └── validate/
│       └── Spec.hs
├── LICENSE
└── README.md
```
