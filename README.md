## Ploy-Board-Game

Ploy-Board-Game is a Haskell implementation of the board game [Ploy](https://en.wikipedia.org/wiki/Ploy_(board_game)). This project includes both a Haskell-based game logic and a Python web server for interacting with the game through a web interface.

<p align="center">
  <a href="https://www.youtube.com/watch?v=Q9Dr3bevVh4" title="YouTube Video">
    <img src="https://img.youtube.com/vi/Q9Dr3bevVh4/maxresdefault.jpg" alt="Demo video thumbnail" width="640" />
  </a>
  <br/>
  <sub>▶ click image to watch demo video</sub>
</p>

### Table of Contents

- [Project Description](#project-description)
- [Game Rules](#game-rules)
- [Installation](#installation)
- [Usage](#usage)
- [Game Modes](#game-modes)
- [Test Commands](#test-commands)
- [Building Commands](#building-commands)

### Project Description

Ploy is a strategic, chess-like board game. Each side controls pieces with different allowed movement directions and ranges. This repository contains:
- a **Haskell** implementation of the game logic (validation, rules), and
- a **Python** web server that provides a simple web UI to play the game.

---

### Game Rules

**Board & Start**
- Board size: **9×9**.
- **Black** moves first.
- Each side starts with **15** pieces. Pieces show **direction lines** (strokes) that indicate along which directions they may move.

**Turn**
- On your turn you must perform a **move and/or a rotation** that **changes the position or orientation** (no null moves).

**Movement**
- A piece moves **horizontally, vertically, or diagonally** strictly **along its printed direction lines**.
- Pieces **cannot jump** over other pieces.
- The destination square must be **empty** or contain an **opponent piece**; in the latter case the opponent piece is **captured** and removed.

**Rotation**
- A rotation changes the **orientation** of a piece’s direction lines (to the next valid orientation for that piece type).

**Piece types & per-turn allowance**

| Piece       | Allowed action per turn |
|-------------|--------------------------|
| **Shield**    | Up to **1 square** **and/or** **rotation** (rotation may be in addition to the move). |
| **Probe**     | **Either** up to **2 squares** **or** **rotation**. |
| **Lance**     | **Either** up to **3 squares** **or** **rotation**. |
| **Commander** | **Either** up to **1 square** **or** **rotation**. |

> Movement must always follow the piece’s direction lines; no jumping.

<!-- Optional: include a legend image of the piece variants/orientations -->
<!--
![Piece Legend](docs/assets/ploy_piece_legend.png)
-->

**Win condition**
- Capture the opponent’s **Commander**, **or**
- capture **all other** opponent pieces.

---

### Installation

To set up Ploy-Board-Game on your local machine, follow these steps:

1. Clone the repository:
    ```bash
    git clone https://github.com/antonhtmnn/Ploy-Board-Game.git
    ```
2. Navigate to the `ploy` directory:
    ```bash
    cd Ploy-Board-Game/ploy
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
