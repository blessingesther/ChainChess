♟️ Chess Game Smart Contract
A smart contract implementation of a two-player Chess game written in Clarity, designed to run on the Stacks blockchain using the Clarinet development environment.

This project supports the creation and management of chess games, enforcing standard rules such as valid movement, turn-taking, and basic castling logic, all within a decentralized and immutable context.

📜 Features
✅ Game creation and tracking (via game IDs)

♟ Turn-based movement with move validation

🔒 Player authentication per move

📐 Full support for standard piece movements:

Pawns (including en passant)

Rooks

Knights

Bishops

Queens

Kings (including basic castling logic)

⛔ Comprehensive error handling with error codes

🧠 Piece color and movement rules enforcement

🧩 Board state stored in FEN-like 64-character format

📦 Contract Structure
Maps
games: Stores game state including players, board, turn, and more.

Variables
last-game-id: Auto-incremented counter to assign unique game IDs.

Constants
Error constants such as:

clarity
Copy
Edit
ERR-NOT-AUTHORIZED (err u100)
ERR-GAME-OVER (err u101)
ERR-NOT-YOUR-TURN (err u102)
...
🚀 How to Use
1. Create a New Game
clarity
Copy
Edit
(create-game opponent)
opponent: Principal of the second player.

Returns a unique game-id.

2. Make a Move
clarity
Copy
Edit
(make-move game-id from-x from-y to-x to-y)
Validates legality of the move, piece color, turn, and board bounds.

Updates board state and game progression.

3. Get Game Info
clarity
Copy
Edit
(get-game game-id)
Fetch current game state including board, turn, castling rights, etc.

🧠 Board Representation
Board state is a 64-character ASCII string simulating an 8x8 board. Ranks go from top (black side) to bottom (white side):

markdown
Copy
Edit
rnbqkbnr
pppppppp
         
         
         
         
PPPPPPPP
RNBQKBNR
Each square corresponds to an index in the string:

Index = y * 8 + x

(0, 0) = top-left

(7, 7) = bottom-right

♻️ Game Status Codes
0: Game in progress

1: White won

2: Black won

3: Draw

🚫 Error Handling
The contract defines specific error codes for clarity and debugging. Example:

clarity
Copy
Edit
(asserts! (is-eq tx-sender white-player) ERR-NOT-YOUR-TURN)
Each err uXXX value is defined as a named constant for readability.

📌 Limitations
No support for check, checkmate, or stalemate detection (WIP)

Castling logic does not yet validate if the king passes through check

No time control or game timeout enforcement

📂 Project Setup (Clarinet)
Ensure you have Clarinet installed:

bash
Copy
Edit
clarinet check         # Check for syntax and logic errors
clarinet test          # Run contract tests (you must write them!)
clarinet console       # Interact with your contract
🤝 Contributing
Pull requests are welcome! If you'd like to add features like checkmate detection, piece promotion, or a UI integration, feel free to fork the repo and contribute.