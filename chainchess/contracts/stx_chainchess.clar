;; Chess Game Smart Contract in Clarity
;; Implementation for Clarinet/Stacks blockchain

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-GAME-OVER (err u101))
(define-constant ERR-NOT-YOUR-TURN (err u102))
(define-constant ERR-INVALID-MOVE (err u103))
(define-constant ERR-INVALID-POSITION (err u104))
(define-constant ERR-NO-PIECE (err u105))
(define-constant ERR-WRONG-PIECE-COLOR (err u106))
(define-constant ERR-GAME-NOT-EXIST (err u107))
(define-constant ERR-GAME-ALREADY-EXISTS (err u108))
(define-constant ERR-INVALID-PLAYER (err u109))

;; Data types
(define-data-var last-game-id uint u0)

;; Game status:
;; 0 = in progress, 1 = white won, 2 = black won, 3 = draw
(define-map games
  { game-id: uint }
  {
    white-player: principal,
    black-player: principal,
    board: (string-ascii 64), ;; FEN-like representation (8x8 chars)
    current-turn: (string-ascii 1), ;; "w" or "b"
    status: uint,
    castling-availability: (string-ascii 4), ;; "KQkq", "-", etc.
    en-passant-target: (optional (tuple (x uint) (y uint))),
    halfmove-clock: uint,
    fullmove-number: uint
  }
)

;; Initialize the board to standard chess position
(define-private (get-initial-board)
  (concat
    "rnbqkbnr" ;; black pieces at top (8th rank)
    "pppppppp" ;; black pawns (7th rank)
    "        " ;; empty squares (6th rank)
    "        " ;; empty squares (5th rank)
    "        " ;; empty squares (4th rank)
    "        " ;; empty squares (3rd rank)
    "PPPPPPPP" ;; white pawns (2nd rank)
    "RNBQKBNR" ;; white pieces at bottom (1st rank)
  )
)

;; Create a new game
(define-public (create-game (opponent principal))
  (let ((game-id (+ (var-get last-game-id) u1)))
    (asserts! (not (is-eq tx-sender opponent)) ERR-INVALID-PLAYER)
    (asserts! (is-none (map-get? games { game-id: game-id })) ERR-GAME-ALREADY-EXISTS)
    
    (map-set games
      { game-id: game-id }
      {
        white-player: tx-sender,
        black-player: opponent,
        board: (get-initial-board),
        current-turn: "w",
        status: u0,
        castling-availability: "KQkq",
        en-passant-target: none,
        halfmove-clock: u0,
        fullmove-number: u1
      }
    )
    
    (var-set last-game-id game-id)
    (ok game-id)
  )
)

;; Get a specific game
(define-read-only (get-game (game-id uint))
  (ok (unwrap! (map-get? games { game-id: game-id }) ERR-GAME-NOT-EXIST))
)

;; Check if coordinates are valid (0-7 for both x and y)
(define-private (is-valid-position (x uint) (y uint))
  (and (<= x u7) (<= y u7))
)

;; Get piece at position
(define-private (get-piece-at (board (string-ascii 64)) (x uint) (y uint))
  (let ((index (+ (* y u8) x)))
    (unwrap-panic (element-at board index))
  )
)

;; Check if piece at position belongs to player
(define-private (is-own-piece (piece (string-utf8 1)) (player-color (string-ascii 1)))
  (if (is-eq player-color "w")
      (is-uppercase piece)
      (is-lowercase piece)
  )
)

;; Check if piece is uppercase (white)
(define-private (is-uppercase (piece (string-utf8 1)))
  (or
    (is-eq piece "R")
    (is-eq piece "N")
    (is-eq piece "B")
    (is-eq piece "Q")
    (is-eq piece "K")
    (is-eq piece "P")
  )
)

;; Check if piece is lowercase (black)
(define-private (is-lowercase (piece (string-utf8 1)))
  (or
    (is-eq piece "r")
    (is-eq piece "n")
    (is-eq piece "b")
    (is-eq piece "q")
    (is-eq piece "k")
    (is-eq piece "p")
  )
)

;; Set piece at position in board
(define-private (set-piece-at (board (string-ascii 64)) (x uint) (y uint) (piece (string-utf8 1)))
  (let ((index (+ (* y u8) x)))
    (replace-at board index piece)
  )
)

;; Helper to replace character at specific position
(define-private (replace-at (str (string-ascii 64)) (index uint) (new-char (string-utf8 1)))
  (let
    (
      (len (len str))
      (before (slice str u0 index))
      (after (slice str (+ index u1) len))
    )
    (concat (concat before new-char) after)
  )
)

;; Check if move is valid for pawn
(define-private (is-valid-pawn-move (board (string-ascii 64))
                                   (from-x uint) (from-y uint)
                                   (to-x uint) (to-y uint)
                                   (is-white bool)
                                   (en-passant-target (optional (tuple (x uint) (y uint)))))
  (let
    (
      (direction (if is-white (- u0 u1) u1)) ;; white moves up (-1), black moves down (+1)
      (start-rank (if is-white u6 u1))       ;; starting rank for white (6) and black (1)
      (y-diff (if is-white (- from-y to-y) (- to-y from-y)))
      (x-diff (abs (- from-x to-x)))
      (to-piece (get-piece-at board to-x to-y))
    )
    (or
      ;; Forward move (1 square)
      (and
        (is-eq x-diff u0)           ;; Same file
        (is-eq y-diff u1)           ;; Move one square
        (is-eq to-piece " ")        ;; Target square is empty
      )
      ;; Forward move (2 squares from starting position)
      (and
        (is-eq x-diff u0)           ;; Same file
        (is-eq y-diff u2)           ;; Move two squares
        (is-eq from-y start-rank)   ;; From starting rank
        (is-eq to-piece " ")        ;; Target square is empty
        ;; Square in between is also empty
        (is-eq (get-piece-at board from-x (+ from-y direction)) " ")
      )
      ;; Capture diagonally (including en passant)
      (and
        (is-eq x-diff u1)           ;; Move one square diagonally
        (is-eq y-diff u1)           ;; Move one square diagonally
        (or
          ;; Regular capture
          (and
            (not (is-eq to-piece " "))                  ;; Target has a piece
            (not (is-own-piece to-piece (if is-white "w" "b")))  ;; Target is opponent's piece
          )
          ;; En passant capture
          (and
            (is-eq to-piece " ")                        ;; Target square is empty
            (is-some en-passant-target)                 ;; En passant is available
            (let ((ep (unwrap-panic en-passant-target)))
              (and
                (is-eq to-x (get ep "x"))
                (is-eq to-y (get ep "y"))
              )
            )
          )
        )
      )
    )
  )
)

;; Check if move is valid for rook
(define-private (is-valid-rook-move (board (string-ascii 64))
                                   (from-x uint) (from-y uint)
                                   (to-x uint) (to-y uint))
  (let
    (
      (x-diff (abs (- from-x to-x)))
      (y-diff (abs (- from-y to-y)))
    )
    (and
      (or
        (and (is-eq x-diff u0) (> y-diff u0)) ;; Vertical move
        (and (is-eq y-diff u0) (> x-diff u0)) ;; Horizontal move
      )
      (is-path-clear board from-x from-y to-x to-y)
    )
  )
)

;; Check if move is valid for knight
(define-private (is-valid-knight-move (board (string-ascii 64))
                                     (from-x uint) (from-y uint)
                                     (to-x uint) (to-y uint))
  (let
    (
      (x-diff (abs (- from-x to-x)))
      (y-diff (abs (- from-y to-y)))
    )
    (or
      (and (is-eq x-diff u1) (is-eq y-diff u2))
      (and (is-eq x-diff u2) (is-eq y-diff u1))
    )
  )
)

;; Check if move is valid for bishop
(define-private (is-valid-bishop-move (board (string-ascii 64))
                                     (from-x uint) (from-y uint)
                                     (to-x uint) (to-y uint))
  (let
    (
      (x-diff (abs (- from-x to-x)))
      (y-diff (abs (- from-y to-y)))
    )
    (and
      (is-eq x-diff y-diff) ;; Must move diagonally (equal x and y distance)
      (> x-diff u0)         ;; Must move at least one square
      (is-path-clear board from-x from-y to-x to-y)
    )
  )
)

;; Check if move is valid for queen
(define-private (is-valid-queen-move (board (string-ascii 64))
                                    (from-x uint) (from-y uint)
                                    (to-x uint) (to-y uint))
  (or
    (is-valid-rook-move board from-x from-y to-x to-y)
    (is-valid-bishop-move board from-x from-y to-x to-y)
  )
)

;; Check if move is valid for king
(define-private (is-valid-king-move (board (string-ascii 64))
                                   (from-x uint) (from-y uint)
                                   (to-x uint) (to-y uint)
                                   (castling-availability (string-ascii 4)))
  (let
    (
      (x-diff (abs (- from-x to-x)))
      (y-diff (abs (- from-y to-y)))
    )
    (or
      ;; Regular king move (one square in any direction)
      (and
        (<= x-diff u1)
        (<= y-diff u1)
        (not (and (is-eq x-diff u0) (is-eq y-diff u0)))
      )
      ;; Castling
      (and
        (is-eq y-diff u0)        ;; Same rank
        (is-eq x-diff u2)        ;; Move two squares horizontally
        (is-castling-valid board from-x from-y to-x to-y castling-availability)
      )
    )
  )
)

;; Determine if castling is valid
(define-private (is-castling-valid (board (string-ascii 64))
                                  (from-x uint) (from-y uint)
                                  (to-x uint) (to-y uint)
                                  (castling-availability (string-ascii 4)))
  (let
    (
      (is-king-side (> to-x from-x))
      (is-white (is-eq from-y u7))
      ;; Check if appropriate castling right is available
      (castling-right 
        (if is-white
            (if is-king-side "K" "Q")
            (if is-king-side "k" "q")
        )
      )
    )
    (and
      ;; King must be in correct starting position
      (or 
        (and is-white (is-eq from-x u4) (is-eq from-y u7))
        (and (not is-white) (is-eq from-x u4) (is-eq from-y u0))
      )
      ;; Castling availability
      (index-of castling-availability castling-right)
      ;; Path must be clear
      (is-path-clear board from-x from-y to-x to-y)
      ;; Rook must be in correct position
      (let
        (
          (rook-x (if is-king-side u7 u0))
          (expected-rook (if is-white "R" "r"))
        )
        (is-eq (get-piece-at board rook-x from-y) expected-rook)
      )
      ;; King must not be in check and must not pass through check
      ;; Note: This is a simplified check that doesn't actually verify check conditions
      ;; A full implementation would need to check if the king is in check or would pass through check
      true
    )
  )
)

;; Check if path between from and to is clear
(define-private (is-path-clear (board (string-ascii 64))
                              (from-x uint) (from-y uint)
                              (to-x uint) (to-y uint))
  (let
    (
      (x-diff (- to-x from-x))
      (y-diff (- to-y from-y))
      (x-step (cond
                ((< x-diff 0) (- u0 u1))
                ((> x-diff 0) u1)
                (true u0)
              ))
      (y-step (cond
                ((< y-diff 0) (- u0 u1))
                ((> y-diff 0) u1)
                (true u0)
              ))
      (steps (max (abs x-diff) (abs y-diff)))
    )
    (fold check-path-clear true (list u1 u2 u3 u4 u5 u6 u7))
    
    ;; Helper function for fold
    (define-private (check-path-clear (step uint) (is-clear bool))
      (if (or (not is-clear) (>= step steps))
          is-clear
          (let
            (
              (x (+ from-x (* step x-step)))
              (y (+ from-y (* step y-step)))
            )
            (is-eq (get-piece-at board x y) " ")
          )
      )
    )
  )
)

;; Check if a move is valid for the given piece
(define-private (is-valid-move-for-piece (board (string-ascii 64))
                                        (from-x uint) (from-y uint)
                                        (to-x uint) (to-y uint)
                                        (piece (string-utf8 1))
                                        (castling-availability (string-ascii 4))
                                        (en-passant-target (optional (tuple (x uint) (y uint)))))
  (let
    (
      (piece-lower (to-lowercase piece))
      (is-white (is-uppercase piece))
    )
    (cond
      ((is-eq piece-lower "p") (is-valid-pawn-move board from-x from-y to-x to-y is-white en-passant-target))
      ((is-eq piece-lower "r") (is-valid-rook-move board from-x from-y to-x to-y))
      ((is-eq piece-lower "n") (is-valid-knight-move board from-x from-y to-x to-y))
      ((is-eq piece-lower "b") (is-valid-bishop-move board from-x from-y to-x to-y))
      ((is-eq piece-lower "q") (is-valid-queen-move board from-x from-y to-x to-y))
      ((is-eq piece-lower "k") (is-valid-king-move board from-x from-y to-x to-y castling-availability))
      (true false)
    )
  )
)

;; Convert character to lowercase
(define-private (to-lowercase (c (string-utf8 1)))
  (cond
    ((is-eq c "P") "p")
    ((is-eq c "N") "n")
    ((is-eq c "B") "b")
    ((is-eq c "R") "r")
    ((is-eq c "Q") "q")
    ((is-eq c "K") "k")
    (true c)
  )
)

;; Make a move
(define-public (make-move (game-id uint) (from-x uint) (from-y uint) (to-x uint) (to-y uint))
  (let
    (
      (game (unwrap! (map-get? games { game-id: game-id }) ERR-GAME-NOT-EXIST))
      (white-player (get game "white-player"))
      (black-player (get game "black-player"))
      (current-turn (get game "current-turn"))
      (board (get game "board"))
      (status (get game "status"))
      (castling-availability (get game "castling-availability"))
      (en-passant-target (get game "en-passant-target"))
      (halfmove-clock (get game "halfmove-clock"))
      (fullmove-number (get game "fullmove-number"))
    )
    
    ;; Check if game is in progress
    (asserts! (is-eq status u0) ERR-GAME-OVER)
    
    ;; Check if it's player's turn
    (asserts! 
      (or 
        (and (is-eq current-turn "w") (is-eq tx-sender white-player))
        (and (is-eq current-turn "b") (is-eq tx-sender black-player))
      )
      ERR-NOT-YOUR-TURN
    )
    
    ;; Check if positions are valid
    (asserts! (and (is-valid-position from-x from-y) (is-valid-position to-x to-y)) ERR-INVALID-POSITION)
    
    ;; Check if there's a piece at the starting position
    (let
      (
        (piece (get-piece-at board from-x from-y))
        (target-piece (get-piece-at board to-x to-y))
      )
      (asserts! (not (is-eq piece " ")) ERR-NO-PIECE)
      
      ;; Check if piece belongs to the player
      (asserts! (is-own-piece piece current-turn) ERR-WRONG-PIECE-COLOR)
      
      ;; Check if target is not own piece
      (asserts! (or (is-eq target-piece " ") (not (is-own-piece target-piece current-turn))) ERR-INVALID-MOVE)
      
      ;; Check if move is valid for the piece
      (asserts! 
        (is-valid-move-for-piece board from-x from-y to-x to-y piece castling-availability en-passant-target) 
        ERR-INVALID-MOVE
      )
      
      ;; Process move
      (let
        (
          ;; Determine if this is a capture
          (is-capture (not (is-eq target-piece " ")))
          
          ;; Handle en passant capture
          (is-en-passant 
            (and 
              (or (is-eq piece "p") (is-eq piece "P"))
              (is-some en-passant-target)
              (let ((ep (unwrap-panic en-passant-target)))
                (and (is-eq to-x (get ep "x")) (is-eq to-y (get ep "y")))
              )
            )
          )
          
          ;; Handle castling
          (is-castling
            (and
              (or (is-eq piece "k") (is-eq piece "K"))
              (is-eq (abs (- from-x to-x)) u2)
            )
          )
          
          ;; Check for pawn promotion (simplified - always promotes to queen)
          (is-promotion
            (and
              (or (is-eq piece "p") (is-eq piece "P"))
              (or (is-eq to-y u0) (is-eq to-y u7))
            )
          )
          
          ;; Calculate new en passant target
          (new-en-passant 
            (if (and
                  (or (is-eq piece "p") (is-eq piece "P"))
                  (is-eq (abs (- from-y to-y)) u2)
                )
                (some { x: from-x, y: (/ (+ from-y to-y) u2) })
                none
            )
          )
          
          ;; Update castling availability
          (new-castling
            (cond
              ;; King moved
              ((is-eq piece "K") (replace-chars castling-availability "KQ" ""))
              ((is-eq piece "k") (replace-chars castling-availability "kq" ""))
              ;; Rook moved or captured
              ((and (is-eq piece "R") (is-eq from-x u0) (is-eq from-y u7)) (replace-chars castling-availability "Q" ""))
              ((and (is-eq piece "R") (is-eq from-x u7) (is-eq from-y u7)) (replace-chars castling-availability "K" ""))
              ((and (is-eq piece "r") (is-eq from-x u0) (is-eq from-y u0)) (replace-chars castling-availability "q" ""))
              ((and (is-eq piece "r") (is-eq from-x u7) (is-eq from-y u0)) (replace-chars castling-availability "k" ""))
              ;; Rook captured
              ((and (is-eq target-piece "R") (is-eq to-x u0) (is-eq to-y u7)) (replace-chars castling-availability "Q" ""))
              ((and (is-eq target-piece "R") (is-eq to-x u7) (is-eq to-y u7)) (replace-chars castling-availability "K" ""))
              ((and (is-eq target-piece "r") (is-eq to-x u0) (is-eq to-y u0)) (replace-chars castling-availability "q" ""))
              ((and (is-eq target-piece "r") (is-eq to-x u7) (is-eq to-y u0)) (replace-chars castling-availability "k" ""))
              (true castling-availability)
            )
          )
          
          ;; Update halfmove clock (resets on pawn move or capture)
          (new-halfmove-clock
            (if (or (is-eq (to-lowercase piece) "p") is-capture)
                u0
                (+ halfmove-clock u1)
            )
          )
          
          ;; Update fullmove number (increments after black's move)
          (new-fullmove-number
            (if (is-eq current-turn "b")
                (+ fullmove-number u1)
                fullmove-number
            )
          )
          
          ;; Process the board
          (new-board
            (let
              (
                ;; First remove the piece from the source square
                (board-after-remove (set-piece-at board from-x from-y " "))
                
                ;; Determine the piece to place (handle promotion)
                (piece-to-place 
                  (if is-promotion
                      (if (is-eq current-turn "w") "Q" "q")
                      piece
                  )
                )
                
                ;; Place the piece in the target square
                (board-after-move (set-piece-at board-after-remove to-x to-y piece-to-place))
                
                ;; Handle en passant capture (remove the captured pawn)
                (board-after-en-passant
                  (if is-en-passant
                      (set-piece-at board-after-move to-x from-y " ") ;; Remove the pawn that was captured en passant
                      board-after-move
                  )
                )
                
                ;; Handle castling (move the rook)
                (board-after-castling
                  (if is-castling
                      (let
                        (
                          (is-king-side (> to-x from-x))
                          (rook-from-x (if is-king-side u7 u0))
                          (rook-to-x (if is-king-side u5 u3))
                          (rook-piece (if (is-eq current-turn "w") "R" "r"))
                          (step1 (set-piece-at board-after-en-passant rook-from-x from-y " "))
                        )
                        (set-piece-at step1 rook-to-x from-y rook-piece)
                      )
                      board-after-en-passant
                  )
                )
              )
              board-after-castling
            )
          )
          
          ;; Check for checkmate or stalemate (simplified - just checks if king is captured)
          (new-status 
            (cond
              ((not (index-of new-board "k")) u1) ;; White wins
              ((not (index-of new-board "K")) u2) ;; Black wins
              ((is-eq new-halfmove-clock u100)) u3 ;; Draw by 50-move rule
              (true u0) ;; Game continues
            )
          )
          
          ;; Update the game state
          (new-turn (if (is-eq current-turn "w") "b" "w"))
        )
        
        ;; Update game state
        (map-set games
          { game-id: game-id }
          {
            white-player: white-player,
            black-player: black-player,
            board: new-board,
            current-turn: new-turn,
            status: new-status,
            castling-availability: new-castling,
            en-passant-target: new-en-passant,
            halfmove-clock: new-halfmove-clock,
            fullmove-number: new-fullmove-number
          }
        )
        
        (ok true)
      )
    )
  )
)

;; Helper function to replace multiple characters in a string
(define-private (replace-chars (str (string-ascii 4)) (chars-to-remove (string-ascii 2)) (replacement (string-ascii 0)))
  (let
    (
      (c1 (unwrap-panic (element-at chars-to-remove u0)))
      (has-second (> (len chars-to-remove) u1))
      (c2 (if has-second (unwrap-panic (element-at chars-to-remove u1)) ""))
      (str-without-c1 (replace-substring str c1 replacement))
    )
    (if has-second
        (replace-substring str-without-c1 c2 replacement)
        str-without-c1
    )
  )
)

;; Helper function to replace substring
(define-private (replace-substring (str (string-ascii 4)) (pattern (string-ascii 1)) (replacement (string-ascii 0)))
  (let ((index (index-of str pattern)))
    (if (is-some index)
        (let
          (
            (i (unwrap-panic index))
            (before (slice str u0 i))
            (after (slice str (+ i u1) (len str)))
          )
          (concat (concat before replacement) after)
        )
        str
    )
  )
)

;; Resign from a game
(define-public (resign (game-id uint))
  (let
    (
      (game (unwrap! (map-get? games { game-id: game-id }) ERR-GAME-NOT-EXIST))
      (white-player (get game "white-player"))
      (black-player (get game "black-player"))
      (status (get game "status"))
    )
    
    ;; Check if game is in progress
    (asserts! (is-eq status u0) ERR-GAME-OVER)
    
    ;; Check if caller is a player
    (asserts! (or (is-eq tx-sender white-player) (is-eq tx-sender black-player)) ERR-NOT-AUTHORIZED)
    
    ;; Set winner based on who resigned
    (let
      (
        (new-status (if (is-eq tx-sender white-player) u2 u1))
      )
      (map-set games
        { game-id: game-id }
        (merge game { status: new-status })
      )
      
      (ok true)
    )
  )
)

;; Offer draw
(define-data-var draw-offers (list 100 { game-id: uint, player: principal }) (list))

(define-public (offer-draw (game-id uint))
  (let
    (
      (game (unwrap! (map-get? games { game-id: game-id }) ERR-GAME-NOT-EXIST))
      (white-player (get game "white-player"))
      (black-player (get game "black-player"))
      (status (get game "status"))
    )
    
    ;; Check if game is in progress
    (asserts! (is-eq status u0) ERR-GAME-OVER)
    
    ;; Check if caller is a player
    (asserts! (or (is-eq tx-sender white-player) (is-eq tx-sender black-player)) ERR-NOT-AUTHORIZED)
    
    ;; Check if opponent already offered draw
    (let
      (
        (opponent (if (is-eq tx-sender white-player) black-player white-player))
        (opponent-offered (get-draw-offer game-id opponent))
      )
      (if opponent-offered
          ;; Accept draw
          (begin
            (clear-draw-offers game-id)
            (map-set games
              { game-id: game-id }
              (merge game { status: u3 })
            )
            (ok true)
          )
          ;; Offer draw
          (begin
            (var-set draw-offers (append (var-get draw-offers) { game-id: game-id, player: tx-sender }))
            (ok true)
          )
      )
    )
  )
)

;; Helper to check if player has offered draw
(define-read-only (get-draw-offer (game-id uint) (player principal))
  (fold check-draw-offer false (var-get draw-offers))
  
  (define-private (check-draw-offer (offer { game-id: uint, player: principal }) (found bool))
    (or 
      found 
      (and 
        (is-eq (get offer "game-id") game-id)
        (is-eq (get offer "player") player)
      )
    )
  )
)

;; Helper to clear draw offers for a game
(define-private (clear-draw-offers (game-id uint))
  (var-set draw-offers 
    (filter 
      (lambda (offer) (not (is-eq (get offer "game-id") game-id)))
      (var-get draw-offers)
    )
  )
)