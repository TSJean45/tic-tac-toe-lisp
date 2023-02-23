; set the default list
(defun set-ref()
(setf ref-board(loop for i from 1 to 16 collect i)))

(defun set-emp()
(setf emp-board(loop for i from 1 to 16 collect "-")))

(defun set-three()
(setf three-board-value(loop for i from 1 to 9 collect 0)))

(defun set-four()
(setf four-board-value(loop for i from 1 to 16 collect 0)))

(defun generate-list(size)
(setf random-list(loop for i from 1 to size collect i)))

; starting screen with a list of modes
(defun start-screen( list-of-modes)
               (loop
  (format t "~A~%" "Start Tic-Tac-Toe by Choosing A Mode Below: ")
  (loop for(item) on list-of-modes
        for i from 1
        do (format t "~A. ~A~%" i item))
  (format t "~%Please enter a number: ")
  (setf command-choice (read))
  (case command-choice
    (1 (print-ref 9 (nth 0 list-of-modes))(print-board 9)(first-mode) (return)) ;Human Vs Human (3x3)
    (2 (print-ref 9 (nth 1 list-of-modes))(print-board 9)(second-mode) (return)) ;Human Vs AI (3x3)
    (3 (print-ref 9 (nth 2 list-of-modes))(print-board 9)(third-mode) (return)) ;AI Vs AI (3x3)
    (4 (print-ref 16 (nth 3 list-of-modes))(print-board 16)(fourth-mode)(return)) ;Human Vs Human (4x4)
	(5 (print-ref 16 (nth 4 list-of-modes))(print-board 16)(fifth-mode)(return)) ;Human Vs AI (4x4)
    (6 (print-ref 16 (nth 5 list-of-modes)) (print-board 16)(sixth-mode)(return)) ;AI Vs AI (4x4)
    (otherwise (format t "~A~%~%" "Invalid choice. Please choose again.")(values)))
  ))

; state the list of modes available in this game
(defvar list-of-modes '("Human Vs Human (3x3)" "Human Vs AI (3x3)" "AI Vs AI (3x3)"
"Human Vs Human (4x4)" "Human Vs AI (4x4)" "AI Vs AI (4x4)"))

;print the game board reference with numbers labelled on each slot
(defun print-ref(size mode)
               (format t "~%Please view this game board for reference as players are required 
to choose the number to insert their moves into the according slot.~%")
              (cond ((= size 9) ;game board for a 3x3 board with 9 slots
                     (format t "~%~A" mode)
					 (format t "~%
   |   |
 ~A | ~A | ~A
___|___|___
   |   |
 ~A | ~A | ~A
___|___|___
   |   |
 ~A | ~A | ~A
   |   |
					~%" (nth 0 ref-board) (nth 1 ref-board) (nth 2 ref-board)
						(nth 3 ref-board) (nth 4 ref-board) (nth 5 ref-board)
						(nth 6 ref-board) (nth 7 ref-board) (nth 8 ref-board)
					)(values)
					 )
                    ((= size 16) ;game board for a 4x4 board with 16 slots
               (format t "~A~%" mode)
			   (format t "~%
    |    |    |   
 ~A  | ~A  | ~A  | ~A
____|____|____|____
    |    |    |
 ~A  | ~A  | ~A  | ~A
____|____|____|____
    |    |    |
 ~A  | ~A | ~A | ~A
____|____|____|____
    |    |    |  
 ~A | ~A | ~A | ~A
					~%" (nth 0 ref-board) (nth 1 ref-board) (nth 2 ref-board)(nth 3 ref-board) 
						(nth 4 ref-board) (nth 5 ref-board) (nth 6 ref-board) (nth 7 ref-board)
						(nth 8 ref-board) (nth 9 ref-board) (nth 10 ref-board) (nth 11 ref-board)
						(nth 12 ref-board) (nth 13 ref-board) (nth 14 ref-board) (nth 15 ref-board)
					)(values))))

;allows players to customise their own symbols instead of the common X and O
(defun custom-symbol(type-1 type-2)
  (format t "~%Customising your symbol.~%")
  (loop
(format t "~A 1's Symbol: " type-1)
  (setf p1-symbol (read-line))
  (if (not (= (length p1-symbol) 1)) ;symbol can be anything as long as its one character long
        (format t "~%Symbol can only be one character long. Please try again.~%")
      (return)))
  (loop 
    (format t "~A 2's Symbol: " type-2)
    (setf p2-symbol (read-line))
    (if (or (equal p2-symbol p1-symbol)
	(not (= (length p2-symbol) 1))) ;the two symbols cannot be the same
        (format t "~%~A 2's symbol cannot be the same as ~A 1's and must only be one character long.~%" type-2 type-1)
      (return)))
  (setf p1-value 1)
  (setf p2-value 2))

; Human vs Human (3x3)
(defun first-mode(&optional (board emp-board) (board-value three-board-value))
(custom-symbol "Player" "Player")
  (loop
	(player-turn board board-value p1-symbol p1-value 1 9) ;each player takes turn to play
    (when (has-won board-value p1-value 9) ;each player will check the winning condition
      (format t "~%Congratulations! Player 1 wins! Enter (start) to play the game again.~%")
      (return))
	 (when (full board-value) ;each player will check the tie condition
			  (format t "~%Congratulations, it's a tie! Enter (start) to play the game again.~%")
			  (return))
    (player-turn board board-value p2-symbol p2-value 2 9)
    (when (has-won board-value p2-value 9)
      (format t "~%Congratulations! Player 2 wins! Enter (start) to play the game again.~%")
      (return))
	  (when (full board-value)
			  (format t "~%Congratulations, it's a tie! Enter (start) to play the game again.~%")
			  (return))))

; Human vs AI (3x3)
(defun second-mode(&optional (board emp-board) (board-value three-board-value))
(custom-symbol "Player" "Computer")
  (loop
	(player-turn board board-value p1-symbol p1-value 1 9) ;each player takes turn to play
    (when (has-won board-value p1-value 9) ;each player will check the winning condition
      (format t "~%Congratulations! Player 1 wins! Enter (start) to play the game again.~%")
      (return))
	 (when (full board-value) ;each player will check the tie condition
			  (format t "~%Congratulations, it's a tie! Enter (start) to play the game again.~%")
			  (return))
    (comp-turn board board-value p2-symbol p2-value 2 9)
    (when (has-won board-value p2-value 9)
      (format t "~%Congratulations! Computer 2 wins! Enter (start) to play the game again.~%")
      (return))
	  (when (full board-value)
			  (format t "~%Congratulations, it's a tie! Enter (start) to play the game again.~%")
			  (return))))

; AI vs AI (3x3)			  
(defun third-mode(&optional (board emp-board) (board-value three-board-value))
(custom-symbol "Computer" "Computer")
(betting) ;bet on which computer will win
(generate-list 9) ;generate a list for the randomiser to work on
  (loop
    (comp-turn board board-value p1-symbol p1-value 1 9)
	(add-timer)
    (when (has-won board-value p1-value 9)
		(cond ((= bet-choice 1) ;different message will be printed based on which computer you bet on
		(format t "~%Congratulations! Computer 1 wins and you won the bet! Enter (start) to play the game again.~%"))
		((= bet-choice 2)
		(format t "~%Congratulations to Computer 1! Uh-oh, you bet on the wrong computer! Enter (start) to play the game again.~%")))
      (return))
	 (when (full board-value)
			  (format t "~%Congratulations, it's a tie! Enter (start) to play the game again.~%")
			  (return))
    (comp-turn board board-value p2-symbol p2-value 2 9)
	(add-timer)
    (when (has-won board-value p2-value 9)
      (cond ((= bet-choice 2)
		(format t "~%Congratulations! Computer 2 wins and you won the bet! Enter (start) to play the game again.~%"))
		((= bet-choice 1)
		(format t "~%Congratulations to Computer 2! Uh-oh, you bet on the wrong computer! Enter (start) to play the game again.~%")))
      (return))
	  (when (full board-value)
			  (format t "~%Congratulations, it's a tie! Enter (start) to play the game again.~%")
			  (return))))

;Human vs Human (4x4) 
(defun fourth-mode(&optional (board emp-board) (board-value four-board-value))
(custom-symbol "Player" "Player")
  (loop
    (player-turn board board-value p1-symbol p1-value 1 16)
    (when (has-won board-value p1-value 16)
      (format t "~%Congratulations! Player 1 wins! Enter (start) to play the game again.~%")
      (return))
	 (when (full board-value)
			  (format t "~%Congratulations, it's a tie! Enter (start) to play the game again.~%")
			  (return))
    (player-turn board board-value p2-symbol p2-value 2 16)
    (when (has-won board-value p2-value 16)
      (format t "~%Congratulations! Player 2 wins! Enter (start) to play the game again.~%")
      (return))
	  (when (full board-value)
			  (format t "~%Congratulations, it's a tie! Enter (start) to play the game again.~%")
			  (return))))

; Human vs AI (4x4)
(defun fifth-mode(&optional (board emp-board) (board-value four-board-value))
(custom-symbol "Player" "Computer")
  (loop
	(player-turn board board-value p1-symbol p1-value 1 16) ;each player takes turn to play
    (when (has-won board-value p1-value 16) ;each player will check the winning condition
      (format t "~%Congratulations! Player 1 wins! Enter (start) to play the game again.~%")
      (return))
	 (when (full board-value) ;each player will check the tie condition
			  (format t "~%Congratulations, it's a tie! Enter (start) to play the game again.~%")
			  (return))
    (comp-turn board board-value p2-symbol p2-value 2 16)
    (when (has-won board-value p2-value 16)
      (format t "~%Congratulations! Computer 2 wins! Enter (start) to play the game again.~%")
      (return))
	  (when (full board-value)
			  (format t "~%Congratulations, it's a tie! Enter (start) to play the game again.~%")
			  (return))))

; AI vs AI (4x4)
(defun sixth-mode(&optional (board emp-board) (board-value four-board-value))
(custom-symbol "Computer" "Computer")
(betting)
(generate-list 16)
  (loop
    (comp-turn board board-value p1-symbol p1-value 1 16)
	(add-timer)
    (when (has-won board-value p1-value 16)
     	(cond ((= bet-choice 1)
		(format t "~%Congratulations! Computer 1 wins and you won the bet! Enter (start) to play the game again.~%"))
		((= bet-choice 2)
		(format t "~%Congratulations to Computer 1! Uh-oh, you bet on the wrong computer! Enter (start) to play the game again.~%")))
      (return))
	 (when (full board-value)
			  (format t "~%Congratulations, it's a tie! Enter (start) to play the game again.~%")
			  (return))
    (comp-turn board board-value p2-symbol p2-value 2 16)
	(add-timer)
    (when (has-won board-value p2-value 16)
      	(cond ((= bet-choice 2)
		(format t "~%Congratulations! Computer 2 wins and you won the bet! Enter (start) to play the game again.~%"))
		((= bet-choice 1)
		(format t "~%Congratulations to Computer 2! Uh-oh, you bet on the wrong computer! Enter (start) to play the game again.~%")))
      (return))
	  (when (full board-value)
			  (format t "~%Congratulations, it's a tie! Enter (start) to play the game again.~%")
			  (return))))

; the player's turn where there's user input
(defun player-turn (board board-value symbol value num size)
  (loop
    (format t "~%Please refer to the guideline above to choose a slot")
    (format t "~%Player ~A's turn (~A): " num symbol)
    (setf slot-chosen (read-line))
	(setf slot-chosen (ignore-errors (parse-integer slot-chosen)))
    (cond
	((null slot-chosen)
	(format t "Invalid input. Please enter a number between 1 and ~A.~%" size))
      ((or (< slot-chosen 1) (> slot-chosen size))
       (format t "Invalid input. Please enter a number between 1 and ~A.~%" size))
      ((= (nth (- slot-chosen 1) board-value) 0)
       (setf (nth (- slot-chosen 1) board) symbol)
       (setf (nth (- slot-chosen 1) board-value) value)
       (print-board size)
       (return))
      (t (format t "The slot is already taken. Please try again.~%")))))

; the computer's turn where there's no user input and instead a randomiser
(defun comp-turn (board board-value symbol value num size)
  (loop
    (format t "~%Computer ~A is choosing a slot (~A)...~%" num symbol)
	(random-move random-list)
    (setf slot-chosen random-number)
	; (setf slot-chosen (best-move board board-value value size))
    (cond
      ((= (nth (- slot-chosen 1) board-value) 0)
       (setf (nth (- slot-chosen 1) board) symbol)
       (setf (nth (- slot-chosen 1) board-value) value)
       (print-board size)
	   (format t "~%Computer ~A has choosen slot ~A..." num slot-chosen)
       (return))
      (t (format t "The slot is already taken. Please try again.~%")))))

; a randomiser for the AIvsAI mode
(defun random-move (list)
(let ((index (random (length list))))
  (setf random-number(nth index list))
  (setf random-list(remove (nth index list) list))))

; check to see if the board is already full
(defun full(board-value)
  (loop for element in board-value
        when (zerop element)
          return nil
        finally (return t)))

; a timer to imitate the time interval for the computer to think of a move
(defun add-timer ()
  (loop
    (sleep 1)
    (return)))

; create a function to store the betting value of the player.
(defun betting()
  (loop
  (format t "~%Place your bet on a computer.~%")
(format t "Who do you think will win by chance(1-2): ")
   (setf bet-choice (read-line))
	(setf bet-choice (ignore-errors (parse-integer bet-choice))) ;to detect non-numeric inputs
    (cond
	((null bet-choice)
	(format t "Invalid input. Please enter 1 or 2.~%"))
      ((or (= bet-choice 1) (= bet-choice 2))
	  (format t "You bet on Computer ~A to win the game.~%" bet-choice)
       (return))
      (t (format t "There's only two computers. Please try again.~%"))))) ;loop back to the top if the user input is invalid.

(defun has-won (board value size)
; set the winning conditions for a 3x3 board
(cond ((= size 9)
  (or
   (and (equal (nth 0 board) value) (equal (nth 1 board) value) (equal (nth 2 board) value))
   (and (equal (nth 3 board) value) (equal (nth 4 board) value) (equal (nth 5 board) value))
   (and (equal (nth 6 board) value) (equal (nth 7 board) value) (equal (nth 8 board) value))
   (and (equal (nth 0 board) value) (equal (nth 3 board) value) (equal (nth 6 board) value))
   (and (equal (nth 1 board) value) (equal (nth 4 board) value) (equal (nth 7 board) value))
   (and (equal (nth 2 board) value) (equal (nth 5 board) value) (equal (nth 8 board) value))
   (and (equal (nth 0 board) value) (equal (nth 4 board) value) (equal (nth 8 board) value))
   (and (equal (nth 2 board) value) (equal (nth 4 board) value) (equal (nth 6 board) value))))
   ;set the winning conditions for a 4x4 board
   ((= size 16)
   (or
   (and (equal (nth 0 board) value) (equal (nth 1 board) value) (equal (nth 2 board) value) (equal (nth 3 board) value))
   (and (equal (nth 4 board) value) (equal (nth 5 board) value) (equal (nth 6 board) value) (equal (nth 7 board) value))
   (and (equal (nth 8 board) value) (equal (nth 9 board) value) (equal (nth 10 board) value) (equal (nth 11 board) value))
   (and (equal (nth 12 board) value) (equal (nth 13 board) value) (equal (nth 14 board) value) (equal (nth 15 board) value))
   (and (equal (nth 0 board) value) (equal (nth 4 board) value) (equal (nth 8 board) value) (equal (nth 12 board) value))
   (and (equal (nth 1 board) value) (equal (nth 5 board) value) (equal (nth 9 board) value) (equal (nth 13 board) value))
   (and (equal (nth 2 board) value) (equal (nth 6 board) value) (equal (nth 10 board) value) (equal (nth 14 board) value))
   (and (equal (nth 3 board) value) (equal (nth 7 board) value) (equal (nth 11 board) value) (equal (nth 15 board) value))
   (and (equal (nth 0 board) value) (equal (nth 5 board) value) (equal (nth 10 board) value) (equal (nth 5 board) value))
   (and (equal (nth 3 board) value) (equal (nth 6 board) value) (equal (nth 9 board) value) (equal (nth 12 board) value))
   (and (equal (nth 0 board) value) (equal (nth 1 board) value) (equal (nth 4 board) value) (equal (nth 5 board) value))
   (and (equal (nth 2 board) value) (equal (nth 1 board) value) (equal (nth 6 board) value) (equal (nth 5 board) value))
   (and (equal (nth 2 board) value) (equal (nth 3 board) value) (equal (nth 6 board) value) (equal (nth 7 board) value))
   (and (equal (nth 8 board) value) (equal (nth 9 board) value) (equal (nth 4 board) value) (equal (nth 5 board) value))
   (and (equal (nth 9 board) value) (equal (nth 10 board) value) (equal (nth 6 board) value) (equal (nth 5 board) value))
   (and (equal (nth 10 board) value) (equal (nth 11 board) value) (equal (nth 6 board) value) (equal (nth 7 board) value))
   (and (equal (nth 8 board) value) (equal (nth 9 board) value) (equal (nth 12 board) value) (equal (nth 13 board) value))
   (and (equal (nth 9 board) value) (equal (nth 10 board) value) (equal (nth 13 board) value) (equal (nth 14 board) value))
   (and (equal (nth 10 board) value) (equal (nth 11 board) value) (equal (nth 14 board) value) (equal (nth 15 board) value))
   ))))

; print the current board
(defun print-board(size)
(format t "~%Starting game...")
(cond ((= size 9)
					 (format t "~%
   |   |
 ~A | ~A | ~A
___|___|___
   |   |
 ~A | ~A | ~A
___|___|___
   |   |
 ~A | ~A | ~A
   |   |
					~%" (nth 0 emp-board) (nth 1 emp-board) (nth 2 emp-board)
						(nth 3 emp-board) (nth 4 emp-board) (nth 5 emp-board)
						(nth 6 emp-board) (nth 7 emp-board) (nth 8 emp-board)
					)(values)
					 )
                    ((= size 16)
			   (format t "~%
    |    |    |   
 ~A  | ~A  | ~A  | ~A
____|____|____|____
    |    |    |
 ~A  | ~A  | ~A  | ~A
____|____|____|____
    |    |    |
 ~A  | ~A  | ~A  | ~A
____|____|____|____
    |    |    |  
 ~A  | ~A  | ~A  | ~A
					~%" (nth 0 emp-board) (nth 1 emp-board) (nth 2 emp-board)(nth 3 emp-board) 
						(nth 4 emp-board) (nth 5 emp-board) (nth 6 emp-board) (nth 7 emp-board)
						(nth 8 emp-board) (nth 9 emp-board) (nth 10 emp-board) (nth 11 emp-board)
						(nth 12 emp-board) (nth 13 emp-board) (nth 14 emp-board) (nth 15 emp-board)
					)(values)
			   )))

;every initialisations needed to start the game
(defun start()
(set-ref)
(set-emp)
(set-three)
(set-four)
(start-screen list-of-modes))

; start the game when the program is loaded
(start)
