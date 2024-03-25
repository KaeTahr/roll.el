;;; roll.el --- Roll die for skill checks and choices in GNU Emacs

;;; Commentary:

;; Is your boyfriend tired of having to decide stuff for you?
;; Not sure what to eat?
;; Now, you can ask Emacs to choose your fate for you!

;;; Code:

;; Seed the random function
(random t)

(defun roll (dice)
  "Rolls a given number of dice of a given type, as defined by DICE."
  (interactive "sWhat would you like to roll? ")
  (let ((roll (split-string dice "d")))
    ;; This amount of conditionals is really ugly; I'll have to reformat this later
    ;; Keeping this comment here until I get shamed enough to fix it.
    (cond ((posix-string-match "[0-9]+d[0-9]+" dice)
	   (message (roll-dice (string-to-number (car roll)) (string-to-number (car (last roll))))))
	  ((posix-string-match "[0-9]+dF" dice)
	   (message (roll-fudge-dice (string-to-number (car roll)))))
	  ((posix-string-match "d[0-9]+" dice)
	   (message (roll-dice 1 (string-to-number (car (last roll))))))
	  ((posix-string-match "dF" dice)
	   (message (roll-fudge-dice 1)))
	  (t (message "Roll syntax: <AMOUNT>d<SIDES>. AMOUNT is optional, SIDES can be 'F' for Fudge dice."))
	  )
    )
  )

(defun roll-fudge ()
  "Rolls an individual Fudge die."
  (let ((dice (list 1 1 0 0 -1 -1)))
    (nth (random 6) dice)
    )
  )

(defun num-to-fudge (num)
  "Convert NUM to a Fudge symbol.
Returns NUM if it's not a valid Fudge value."
  (cond ((eq num -1) "[-]")
	((eq num 0) "[ ]")
	((eq num 1) "[+]")
	(t num)
	)
  )

(defun roll-fudge-dice (amount)
  "Rolls a given AMOUNT of Fudge dice."
  (let ((output nil))
    (dotimes (i amount)
      (push (roll-fudge) output)
      )
    (push (concat "= " (number-to-string (apply '+ output))) (cdr (last output)))
    (push "You rolled:" output)
    (mapconcat 'num-to-fudge output " ")
    )
  )

(defun roll-dice (amount sides)
  "Rolls AMOUNT dice with SIDES number of sides each."
  (if (eq amount 1)
      (concat (replace-regexp-in-string "num" (number-to-string sides) "Your Dnum rolls: ")
	      (number-to-string (+ 1 (random sides))))
    (let ((output nil))
      (dotimes (i amount)
	(push (+ 1 (random sides)) output)
	)
      (unless (eq (length output) 1)
	(push (concat "= " (number-to-string (apply '+ output))) (cdr (last output))))
      (push "You rolled:" output)
      (mapconcat (lambda (x) (cond ((numberp x) (number-to-string x))(t x))) output " ")
      )
    )
  )
(provide 'roll)
;;; roll.el ends here
