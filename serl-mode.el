(defconst serl-symbol-re
  "\\S *")

(defconst serl-atom-char1
  "-a-z*/+"
  "The first char of an atom.")

(defconst serl-atom-char+
  (concat serl-atom-char1 "A-Z0-9_")
  "The other chars of an atom")

(defconst serl-symbol-char-re
  (concat "\\(" serl-alnum "\\|[-_+*/])")
  "symbol constituient except first char.")

(defconst serl-atom-re
  (concat "[" serl-atom-char1 "]"
	  "[" serl-atom-char+ "]*"))

(defconst serl-var-re
  (concat "[_A-Z]"
	  "[" serl-atom-char+ "]*"))


(defvar serl-mode-map
  nil)

(defvar serl-mode-hook nil)

(unless serl-mode-map
  (setq serl-mode-map
	(let ((map (make-keymap)))
	  (set-keymap-parent map lisp-mode-map))))


(defun serl-syntax-table () 
  (with-syntax-table (make-syntax-table)
    (modify-syntax-entry ?\- "_")
    (modify-syntax-entry ?\_ "_")
    ;; comment
    (modify-syntax-entry ?# "<")
    (modify-syntax-entry ?\n ">")
    ;; punctuation
    (modify-syntax-entry ?\\ ".")
    (modify-syntax-entry ?\: ".")
    (modify-syntax-entry ?\~ ".")
    (modify-syntax-entry ?\. ".")
    ;; parenthesis
    (modify-syntax-entry ?\( "()")
    (modify-syntax-entry ?\) ")(" )
    (modify-syntax-entry ?\{ "(}" )
    (modify-syntax-entry ?\} "){" )
    (modify-syntax-entry ?\[ "(]" )
    (modify-syntax-entry ?\] ")[" )
    ;; string
    (modify-syntax-entry ?\" "\"")
    ;;(modify-syntax-entry ?\\ "\\") ;; this for some reason messes up \ as punctuation
    ;; expression prefix
    (modify-syntax-entry ?\' "\'")
    (modify-syntax-entry ?\` "\'")
    (modify-syntax-entry ?\, "\'")
    (modify-syntax-entry ?\; "\'")
    (syntax-table)))

(defvar serl-syntax-table nil)

(unless serl-syntax-table
  (setq serl-syntax-table (serl-syntax-table)))

(defvar serl-font-lock-keywords nil)

(unless serl-font-lock-keywords
  (setq serl-font-lock-keywords
	(list
	 (list (concat "(\\s *\\(def\\|defm\\)\\s +\\("
		       serl-atom-re
		       "\\)")
	       2 font-lock-function-name-face)
	 (cons (concat "\\b\\("
		       (regexp-opt
			'("def"
			  "defm"
			  "module"
			  "import"
			  "export"
			  "case"
			  "try"
			  "catch"
			  "finally"
			  "xyz"
			  ))
		       "\\)\\b")
	       1)
	 (cons (concat "\\b" serl-var-re "\\b")
	       'font-lock-variable-name-face)
	 ;;(cons "[:~.{}]" 'font-lock-variable-name-face)
	 '("[{}]" . 'serl-brace)
	 '("[:~.\\]" . font-lock-variable-name-face)
	 ))
  ) 

(defface serl-brace
    '((t :height 1.1))
  "Face for {}")


(defun serl-mode ()
  "Major mode for Serl"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'serl-mode)
  (setq mode-name "Serl")
  (set-syntax-table serl-syntax-table)
  (use-local-map serl-mode-map)
  (make-local-variable 'font-lock-defaults)
  ;;(make-local-variable 'font-lock-keywords)
  (setq font-lock-defaults '((serl-font-lock-keywords) nil nil (("-_" . "w")))) 
  (make-variable-buffer-local 'comment-start)
  (setq comment-start "# ")
  (make-variable-buffer-local 'comment-end)
  (setq comment-end "")
  ;; (make-variable-buffer-local 'comment-column)
  ;;   (setq comment-column 32) 
  (make-variable-buffer-local 'comment-start-skip)
  (setq comment-start-skip "#+ *")
  ;;(setq indent-tabs-mode ruby-indent-tabs-mode)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (setq indent-line-function 'lisp-indent-line)
  (setq lisp-indent-function 'common-lisp-indent-function)
  (run-mode-hooks 'serl-mode-hook)
  )


;; font-lock-maximum-decoration
