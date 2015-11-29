(setq tm-keywords
      '("if>=0" "if>0" "if<=0" "if<0" "if=0" "if~0" "ift" "iff" "ife" "ifne"
	"match" "bind" "also" "then" "fun" "type" "export" "import" "idle"
	"`" "\\." "@"))

(setq tm-keywords-regexp (regexp-opt tm-keywords 'words))

(defconst valid-upper-char "[A-Z~!#$%^&*_=|:<>?/]")
(defconst common-valid-char "[a-zA-Z0-9~!#$%^&*-_=|:<>?/]")
(defconst common-valid-char-no-digits "[a-zA-Z~!#$%^&*-_=|:<>?/]")
(defconst tm-name-regexp
  (format "\\(%s%s*\\)\\|\\(\\+%s?\\)\\|\\(\\+%s%s*\\)\\|\\(-%s?\\)\\|\\(-%s%s*\\)"
	  valid-upper-char common-valid-char
	  common-valid-char-no-digits
	  common-valid-char-no-digits common-valid-char
	  common-valid-char-no-digits
	  common-valid-char-no-digits common-valid-char))

(setq tm-font-lock-keywords
      `((,tm-keywords-regexp . font-lock-keyword-face)
	("\\('[^']*'\\)" . font-lock-string-face)
	("\\(\"[^\"]*\"\\)" . font-lock-comment-face)
	(,tm-name-regexp . font-lock-variable-name-face)
	("\\([a-z][a-zA-Z0-9~!#$%^&*-_+=|:<>?/]*\\)" . font-lock-constant-face)
	))

(define-derived-mode towel-mode fundamental-mode
  "Towel mode"
  (setq font-lock-defaults '((tm-font-lock-keywords))))

(provide 'towel-mode)
