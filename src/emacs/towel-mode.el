(setq tm-keywords
      '("if>=0" "if>0" "if<=0" "if<0" "if=0" "if~0" "ift" "iff" "ife" "ifne"
	"match" "bind" "also" "then" "fun" "type" "export" "import" "idle"
	"`" "\\."))

(setq tm-keywords-regexp (regexp-opt tm-keywords 'words))

(setq tm-font-lock-keywords
      `((,tm-keywords-regexp . font-lock-keyword-face)))

(define-derived-mode towel-mode fundamental-mode
  "Towel mode"
  (setq font-lock-defaults '((tm-font-lock-keywords))))

(provide 'towel-mode)
