(setq cherry-keywords '("module" "runs" "exports" "fromjs" "from" "import"
                        "infix" "infixl" "infixr" "class" "extends" "with"
                        "trait" "interface"))

(setq cherry-constants '("True" "False"))

(setq cherry-keywords-regexp (regexp-opt cherry-keywords 'word))
(setq cherry-constants-regexp (regexp-opt cherry-constants 'word))

(setq cherry-font-lock-keywords
      `((,cherry-constants-regexp . font-lock-constant-face)
        (,cherry-keywords-regexp . font-lock-keyword-face)
        ("^\\([a-z_][0-9A-Za-z_']*\\|([^)]+)\\)" . font-lock-function-name-face)
        ("\\<[A-Z][0-9A-Za-z_]*" . font-lock-type-face)
        ))

(define-derived-mode cherry-mode prog-mode "Cherry"
  (setq-default indent-tabs-mode nil)

  (setq font-lock-defaults '((cherry-font-lock-keywords))))

(provide 'cherry-mode)
