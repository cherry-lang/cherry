(setq cherry-highlights
      '(("module\\|runs\\|fromjs\\|import" . font-lock-function-name-face)))

(define-derived-mode cherry-mode prog-mode "Cherry"
  (setq-default indent-tabs-mode nil)

  (setq font-lock-defaults '(cherry-highlights)))
