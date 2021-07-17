;;; sampletex-mode.el --- a simple major mode for editing sampletex.

(defvar sampletex-highlights nil "first element for `font-lock-defaults'")

(setq sampletex-highlights (let* (
            ;; define several category of keywords
            (x-keywords '("li" "il" "newpage" "sec" "var" "import" "out"))

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words)))
            
        `(
          (,x-keywords-regexp . font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))
			    
;;;###autoload
(define-derived-mode sampletex-mode fundamental-mode "sample" "major mode for editing sampletex"
  (setq font-lock-defaults '(sampletex-highlights))
  )
;;; sampletex-mode.el ends here
