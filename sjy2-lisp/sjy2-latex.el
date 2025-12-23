;;; sjy2-latex.el --- BibTeX management for sjy2 -*- lexical-binding: t; -*-(require 'tex)

(use-package auctex
  :after tex
  :defer t
  :hook
  (LaTeX-mode . turn-on-prettify-symbols-mode)
  (LaTeX-mode . turn-on-flyspell)
  (LaTeX-mode . corfu-mode)
  (LaTeX-mode . reftex-mode)            ; Built-in reference management
  (LaTeX-mode . prettify-symbols-mode)
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . auto-fill-mode)          ; Automatic line wrapping
  (LaTeX-mode . display-line-numbers-mode)
  :mode ("\\.tex\\'" . latex-mode)
  :defines (TeX-auto-save
    	      TeX-parse-self
    	      TeX-electric-escape
    	      TeX-PDF-mode
    	      TeX-source-correlate-method
    	      TeX-newline-function
    	      TeX-view-program-list
    	      TeX-view-program-selection
    	      TeX-mode-map)
  :bind
  (:map LaTeX-mode-map
        ("M-RET" . LaTeX-insert-item)
        :map TeX-source-correlate-map     
        ([C-down-mouse-1] . TeX-view-mouse))
  :config
  ;; Explicitly set XeLaTeX as the engine
  (setq-default TeX-engine 'xetex)
  
  ;; Define XeLaTeX and LuaLaTeX engines
  (setq TeX-engine-alist
        '((xetex "XeLaTeX" "xelatex" "xelatex -synctex=1 -interaction=nonstopmode")
          (luatex "LuaLaTeX" "lualatex" "lualatex -synctex=1 -interaction=nonstopmode")))
  
  ;; Explicitly add XeLaTeX and LuaLaTeX to command list
  (add-to-list 'TeX-command-list 
               '("XeLaTeX" "%`xelatex %(mode)%' %t" TeX-run-TeX nil t :help "Run XeLaTeX"))
  (add-to-list 'TeX-command-list 
               '("LuaLaTeX" "%`lualatex %(mode)%' %t" TeX-run-TeX nil t :help "Run LuaLaTeX"))
  
  ;; Set default command and engine
  (setq-default TeX-command-default "XeLaTeX")
  (setq latex-run-command "xelatex")
  
  (TeX-PDF-mode t)  ; compile to PDFs by default
  (setq TeX-auto-save t)
  (setq TeX-electric-escape nil)
  (setq TeX-debug-bad-boxes t)   ; More robust error handling
  (setq TeX-debug-warnings t)    ; More robust error handling
  (setq TeX-error-overview-open-after-TeX-run nil)
  (setq TeX-parse-self t)
  (setq TeX-newline-function 'reindent-then-newline-and-indent)
  (setq TeX-PDF-from-DVI "Dvips")
  (setq TeX-source-correlate-method 'synctex)
  (setq-default 
   TeX-master nil                    ; Always ask for master file
   TeX-save-query nil                ; Save files without asking
   TeX-show-compilation nil          ; Don't show compilation buffer automatically
   LaTeX-indent-level 2              ; Consistent indentation
   LaTeX-item-indent 0               ; Indent of \item
   )

  (setq TeX-view-program-selection
        '(((output-dvi has-no-display-manager) "dvi2tty")
	        ((output-dvi style-pstricks) "dvips and gv")
	        (output-dvi "xdvi")
	        (output-pdf "Sioyek")
	        ;;(output-pdf "PDF Tools")
	        ;;(output-pdf "Zathura"
          (output-html "xdg-open")))

  (setq TeX-view-program-list '(("Sioyek" "sioyek %o")))
  (setq TeX-view-program-selection '((output-pdf "Sioyek")))
  (setq +latex-viewers '(sioyek)))

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)


(use-package outline-magic
  :after auctex
  :hook (LaTeX-mode . outline-minor-mode)
  :bind (:map outline-minor-mode-map
              ("<tab>" . outline-cycle))
  )

(with-eval-after-load 'tex
  (add-to-list 'TeX-command-list 
               '("XeLaTeX" "%`xelatex %(mode)%' %t" TeX-run-TeX nil t :help "Run XeLaTeX"))
  (add-to-list 'TeX-command-list 
               '("LuaLaTeX" "%`lualatex %(mode)%' %t" TeX-run-TeX nil t :help "Run LuaLaTeX")))

(with-eval-after-load 'latex
  (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.synctex\\.gz"))

(custom-set-variables
 '(TeX-view-program-list
   '(("Sioyek"
      ("sioyek %o --reuse-instance"
       (mode-io-correlate " --forward-search-file %b --forward-search-line %n --inverse-search \"emacsclient --no-wait +%2:%3 %1\""))
      "sioyek"))))

(add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
(with-eval-after-load 'latex
  (define-key TeX-source-correlate-map [C-down-mouse-1] #'TeX-view-mouse))

(defun sjy2/debug-latex-compilation ()
  "Debug LaTeX compilation settings."
  (interactive)
  (let ((debug-info 
         (format 
          "TeX-command-list: %s\n\nTeX-engine: %s\n\nTeX-engine-alist: %s\n\nTeX-command-default: %s\n\nlatex-run-command: %s"
          (mapcar #'car TeX-command-list)
          TeX-engine
          TeX-engine-alist
          TeX-command-default
          latex-run-command)))
    (with-output-to-temp-buffer "*LaTeX Debug Info*"
      (princ debug-info))))

(provide 'sjy2-latex)
;;; sjy2-latex.el ends here
