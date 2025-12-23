;; -*- lexical-binding: t; byte-compile-warnings: (not lexical); -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-list
   '(("Sioyek"
      ("sioyek %o --reuse-instance"
       (mode-io-correlate
	" --forward-search-file %b --forward-search-line %n --inverse-search \"emacsclient --no-wait +%2:%3 %1\""))
      "sioyek")))
 '(bibtex-aux-opt-alist
   '(("keywords" "Keywords to describe the entry" "")
     ("file" "Link to a document file" "")) nil nil "Customized with use-package bibtex")
 '(org-fold-catch-invisible-edits 'show-and-error nil nil "Customized with use-package org")
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((kbd-mode :url "https://github.com/kmonad/kbd-mode")
     (vundo :url "https://github.com/casouri/vundo")
     (minions :url "https://github.com/tarsius/minions.git")
     (org-noter :url "https://github.com/weirdNox/org-noter")
     (org-sticky-header-mode :url
			     "https://github.com/alphapapa/org-sticky-header")
     (denote-menu :url "https://github.com/namilus/denote-menu")
     (bibtex-actions :url "https://github.com/mbosley/bibtex-actions")
     (nov :url "https://github.com/emacsmirror/nov")
     (dired-hist :url "https://github.com/karthink/dired-hist.git")
     (ultra-scroll :url "https://github.com/jdtsmith/ultra-scroll")
     (rainbow-mode :url "https://github.com/emacsmirror/rainbow-mode/")
     (async :url "https://github.com/jwiegley/emacs-async.git")
     (keycast :url "https://github.com/tarsius/keycast.git")
     (doom-modeline :url
		    "https://github.com/seagle0128/doom-modeline.git")
     (mixed-pitch :url "https://github.com/emacsmirror/mixed-pitch")
     (nerd-icons :url "https://github.com/emacsmirror/nerd-icons")
     (ef-themes :url "https://github.com/protesilaos/ef-themes")
     (modus-themes :url "https://gitlab.com/protesilaos/modus-themes"
		   :branch "main")
     (paw :url "https://github.com/chenyanming/paw")
     (org :url "https://git.savannah.gnu.org/git/emacs/org-mode.git"
	  :rev :newest)
     (org-git :url
	      "https://git.savannah.gnu.org/git/emacs/org-mode.git"
	      :lisp-dir "lisp")
     (typst-ts-mode :url "https://git.sr.ht/~meow_king/typst-ts-mode/")
     (esup :url "https://github.com/jschaf/esup")
     (denote-explore :url "https://github.com/pprevos/denote-explore")
     (citar-denote :url "https://github.com/pprevos/citar-denote")))
 '(zoom-size 'size-callback))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Iosevka"))))
 '(font-lock-comment-face ((t (:foreground "#95A99F" :slant italic))))
 '(git-gutter-fr:added ((t (:foreground "#87CF70" :background "#87CF70"))))
 '(git-gutter-fr:deleted ((t (:foreground "#F15952" :background "#F15952"))))
 '(git-gutter-fr:modified ((t (:foreground "#4B919E" :background "#4B919E"))))
 '(keycast-key ((t (:background "#6D7587" :foreground "#E4C869" :weight bold))))
 '(mode-line-emphasis ((t (:foreground "#E4C869" :weight bold))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-block-begin-line ((t (:inherit org-meta-line :background "#e8e8e8" :foreground "#666666" :height 0.9))))
 '(org-block-end-line ((t (:inherit org-block-begin-line))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-modern-label ((t (:inherit fixed-pitch :height 0.9))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(term ((t (:background "#002244" :foreground "#95A99F" :weight bold))))
 '(variable-pitch ((t (:family "Iosevka")))))
