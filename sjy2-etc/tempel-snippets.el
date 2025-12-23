fundamental-mode ;; Available everywhere

(isod (format-time-string "%Y-%m-%d"))
(isot (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
(orgd (format-time-string "%Y-%m-%dT%H:%M(%a)"))
(maran "Maranatha" n
       "Bridgnorth Road, Highley" n
       "BRIDGNORTH" n
       "WV16 6JT")

(lorem "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc sed convallis augue. Integer sit amet risus sit amet dui tempor finibus id et erat. Nunc varius turpis est, ut tincidunt ligula finibus vitae. Proin tincidunt felis at lectus tempus, quis feugiat metus aliquet. Mauris vel egestas odio. Donec id sem euismod, placerat eros vel, luctus est. Ut quis mi mauris. Vivamus sagittis efficitur sodales. Sed accumsan bibendum nibh.")

(ubox "┌─" (make-string (length str) ?─) "─┐" n
     "│ " (s str)                       " │" n
     "└─" (make-string (length str) ?─) "─┘" n :doc "UNICODE BOX")

(abox "+-" (make-string (length str) ?-) "-+" n
      "| " (s str)                       " |" n
      "+-" (make-string (length str) ?-) "-+" n :doc "ASCII BOX")

(cut "--8<---------------cut here---------------start------------->8---" n r n
     "--8<---------------cut here---------------end--------------->8---" n)

(rot13 (p "plain text" text) n "----" n (rot13 text))

(sponge (p "Enter text: " text) n "----" n (sjy2/spongebob-case text))

c-mode
;; Function definition
(fun
 "/* " (s "Description") " */" n
 (s type) " " (s name) "(" (s "args") ")" n
 "{" n
 > r n
 "}")

;; If statement
(if
    "if (" p ")" n
    "{" n
    > r n
    "}")

;; If-else statement
(ife
 "if (" p ")" n
 "{" n
 > r n
 "}" n
 "else" n
 "{" n
 > r n
 "}")

;; For loop
(for
 "for (" (s "int i = 0") "; " (s "i < n") "; " (s "i++") ")" n
 "{" n
 > r n
 "}")

;; While loop
(while
    "while (" p ")" n
    "{" n
    > r n
    "}")

;; Struct definition
(struct
 "typedef struct " (s name) " {" n
 > r n
 "} " (s name) ";" n)

;; Standard headers
(head
 "#include <stdio.h>" n
 "#include <stdlib.h>" n
 "#include <string.h>" n
 "#include <stdbool.h>" n
 r)

;; Main function - empty
(imv
 "int main(void)" n
 "{" n
 > r n
 "return 0;" n
 "}")

;; Main with argc and argv
(ima
 "int main(int argc, char *argv[])" n
 "{" n
 > r n
 "return 0;" n
 "}")

;; Main with header comment
(imd
 "/*" n
 " * " (s "Program description") n
 " */" n
 "#include <stdio.h>" n
 "#include <stdlib.h>" n
 n
 "int main(int argc, char *argv[])" n
 "{" n
 > r n
 "return 0;" n
 "}")


html-mode

(p "<p>" n> q n"</p>")
(pre "<pre>" n> q n "</pre>")
(bq "<blockquote>" n> q n "</blockquote>")
(span "<span>" q "</span>")
(div "<div class=\"" (s "class-name") "\">" n > r n "</div>" n)

;; (p "<p>" p "</p>") ;; More inline/concise
;; (pre "<pre>" n> q n "</pre>")
;; (bq "<blockquote>" n> q n "</blockquote>")
;; (span "<span>" p "</span>")



org-mode

(tad "#+title: " p n "#+author: " (s "salopst") n "#+language: en" n "#+date: " (format-time-string "[%Y-%m-%d %a]") n r)

(over "#+begin_verse" n> r> n> "#+end_verse")
(oquo "#+begin_quote" n> r> n> "#+end_quote")
(sh  "#+begin_src shell" n> r> n> "#+end_src" :post (org-edit-src-code))
(elisp  "#+begin_src emacs-lisp" n> r> n> "#+end_src" :post (org-edit-src-code))
(latex
 "#+LATEX_CLASS: article" n
 "#+LATEX_CLASS_OPTIONS: [a4paper,11pt]" n
 "#+LATEX_HEADER: \\usepackage[margin=2.5cm]{geometry}" n
 "#+LATEX_HEADER: \\usepackage{fontawesome5}" n
 "#+OPTIONS: toc:nil" n)
