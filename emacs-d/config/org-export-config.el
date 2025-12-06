;;; org-export-config.el --- Org export configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 SeungJun Choi

;;; Commentary:

;; Export configuration:
;; - PDF via XeLaTeX (CJK-friendly)
;; - HTML with custom styling
;; - Markdown (GFM)
;; - Presentations (org-reveal, org-present)
;;
;; Prerequisites for PDF:
;; - macOS: brew install --cask mactex
;; - Ubuntu: apt install texlive-full

;;; Code:

;;; Export Backends
(with-eval-after-load 'org
  (require 'ox-md)
  (require 'ox-beamer)
  
  (setq org-export-backends '(ascii html latex md beamer)))

;;; PDF Export via LaTeX (XeLaTeX for CJK)
(with-eval-after-load 'ox-latex
  ;; Use XeLaTeX for CJK support
  (setq org-latex-compiler "xelatex")
  
  ;; PDF process
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  
  ;; CJK-friendly article class
  (add-to-list 'org-latex-classes
               '("cjk-article"
                 "\\documentclass[11pt,a4paper]{article}
\\usepackage{fontspec}
\\usepackage{xeCJK}
\\usepackage{geometry}
\\geometry{margin=1in}

% CJK Fonts - Sarasa Gothic for consistency with Emacs
\\setCJKmainfont{Sarasa UI SC}
\\setCJKsansfont{Sarasa Gothic SC}
\\setCJKmonofont{Sarasa Mono SC}

% Fallback for Japanese/Korean specific characters
\\xeCJKDeclareSubCJKBlock{Kana}{\"3040 -> \"309F, \"30A0 -> \"30FF}
\\xeCJKDeclareSubCJKBlock{Hangul}{\"1100 -> \"11FF, \"AC00 -> \"D7AF}

% You can set different fonts per language if needed:
% \\setCJKmainfont[Kana]{Sarasa UI J}
% \\setCJKmainfont[Hangul]{Sarasa UI K}

% Better defaults
\\usepackage{hyperref}
\\hypersetup{colorlinks=true,linkcolor=blue,urlcolor=blue}
\\usepackage{enumitem}
\\setlist{noitemsep}

[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  
  ;; Simple article (without CJK setup)
  (add-to-list 'org-latex-classes
               '("simple-article"
                 "\\documentclass[11pt,a4paper]{article}
\\usepackage{geometry}
\\geometry{margin=1in}
\\usepackage{hyperref}
\\hypersetup{colorlinks=true,linkcolor=blue,urlcolor=blue}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  
  ;; Default to CJK-friendly class
  (setq org-latex-default-class "cjk-article")
  
  ;; Syntax highlighting with minted
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-minted-options
        '(("frame" "lines")
          ("fontsize" "\\small")
          ("breaklines" "true")
          ("breakanywhere" "true")))
  
  ;; Remove default packages we don't want
  (setq org-latex-default-packages-alist
        '(("AUTO" "inputenc" t ("pdflatex"))
          ("T1" "fontenc" t ("pdflatex"))
          ("" "graphicx" t)
          ("" "longtable" nil)
          ("" "wrapfig" nil)
          ("" "rotating" nil)
          ("normalem" "ulem" t)
          ("" "amsmath" t)
          ("" "amssymb" t)
          ("" "capt-of" nil)
          ("" "hyperref" nil))))

;;; HTML Export
(with-eval-after-load 'ox-html
  ;; Don't include default styles
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-include-scripts nil)
  
  ;; Custom CSS
  (setq org-html-head
        "<style>
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
  max-width: 800px;
  margin: 0 auto;
  padding: 2rem;
  line-height: 1.6;
  color: #333;
}
h1, h2, h3, h4 { color: #1a1a1a; margin-top: 1.5em; }
a { color: #0066cc; }
pre, code {
  font-family: 'Sarasa Mono SC', 'JetBrains Mono', monospace;
  background: #f5f5f5;
  border-radius: 4px;
}
pre { padding: 1rem; overflow-x: auto; }
code { padding: 0.2rem 0.4rem; }
pre code { padding: 0; }
blockquote {
  border-left: 4px solid #ddd;
  margin: 1rem 0;
  padding-left: 1rem;
  color: #666;
}
table { border-collapse: collapse; width: 100%; }
th, td { border: 1px solid #ddd; padding: 0.5rem; text-align: left; }
th { background: #f5f5f5; }
.todo, .done { font-weight: bold; }
.todo { color: #e74c3c; }
.done { color: #27ae60; }
</style>")
  
  ;; Use CSS classes for syntax highlighting
  (setq org-html-htmlize-output-type 'css)
  
  ;; Validation link
  (setq org-html-validation-link nil)
  
  ;; Postamble
  (setq org-html-postamble t)
  (setq org-html-postamble-format
        '(("en" "<p class=\"postamble\">Last updated: %T</p>"))))

;;; GitHub Flavored Markdown
(use-package ox-gfm
  :ensure t
  :after org
  :config
  (add-to-list 'org-export-backends 'gfm))

;;; Hugo Export
(use-package ox-hugo
  :ensure t
  :after org
  :config
  ;; Auto-export on save
  ;; (org-hugo-auto-export-mode)
  )

;;; Reveal.js Presentations
(use-package ox-reveal
  :ensure t
  :after org
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (setq org-reveal-theme "simple")
  (setq org-reveal-plugins '(notes search zoom))
  (setq org-reveal-title-slide "<h1>%t</h1><h3>%a</h3><p>%d</p>"))

;;; org-present - In-Emacs Presentations
(use-package org-present
  :ensure t
  :commands org-present
  :hook ((org-present-mode . my/org-present-start)
         (org-present-mode-quit . my/org-present-end))
  :config
  (defun my/org-present-start ()
    "Configure org-present mode."
    (org-present-big)
    (org-display-inline-images)
    (org-present-hide-cursor)
    (org-present-read-only)
    (setq-local face-remapping-alist
                '((default (:height 1.5) variable-pitch)
                  (header-line (:height 4.0) variable-pitch)
                  (org-document-title (:height 2.0) org-document-title)
                  (org-code (:height 1.2) org-code)
                  (org-block (:height 1.2) org-block)
                  (org-verbatim (:height 1.2) org-verbatim)))
    (setq header-line-format " "))
  
  (defun my/org-present-end ()
    "Restore normal org-mode."
    (org-present-small)
    (org-remove-inline-images)
    (org-present-show-cursor)
    (org-present-read-write)
    (setq-local face-remapping-alist nil)
    (setq header-line-format nil)))

;;; Beamer Presentations
(with-eval-after-load 'ox-beamer
  (add-to-list 'org-latex-classes
               '("beamer-cjk"
                 "\\documentclass[presentation]{beamer}
\\usepackage{fontspec}
\\usepackage{xeCJK}
\\setCJKmainfont{Sarasa UI SC}
\\setCJKsansfont{Sarasa Gothic SC}
\\setCJKmonofont{Sarasa Mono SC}
\\usetheme{metropolis}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}"))))

;;; Export Helper Functions

(defun my/org-export-to-pdf ()
  "Export current org file to PDF using XeLaTeX."
  (interactive)
  (org-latex-export-to-pdf))

(defun my/org-export-to-html ()
  "Export current org file to HTML and open in browser."
  (interactive)
  (let ((html-file (org-html-export-to-html)))
    (browse-url (concat "file://" html-file))))

(defun my/org-export-to-md ()
  "Export current org file to GitHub Flavored Markdown."
  (interactive)
  (org-gfm-export-to-markdown))

;;; Keybindings
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c e p") #'my/org-export-to-pdf)
  (define-key org-mode-map (kbd "C-c e h") #'my/org-export-to-html)
  (define-key org-mode-map (kbd "C-c e m") #'my/org-export-to-md)
  (define-key org-mode-map (kbd "C-c e r") #'org-reveal-export-to-html)
  (define-key org-mode-map (kbd "C-c e P") #'org-present))

(provide 'org-export-config)
;;; org-export-config.el ends here
