;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept.

;; FONTS
;; NOTE only apply if fonts are available
(let ((mono-font (find-font (font-spec :family "JetBrainsMono Nerd Font Mono")))
      (pitch-font (find-font (font-spec :family "SauceCodePro Nerd Font"))))
  (if (and mono-font pitch-font)
      ;; Both fonts found, use your config
      (setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 16 :weight 'medium)
            doom-variable-pitch-font (font-spec :family "SauceCodePro Nerd Font" :size 20)
            doom-big-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 25))
    ;; Fonts not found, use Emacs defaults
    (message "Warning: Preferred fonts not found, using system defaults")))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; Emacs transparency (compatible with wayland)
;; Taken from https://www.emacswiki.org/emacs/TransparentEmacs
 (set-frame-parameter (selected-frame) 'alpha '(90 . 85))
 (add-to-list 'default-frame-alist '(alpha . (90 . 85)))

;; Enable built-in desktop save (buffers, windows, etc.)
;; (desktop-save-mode 1)

(defvar doom/frame-geometry-file
  (expand-file-name "frame-geometry.el" user-emacs-directory)
  "File to store frame geometry.")

(defun doom/save-frame-geometry ()
  "Save current frame's geometry to a file."
  (when (and (display-graphic-p)
             (not (daemonp)))
    (let* ((frame (selected-frame))
           (left   (frame-parameter frame 'left))
           (top    (frame-parameter frame 'top))
           (width  (frame-parameter frame 'width))
           (height (frame-parameter frame 'height)))
      ;; Ensure all values are numbers
      (unless (numberp left)   (setq left 0))
      (unless (numberp top)    (setq top 0))
      (unless (numberp width)  (setq width 80))
      (unless (numberp height) (setq height 25))
      (with-temp-buffer
        (insert ";;; Emacs frame geometry\n")
        (insert "(setq initial-frame-alist\n")
        (insert "  '( (top . ")    (insert (number-to-string (max top 0)))    (insert ")\n")
        (insert "     (left . ")  (insert (number-to-string (max left 0)))  (insert ")\n")
        (insert "     (width . ") (insert (number-to-string (max width 0))) (insert ")\n")
        (insert "     (height . ")(insert (number-to-string (max height 0)))(insert ")))\n")
        (when (file-writable-p doom/frame-geometry-file)
          (write-file doom/frame-geometry-file))))))

(defun doom/load-frame-geometry ()
  "Load saved frame geometry and apply to new frames."
  (when (and (display-graphic-p)
             (not (daemonp))
             (file-readable-p doom/frame-geometry-file))
    (load-file doom/frame-geometry-file)))

;; Load geometry early, save on exit (remember window size)
(add-hook 'after-init-hook #'doom/load-frame-geometry)
(add-hook 'kill-emacs-hook #'doom/save-frame-geometry)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord-aurora)

;; Emacs 31 makes gnus-group-news-low inherit from -low-empty. Doom themes
;; defines the reverse inheritance, which creates a face cycle at startup.
(when (eq system-type 'darwin)
  (custom-set-faces
   '(gnus-group-news-low-empty
     ((t (:inherit gnus-group-mail-1-empty :weight normal))))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)

;;Hide ugly empty line tildes
(setq-default indicate-empty-lines nil)

;; Disable line numbers for some modes
(dolist (mode '(
                term-mode-hook
                eshell-mode-hook
                shell-mode-hook
                ))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(setq org-directory "~/org/")

;; Verify org directory exists before setting agenda files (does not create automatically)
(when (file-directory-p org-directory)
  (setq org-agenda-files
        (cl-remove-if-not
         #'file-exists-p
         '("~/org/Todos.org"
           "~/org/work/Todos.org"
           "~/org/work/meeting-notes.org"
           "~/org/Agenda.org"
           "~/org/calendar.org"
           "~/org/Birthdays.org"
           "~/org/Habits.org"
           "~/org/Holidays.org"))))

;; Warn if org directory is missing
(unless (file-directory-p org-directory)
  (display-warning 'my-config
                   (format "Org directory %s does not exist. Org agenda features will be limited." org-directory)
                   :warning))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; CALENDAR settings
;; From https://emacs.stackexchange.com/questions/10965/easiest-way-to-customize-holidays-that-appear-in-org-agenda/13236#13236
(after! calendar
  (setq calendar-week-start-day 1
        calendar-location-name "Oslo"
        holiday-bahai-holidays nil         ; Disable Bahá'í holidays
        holiday-hebrew-holidays nil        ; Disable Hebrew holidays
        holiday-islamic-holidays nil       ; Disable Islamic holidays
        holiday-solar-holidays nil         ; Disable solar holidays
  )
)

;; ORGMODE CONFIG
(defun efs/org-mode-setup ()
  (display-line-numbers-mode 0)
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package! org
  :hook (org-mode . efs/org-mode-setup)
  :config
  ;; Org config
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  (setq org-src-preserve-indentation t)
  (setq org-startup-indented t)
  (setq org-highlight-links '(bracket angle plain tag date footnote))
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (setq org-tags-column -77)
  (setq org-hide-leading-stars t)

  ;; Org Agenda config
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'note)
  (setq org-log-into-drawer t)

  ;; Org TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d@/!)")
          (sequence "OPEN(o)" "IN PROGRESS(i)" "REOPENED(r)" "|""RESOLVED(s@/!)" "CLOSED(c@/!)" "WON'T DO(w@/!)")))

  ;; Org tag list
  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("blog" . ?b)
          ("computing" . ?c)
          ("exercise" . ?e)
          ("health" . ?h)
          ("howto" . ?H)
          ("knitting" . ?k)
          ("maintenance" . ?m)
          ("meeting" . ?M)
          ("note" . ?n)
          ("noexport" .?N)
          ("personal" . ?p)
          ("recipe" . ?r)
          ("TOC" . ?T)
          ("work" . ?w)))

  ;; Org capture templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/work/Todos.org" "Captured todos")
           "* TODO %?\n  %i\n  %a")
          ("p" "Personal Todo" entry (file+headline "~/org/Todos.org" "Captured todos")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("m" "Meeting Notes" entry (file+olp+datetree "~/org/work/meeting_notes.org")
           "* %^{Meeting Title} %^g\nEntered on: %U\n\n%?" :empty-lines 1)
          ("w" "Work Note" plain
           ;; Create new file in ~/org/work/ with dynamic name
           (file
            (lambda ()
              (let* ((dir (expand-file-name "~/org/work/"))
                     (fname (read-string "File name (without .org): "))
                     (full-path (expand-file-name (concat fname ".org") dir)))
                ;; Create directory if missing
                (unless (file-exists-p dir)
                  (make-directory dir :parents))
                full-path)))
           ;; Insert template content
           (file "~/org/Templates/work-note-template.org"))
        )
  )

  ;; ORG-PUBLISH
  (when (file-directory-p (expand-file-name "work" org-directory))
  (setq org-publish-use-timestamps-flag t) ;;only generate on file changes

  (require 'ox-html)
  (require 'ox-publish)
  ;; Export priority cookies so generated headings retain their [A]/[B]/[C]
  ;; markers for the stylesheet and dashboard presentation.
  (setq org-export-with-priority t)
  ;; Inline HTMLize styles preserve colors from the currently active Doom theme
  ;; without hardcoding a theme into the publishing configuration.
  (setq org-html-htmlize-output-type 'inline-css)

  ;; Replace Org outline containers with native disclosures while preserving
  ;; heading IDs, levels, text, and nested exported contents.
  (defun my/org-html-foldable-headline (headline contents info)
    "Export HEADLINE as a collapsed native HTML disclosure element."
    (let ((html (org-html-headline headline contents info)))
      (if (or (not html)
              (not
               (string-match
                "<div\\([^>]*\\)>\n<h\\([1-6]\\) id=\"\\([^\"]+\\)\"\\([^>]*\\)>\\(.*\\)</h[1-6]>\n"
                html)))
          html
        (let* ((outer-attributes (match-string 1 html))
               (level (match-string 2 html))
               (id (match-string 3 html))
               (heading-attributes (match-string 4 html))
               (title (match-string 5 html))
               (contents (substring html (match-end 0))))
          (if (not (string-match "\n</div>\\(?:\n\\)?\\'" contents))
              html
            (concat
             "<details" outer-attributes ">\n"
             "<summary role=\"heading\" aria-level=\"" level
             "\" id=\"" id "\"" heading-attributes ">" title "</summary>\n"
             (substring contents 0 (match-beginning 0))
             "\n</details>\n"))))))

  ;; Keep the backend template path compatible with native folding by wrapping
  ;; the generated table of contents in its own disclosure element.
  (defun my/org-html-foldable-inner-template (contents info)
    "Wrap the generated table of contents in a native disclosure element."
    (let ((html (org-html-template contents info)))
      (if (not
           (string-match
            "<div id=\"table-of-contents\"\\([^>]*\\)>\n<h2>\\([^<]+\\)</h2>\n"
            html))
          html
        (let* ((attributes (match-string 1 html))
               (title (match-string 2 html))
               (start (match-beginning 0))
               (body-start (match-end 0))
               (end (and (string-match "\n</div>\n</div>\n" html body-start)
                         (match-end 0)))
               (close-start (and end (- end (length "\n</div>\n</div>\n")))))
          (if (not end)
              html
            (concat
             (substring html 0 start)
             "<details id=\"table-of-contents\"" attributes ">\n"
             "<summary role=\"heading\" aria-level=\"2\">" title "</summary>\n"
             (substring html body-start close-start)
             "\n</div>\n</details>\n"
             (substring html end)))))))

  ;; Apply the same TOC wrapper after publishing, because the final publisher
  ;; output can bypass the backend template translation hook.
  (defun my/org-html-wrap-toc (html)
    "Wrap generated table of contents HTML in a native disclosure element."
    (if (not
         (string-match
          "<div id=\"table-of-contents\"\\([^>]*\\)>\n<h2>\\([^<]+\\)</h2>\n"
          html))
        html
      (let* ((attributes (match-string 1 html))
             (title (match-string 2 html))
             (start (match-beginning 0))
             (body-start (match-end 0))
             (close-start (string-match
                           "\n</div>\n</div>\n<details"
                           html body-start))
             (end (and close-start
                       (+ close-start (length "\n</div>\n</div>\n")))))
        (if (not close-start)
            html
          (concat
           (substring html 0 start)
           "<details id=\"table-of-contents\"" attributes ">\n"
           "<summary role=\"heading\" aria-level=\"2\">" title "</summary>\n"
           (substring html body-start close-start)
           "\n</div>\n</details>\n"
           (substring html end))))))

  ;; Org emits one generic priority class; add level classes so custom.css can
  ;; distinguish A, B, and C highlights without changing the Org source.
  (defun my/org-html-colorize-priorities (html)
    "Add priority-specific classes to exported priority spans."
    (dolist (priority '("A" "B" "C"))
      (setq html
            (replace-regexp-in-string
             (format "<span class=\"priority\">\\[%s\\]</span>" priority)
             (format "<span class=\"priority priority-%s\">[%s]</span>"
                     (downcase priority) priority)
             html t t)))
    html)

  (org-export-define-derived-backend
      'my-org-html 'html
    :translate-alist '((headline . my/org-html-foldable-headline)
                       (template . my/org-html-foldable-inner-template)))

  ;; Publish with the derived backend, then apply deterministic HTML-only
  ;; adjustments to TOC structure and priority classes.
  (defun my/org-publish-to-foldable-html (plist filename pub-dir)
    "Publish FILENAME using the foldable Org HTML backend."
    (let ((output
           (org-publish-org-to
            'my-org-html filename
            (concat (when (> (length org-html-extension) 0) ".")
                    (or (plist-get plist :html-extension)
                        org-html-extension
                        "html"))
            plist pub-dir)))
      (with-temp-buffer
        (insert-file-contents output)
        (let ((wrapped (my/org-html-colorize-priorities
                        (my/org-html-wrap-toc (buffer-string)))))
          (unless (equal wrapped (buffer-string))
            (erase-buffer)
            (insert wrapped)
            (write-region nil nil output nil 'silent))))
      output))

  (defun my/generate-img-projects ()
    "Generate publishing entries and names for all *-img directories under ~/org/work."
    (let* ((base-dir "~/org/work/")
           (publish-base "~/work-dashboard/")
           (img-dirs (directory-files base-dir t "^[^.]\\{1,\\}-img$"))
           (projects '())
           (names '()))
      (dolist (dir img-dirs)
        (let* ((name (file-name-nondirectory (directory-file-name dir)))
               (project-name (concat "org-" name))
               (target-dir (expand-file-name name publish-base)))
          (push project-name names)
          (push
           `(,project-name
             :base-directory ,dir
             :base-extension "jpg\\|png\\|gif\\|pdf\\|svg"
             :publishing-directory ,target-dir
             :recursive t
             :publishing-function org-publish-attachment)
           projects)))
      ;; Return a list of two things: the new projects, and their names
      (list projects names)))

  (let* ((img-data (my/generate-img-projects))
         (img-projects (nth 0 img-data))
         (img-names (nth 1 img-data))
         (static-projects
          '(("org-work-files"
             :base-directory "~/org/work/"
             :base-extension "org"
             :publishing-directory "~/work-dashboard/"
             :recursive t
             :publishing-function my/org-publish-to-foldable-html
             :headline-levels 4
             :auto-preamble t)
            ("org-presentation-files"
             :base-directory "~/org/work/Presentations/"
             :base-extension "org"
             :publishing-directory "~/work-dashboard/Presentations/"
             :recursive t
             :publishing-function my/org-publish-to-foldable-html
             :headline-levels 4
             :auto-preamble t)
            ("org-work-assets"
             :base-directory "~/org/work/media/"
             :base-extension "jpg\\|png\\|gif\\|pdf\\|svg\\|diff\\|pptx"
             :publishing-directory "~/work-dashboard/media/"
             :recursive t
             :publishing-function org-publish-attachment)
            ("org-work-stylesheets"
             :base-directory "~/org/work/css/"
             :base-extension "css"
             :publishing-directory "~/work-dashboard/css/"
             :recursive t
             :publishing-function org-publish-attachment)
            ("org-presentation-assets"
             :base-directory "~/org/work/Presentations/media/"
             :base-extension "jpg\\|png\\|gif\\|pdf\\|svg\\|diff"
             :publishing-directory "~/work-dashboard/Presentations/media/"
             :recursive t
             :publishing-function org-publish-attachment)))
         ;; Combine component names
         (dashboard-components
          (append '("org-work-files"
                    "org-work-assets"
                    "org-work-stylesheets"
                    "org-presentation-files"
                    "org-presentation-assets")
                  img-names)))

  ;; Final set of projects
  (setq org-publish-project-alist
        (append
         static-projects
         img-projects
         (list `("work-dashboard" :components ,dashboard-components)))))
)) ;; end when org/work exists

;; ORG-HABIT
(use-package! org-habit
  :after org
  :config
  (setq org-habit-following-days 7
        org-habit-preceding-days 25
        org-habit-show-all-today t
        org-habit-graph-column 40
        org-habit-show-habits t))

;; VISUAL-FILL
;;Center org buffers
(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package! visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;;ORGMODE CUSTOM BLOCKS
;;NOTE: see https://github.com/fniessen/org-html-themes/blob/26666aa5c3325dfd11b4c7ec83de53fba260b482/examples/org-mode-syntax-example.org#L702 for syntax on these
(defun my/org-normalize-structure-templates ()
  (dolist (entry '(("n" . "note")
                   ("w" . "warning")
                   ("in" . "info")
                   ("t" . "tip")))
    ;; Replace any existing entry with the same key instead of adding a
    ;; duplicate cons cell.
    (setq org-structure-template-alist
          (cons entry
                (assoc-delete-all (car entry)
                                  org-structure-template-alist)))))

(after! org
  (require 'org-tempo))

(defun my/org-setup-custom-templates ()
  (my/org-normalize-structure-templates)
  (org-tempo-add-templates))

(add-hook 'org-mode-hook #'my/org-setup-custom-templates)

;;HL-TODO
(after! hl-todo
(setq hl-todo-keyword-faces
      '(("TODO" . "#EBCB8B")
        ("NOTE" . "#8FBCBB")
        ("COMMENT" . ,(face-foreground 'font-lock-comment-face))
        ("HACK" . "#D08770")
        ("FIXME" . "#BF616A")
        ("REVIEW" . "#5E81AC")
        ("DEPRECATED" . "#B48EAD")
        ("BUG" . "#B48EAD")
        ("XXX" . "#B48EAD"))))

;; LATEX
;; Some elements taken from: https://www.aidanscannell.com/post/org-mode-resume/
(after! ox-latex
  (setq org-latex-src-block-backend 'minted)
  ;; (setq org-latex-minted-options
  ;;       '(("linenos" "true")
  ;;         ("bgcolor" "bg")
  ;;         ("breaklines" "true")
  ;;         ("frame" "lines")
  ;;         ("framesep" "2mm")
  ;;         ("baselinestretch" "1.2")
  ;;         ("fontsize" "\\footnotesize")))
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; (setq org-latex-with-hyperref nil) ;; was used before, now deprecated
  (setq org-latex-hyperref-template nil) ;; stop adding hypersetup{author..} to latex export
  ;; (setq org-latex-prefer-user-labels t)

  ;; delete unwanted file extensions after latexMK
  (setq org-latex-logfiles-extensions
          (quote ("lof"
                  "lot"
                  "tex~"
                  "aux"
                  "idx"
                  "log"
                  "out"
                  "toc"
                  "nav"
                  "snm"
                  "vrb"
                  "dvi"
                  "fdb_latexmk"
                  "blg"
                  "brf"
                  "fls"
                  "entoc"
                  "ps"
                  "spl"
                  "bbl"
                  "xmpi"
                  "run.xml"
                  "bcf"
                  "acn"
                  "acr"
                  "alg"
                  "glg"
                  "gls"
                  "ist")))

  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nill))
)

;; ignore headlines tagged with :noexport:
(use-package! org-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

;; EVIL
(use-package! evil
  :config
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

;; ORG-ROAM
;; some elements taken from: https://github.com/jethrokuan/dots/blob/master/.doom.d/config.el
(when (file-directory-p (expand-file-name "secondBrain" org-directory))
(use-package! org-roam
  :init
  (setq org-roam-directory (file-truename "~/org/secondBrain")
        org-roam-database-connector 'sqlite-builtin
        org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id t)
  :config
  (org-roam-db-autosync-mode +1)
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)
  (setq org-roam-capture-templates
        '(("m" "main" plain
           (file "~/org/Templates/roam-main-template.org")
           :target (file "main/${slug}.org")
           :immediate-finish t
           :unnarrowed t)
          ("h" "How-To" plain
           (file "~/org/Templates/roam-howto-template.org")
           :target (file "howto/${slug}.org")
           :immediate-finish t
           :unnarrowed t)
          ("d" "D&D root node" plain
           (file "~/org/Templates/dnd-root-node-template.org")
           :target (file "DnD_root_nodes/${slug}.org")
           :immediate-finish t
           :unnarrowed t)
          ("d" "D&D node" plain
           (file "~/org/Templates/dnd-node-template.org")
           :target (file "DnD_nodes/${slug}.org")
           :immediate-finish t
           :unnarrowed t)
         ))
  )
)

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(require 'org-roam-export)

(require 'ox-dnd)

;; MERMAID
(after! mermaid-mode
  (require 'ob-mermaid)
  (setq ob-mermaid-cli-path (executable-find "mmdc"))
  (when (eq system-type 'darwin)
    (setf (alist-get :puppeteer-config-file
                     org-babel-default-header-args:mermaid)
          (expand-file-name "~/pupeteer-mmd.json")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mermaid . t))))

;; ORG-DOWNLOAD
;; NOTE: See https://emacs.stackexchange.com/questions/71100/pasting-images-from-clipboard-into-orgmode
(after! org-download
      (setq org-download-method 'directory)
      (setq org-download-image-dir (concat (file-name-sans-extension (buffer-file-name)) "-img"))
      ;; (setq org-download-image-html-width 1000)
      ;; (setq org-download-image-latex-width 1000)
      ;; (setq org-download-image-org-width 1000)
      (setq org-download-image-attr-list
            '("#+ATTR_ORG: :width 1000 :align center"
              "#+ATTR_HTML: :with 1000 :align center"
              "#+ATTR_LATEX: :options {0.9\\textwidth}"))
      (setq org-download-link-format "[[file:%s]]\n"
        org-download-abbreviate-filename-function #'file-relative-name)
      (setq org-download-link-format-function #'org-download-link-format-function-default))

;; OX-DND-HTML
(use-package! ox-dnd-html :after org)

;; HARPER
;; (after! lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-stdio-connection "harper-ls" "--stdio")
;;     :activation-fn (lsp-activate-on "markdown-mode" "text-mode" "org-mode")
;;     :server-id 'harper-ls)))

;; NOTE: Alternative is to use eglot, init.el must have lsp +eglot
(after! 'eglot
  (add-to-list 'eglot-server-programs
               '(markdown-mode text-mode org-mode . ("harper-ls" "--stdio"))))

(use-package! ox-hugo
  :after ox
  :config
  (setq org-hugo-base-dir "~/hugo_local_setup/lopeztel_blog"))

;; CALDAV
;; Calendar sync
;; https://gitlab.com/hperrey/khalel
(use-package! khalel
  :after org
  :config
;; (setq khalel-khal-command "~/.local/bin/khal")
;; (setq khalel-vdirsyncer-command "vdirsyncer")
(setq khalel-capture-key "e")
(setq khalel-import-org-file (concat org-directory "calendar.org"))
(setq khalel-import-org-file-confirm-overwrite nil)
(setq khalel-default-calendar "Personal")
;; (setq khalel-import-end-date "+30d")
(khalel-add-capture-template))

;; POPTERM
(use-package! popterm
  :hook (after-init . popterm-global-mode)
  :config
  (setq popterm-backend 'vterm
        popterm-scope nil
        popterm-posframe-width-ratio  0.85
        popterm-posframe-height-ratio 0.85
        popterm-posframe-min-width 100
        popterm-posframe-border-width 3
        popterm-auto-cd nil)

  (defvar my/popterm-last "1"
    "The most recently used Popterm instance.")

  (defun my/popterm--create-preserve-source-window (orig-fun &rest args)
    "Prevent terminal creation from replacing the source buffer."
    (let* ((source-window (selected-window))
           (source-buffer (window-buffer source-window)))
      (prog1
          (apply orig-fun args)
        (when (and (window-live-p source-window)
                   (buffer-live-p source-buffer))
          (set-window-buffer source-window source-buffer)
          (select-window source-window)))))

  (advice-add #'popterm--create :around
              #'my/popterm--create-preserve-source-window)

  (defun my/popterm-toggle (&optional count)
    "Toggle the last Popterm, or numbered instance COUNT, without prompting."
    (interactive "P")
    (let ((name (if count
                    (number-to-string (prefix-numeric-value count))
                  my/popterm-last)))
      (setq my/popterm-last name)
      (let ((current-prefix-arg nil)
            (popterm-display-method 'posframe))
        (popterm-toggle name popterm-backend))))

  (map! :n "C-\\" #'my/popterm-toggle))

;; Configure Evil behavior for popterm buffers specifically
(after! evil
  (defun my/popterm-esc-handler ()
    "Send ESC to terminal in popterm buffers, normal behavior elsewhere."
    (interactive)
    (if (string-prefix-p "*popterm" (buffer-name))
        (vterm-send-key "<escape>")
      (evil-normal-state)))

  (add-hook! 'vterm-mode-hook
    (when (string-prefix-p "*popterm" (buffer-name))
      (setq-local vterm-keymap-exceptions '("C-c"))
      (evil-set-initial-state (current-buffer) 'insert)
      (evil-define-key 'insert vterm-mode-map (kbd "<escape>") #'my/popterm-esc-handler)
      (evil-define-key 'normal vterm-mode-map (kbd "<escape>") #'my/popterm-esc-handler)
      (define-key vterm-mode-map (kbd "C-c C-o")
        (lambda ()
          (interactive)
          (if (eq evil-state 'normal)
              (evil-insert-state)
            (evil-normal-state))))
      (setq-local evil-escape-key-sequence nil)
      (setq-local evil-escape-delay nil))))

;;MARKDOWN
;;NOTE Just making it look nicer
(after! markdown-mode
  ;; Use Prettier for SPC c f and format-on-save.
  (set-formatter! 'prettier :modes '(markdown-mode gfm-mode))
  (setq-hook! 'markdown-mode-hook +format-with 'prettier)
  (setq-hook! 'gfm-mode-hook +format-with 'prettier)
  (custom-set-faces
   '(markdown-header-face-1 ((t (:height 1.50 :weight bold))))
   '(markdown-header-face-2 ((t (:height 1.30 :weight bold))))
   '(markdown-header-face-3 ((t (:height 1.18 :weight bold))))
   '(markdown-header-face-4 ((t (:height 1.08 :weight bold))))
   '(markdown-header-face-5 ((t (:height 1.02 :weight bold))))
   '(markdown-header-face-6 ((t (:height 0.98 :weight bold)))))
  (add-hook 'markdown-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (visual-line-mode 1)
              (variable-pitch-mode 1)
              (setq-local visual-fill-column-width 100
                          visual-fill-column-center-text t)
              (visual-fill-column-mode 1)))
  (add-hook 'markdown-mode-hook
            #'markdown-toggle-markup-hiding))

;;fix for locate invalid args
(with-eval-after-load 'consult
  (when (eq system-type 'darwin)
    (setq consult-locate-args "mdfind")))

;; PDF
(after! 'pdf-view
  (define-key pdf-view-mode-map (kbd "C-c x") #'pdf-view-as-text))

(after! 'pdf-text
  (define-key pdf-text-mode-map (kbd "RET") #'pdf-text-show-in-pdf)
  (define-key pdf-text-mode-map (kbd "q") #'bury-buffer))

;;NIX
(setq nix-nixfmt-bin "nixfmt")
