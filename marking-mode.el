(require 'font-lock)

(defgroup marking-mode nil
  "Major mode for editing MARKING.txt files."
  :group 'languages)

(defcustom marking-mode-hook nil
  "Hook run when entering Vimperator mode."
  :type 'hook
  :group 'marking)

(defface marking-success-face '((t :inherit (success)))
  "Success keyword face."
  :group 'marking-mode)
(defface marking-fail-face '((t :inherit (error)))
  "Fail keyword face."
  :group 'marking)
(defface marking-separator-face '((t :inherit font-lock-comment-face))
  "Separator face."
  :group 'marking)
(defface marking-testing-face '((t :inherit font-lock-type-face))
  "Wort Testing face."
  :group 'marking)
(defface marking-test-name-face '((t :inherit font-lock-variable-name-face))
  "Name of the test face."
  :group 'marking)
(defface marking-mark-face '((t :inherit (underline)))
  "A mark."
  :group 'marking)

(defvar-local marking-submission-directory nil
  "Directory that corresponds tho the submission that the buffer
  refers to. This should be set if the buffer does not refer to a
  file.")

(defconst marking-font-lock-keywords
  '(("^ *\\(Testing\\|Executing\\)" 0 'marking-testing-face)
    ("^Testing \\(.*\\)..." 1 'marking-test-name-face)
    (" *\\(Failed\\|crashed\\)" 1 'marking-fail-face)
    (" \\(__\\) " 1 'marking-fail-face)
    (" *\\(Passed\\)" 1 'marking-success-face)
    ("[0-9]*\\( marks?\\|\\) out of [0-9]* (.*)" 0 'marking-mark-face))
  "Subdued level highlighting for Vimperator mode.")

(defvar marking-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") nil)
    map)
  "Keymap for using `marking-mode'.")

(defvar marking-part-names
  '(("A" . "statements")
    ("B" . "pos_tagging")
    ("C" . "agreement")
    ("D" . "semantics"))
  "What part corresponds to what file")

(defun marking-jump-to-part (search btn)
  (find-file (concat (cdr (assoc (button-get btn 'part) marking-part-names)) ".py"))
  (when search
    (goto-char (point-min))
    (search-forward-regexp search)))

(defun marking-submission-directory-fn (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (or marking-submission-directory
        (when (buffer-file-name) (file-name-directory (buffer-file-name)))
        default-directory)))

(defun marking-kill-submission-buffers ()
  (interactive)
  (dolist (b (buffer-list))
    (when (string= (marking-submission-directory-fn)
                   (marking-submission-directory-fn b))
      (kill-buffer b))))

(defun marking-current-part ()
  (save-match-data
    (save-excursion
      (search-backward-regexp "^Part \\([ABCD]\\):" nil t)
      (match-string-no-properties 1))))

(defun marking-make-buttons ()
  (interactive)
  (save-excursion
    ;; Parts
    (goto-char (point-min))
    (while (search-forward-regexp "^Part \\([ABCD]\\):" nil t)
      (make-button (match-beginning 0) (match-end 0)
                   'part (match-string-no-properties 1)
                   'action (lambda (btn) (marking-jump-to-part nil btn))))

    ;; Tests
    (goto-char (point-min))
    (while (search-forward-regexp "^Testing \\(.*\\)..." nil t)
      (make-button (match-beginning 1) (match-end 1)
                   'part (marking-current-part)
                   'action (apply-partially 'marking-jump-to-part (concat "\\( *def\\|class\\) *" (match-string 1)))))))

(defun marking-root-directory ()
  (expand-file-name (concat (marking-automark-directory) "/..")))

(defun marking-automark-directory ()
  (marking-parent-directory "/automarker" 'file-directory-p))

(defun marking-good-sample-directory ()
  (marking-parent-directory "/samples/good_sample" 'file-directory-p))

(defun marking-parent-directory (ext check)
  (let ((root default-directory))
    (while (and (not (equal root "/"))
                (not (funcall check
                              (expand-file-name (concat root ext)))))
      (setq root (expand-file-name (concat root "/.."))))
    (if (funcall check (expand-file-name (concat root ext)))
        (expand-file-name (concat root ext))
      (error (concat "Can't find parent directory matching criteria: " ext)))))

(defun marking-current-test ()
  (let* ((word (cdr (assoc (marking-current-part) marking-part-names)))
         (automark-prefix (concat (expand-file-name
                                   (concat (marking-root-directory)
                                           "/automarker/" word "_marking/"))
                                  word)))
    (cond
     ((file-exists-p (concat automark-prefix "_test.py"))
      (concat automark-prefix "_test.py"))
     ((file-exists-p (concat automark-prefix ".py"))
      (concat automark-prefix ".py"))
     (t (error "Couldn't determine test file.")))))

(defun marking-open-good-sample ()
  (interactive)
  (let ((fname (save-excursion
                 (beginning-of-defun)
                 (when (looking-at "def")
                   (buffer-substring
                    (point)
                    (progn (search-forward "(") (point)))))))
    (find-file
     (expand-file-name
      (concat (marking-good-sample-directory) "/"
              (file-name-nondirectory (buffer-file-name)))))
    (when fname
      (goto-char (point-min))
      (search-forward fname)
      (beginning-of-line))))

(defun marking-rerun-part ()
  (interactive)
  (let ((dir (marking-submission-directory-fn))
        (proc-name (concat "*marking " (marking-current-part) "*"))
        (test-file (marking-current-test))
        (venv (marking-python-virtualenv))
        (part (marking-current-part)))
    (switch-to-buffer proc-name)
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (insert (concat "$ cd " (marking-automark-directory) " && "
                    venv " " test-file " " dir "\n\n"))
    (insert (concat "Part " part ":\n"))
    (cd (marking-automark-directory))
    (start-process proc-name proc-name venv test-file dir)
    (cd dir)
    (with-current-buffer proc-name
      (marking-mode)
      (setq-local marking-submission-directory dir))))

(define-derived-mode marking-mode fundamental-mode "Marking"
  "Major mode for editing MARKING.txt

\\{marking-mode-map}"
  :syntax-table nil

  (set (make-local-variable 'font-lock-defaults)
       '(marking-font-lock-keywords))

  (use-local-map marking-mode-map)
  (marking-make-buttons)

  (run-mode-hooks 'marking-mode-hook))

(add-to-list 'auto-mode-alist '("MARKING.txt" . marking-mode))
(provide 'marking-mode)
