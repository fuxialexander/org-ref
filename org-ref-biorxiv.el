;;; org-ref-biorxiv.el --- Library to download reference from biorxiv -*- lexical-binding: t; -*-

;; * Code
;;
(require 'org-ref-core)
(require 'doi-utils)
(require 'url-handlers)
;;;###autoload
(defun org-ref-biorxiv-update-bibtex (url)
  (interactive (list
                (or (bibtex-autokey-get-field
                     "url")
                    (read-string "URL: "))))
  nil
  (bibtex-narrow-to-entry)
  (bibtex-beginning-of-entry)
  (let ((oldkey (bibtex-completion-key-at-point)))
    (call-interactively
     'mark-whole-buffer)
    (call-interactively
     'delete-region)
    (org-ref-biorxiv--get-bibtex
     url)
    (bibtex-beginning-of-entry)
    (re-search-forward
     bibtex-entry-maybe-empty-head)
    (if (match-beginning
         bibtex-key-in-head)
        (delete-region
         (match-beginning
          bibtex-key-in-head)
         (match-end bibtex-key-in-head)))
    (insert oldkey)
    (widen))
  (org-ref-clean-bibtex-entry))
;;;###autoload
(defun org-ref-biorxiv-add (url &optional bibfile)
  (interactive (list
                (or (ignore-errors
                      (bibtex-autokey-get-field
                       "url"))
                    (read-string "URL: "))))
  ;; nil
  (unless bibfile
    (setq bibfile
          (completing-read
           "Bibfile: "
           (-uniq
            (append
             ;; see if we should add it to a bib-file defined in the file
             (org-ref-find-bibliography)
             ;; or any bib-files that exist in the current directory
             (f-entries
              "."
              (lambda (f)
                (and (not (string-match "#" f))
                     (f-ext? f "bib"))))
             ;; and last in the default bibliography
             org-ref-default-bibliography)))))
  (save-window-excursion
    (with-current-buffer
        (find-file-noselect bibfile)
      ;; Check if the url already exists
      (goto-char (point-min))
      (if (word-search-forward
           (concat url)
           nil
           t)
          (message
           "%s is already in this file"
           url)
        (goto-char (point-max))
        ;; make sure we are at the beginning of a line
        (when (not (= (point)
                      (line-beginning-position)))
          (forward-char 1))
        (when (not (looking-back "\n\n" 3))
          (insert "\n\n"))
        (when (not (looking-back
                    "\n\n"
                    (min 3 (point))))
          (insert "\n\n"))
        (org-ref-biorxiv-insert-entry
         url)
        (save-buffer)))))

;;;###autoload
(defun org-ref-biorxiv-insert-entry (url)
  "Insert bibtex entry from a DOI.
Also cleans entry using ‘org-ref’, and tries to download the corresponding pdf."
  (org-ref-biorxiv--get-bibtex
   url)
  (backward-char)
  ;; set date added for the record
  (bibtex-set-field
     "journaltitle"
     (bibtex-autokey-get-field "journal"))
  (when doi-utils-timestamp-format-function
    (bibtex-set-field
     doi-utils-timestamp-field
     (funcall
      doi-utils-timestamp-format-function)))
  ;; Clean the record
  (org-ref-clean-bibtex-entry)
  ;; try to get pdf
  (when doi-utils-download-pdf
    (org-ref-biorxiv-get-pdf))
  ;; Make notes
  (when (and doi-utils-make-notes
             org-ref-bibliography-notes)
    (save-excursion
      (when (f-file?
             org-ref-bibliography-notes)
        (find-file-noselect
         org-ref-bibliography-notes)
        (save-buffer))
      (let ((bibtex-files (list (buffer-file-name))))
        (funcall
         doi-utils-make-notes-function)))))

;;;###autoload
;; <a href="/highwire/citation/73994/bibtext"
(defun org-ref-biorxiv--get-bibtex (url)
  (with-eval-after-load 'url
      (setq url-proxy-services nil))
  (setq *doi-utils-waiting* t)
  (url-retrieve
   url
   (lambda (cbargs)
     (goto-char (point-min))
     (re-search-forward
      "<a href=\"\\(/highwire/citation/.*/bibtext\\)\""
      nil
      t)
     (setq *doi-utils-biorxiv-bibtex-url*
           (concat
            "https://www.biorxiv.org"
            (match-string 1))
           *doi-utils-waiting*
           nil)))
  (while *doi-utils-waiting*
    (sleep-for 0.1))
  (goto-char (point-max))
  (url-insert
   (url-retrieve-synchronously
    *doi-utils-biorxiv-bibtex-url*)))

(defun biorxiv-pdf-url (*doi-utils-redirect*)
  "Get url to the pdf from *DOI-UTILS-REDIRECT*."
  (when (string-match
         "www.biorxiv.org"
         *doi-utils-redirect*)
    (replace-regexp-in-string
     "early"
     "biorxiv/early"
     (concat
      *doi-utils-redirect*
      ".full.pdf"))))

(defun org-ref-biorxiv-get-pdf ()
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((pdf (bibtex-autokey-get-field
               "eprint"))
         (key (cdr (assoc "=key="
                          (bibtex-parse-entry))))
         (pdf-file (concat
                    (if org-ref-pdf-directory
                        (file-name-as-directory
                         org-ref-pdf-directory)
                      (read-directory-name
                       "PDF directory: "
                       "."))
                    key
                    ".pdf")))
    (unless (file-exists-p pdf-file)
      (url-copy-file pdf pdf-file))))

;;;###autoload
(defun org-ref-biorxiv-add-async (url &optional bibfile)
  (interactive (list
                (or (ignore-errors
                      (bibtex-autokey-get-field
                       "url"))
                    (read-string "URL: "))))
  (message "Downloading from %s" url)
  (setq *org-ref-add-ref-async--url
        url
        *org-ref-add-ref-async--bibfile
        (completing-read
         "Bibfile: "
         (-uniq
          (append
           ;; see if we should add it to a bib-file defined in the file
           (org-ref-find-bibliography)
           ;; or any bib-files that exist in the current directory
           (f-entries
            "."
            (lambda (f)
              (and (not (string-match "#" f))
                   (f-ext? f "bib"))))
           ;; and last in the default bibliography
           org-ref-default-bibliography))))
  (let ((doi-utils-make-notes nil))
    (require 'async)
    (async-start
     ;; What to do in the child process
     `(lambda
        ()
        (setq load-path ',load-path)
        (let ((noninteractive))
          (require 'org-ref-biorxiv))
        ,(async-inject-variables
          "\\`\\(org-ref\\)-")
        ,(async-inject-variables
          "\\`\\(doi-utils\\)-")
        ,(async-inject-variables
          "\\`\\(\\*org-ref-add-ref-async\\)-.*")
        (org-ref-biorxiv-add
         *org-ref-add-ref-async--url
         *org-ref-add-ref-async--bibfile)
        *org-ref-add-ref-async--url)
     ;; What to do when it finishes
     (lambda (result)
       (message
        "Successfully added %s "
        result)))))

(provide 'org-ref-biorxiv)
;;; org-ref-biorxiv.el ends here
