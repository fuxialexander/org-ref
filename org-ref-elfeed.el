;;; org-ref-elfeed.el --- Library to integrate Elfeed -*- lexical-binding: t; -*-

;; * Code
(require 'elfeed)
(require 'doi-utils)
(require 'org-ref-biorxiv)
(require 'org-ref-arxiv)
;;;###autoload
(defun org-ref-elfeed-add ()
  "Add elfeed entry to bibtex."
  (interactive)
  (let ((url (elfeed-entry-link
              elfeed-show-entry)))
    (if (string-match ".*biorxiv" url)
        (org-ref-biorxiv-add-async
         url)
      (org-ref-elfeed-add-async--doi))))
;;;###autoload
(defun org-ref-elfeed-add-async--doi ()
  "Add elfeed entry to bibtex."
  (interactive)
  (let* (;;(title (elfeed-entry-title elfeed-show-entry))
         (url (elfeed-entry-link
               elfeed-show-entry))
         (content (elfeed-deref
                   (elfeed-entry-content
                    elfeed-show-entry)))
         (entry-id (elfeed-entry-id
                    elfeed-show-entry)))
    (if (string-match
         "DOI: \\(.*\\)$"
         content)
        (org-ref-doi-add-async
         (match-string 1 content))
      (let ((dois (org-ref-url-scrape-dois url)))
        (cond ;; One doi found. Assume it is what we want.
         ((= 1 (length dois))
          (org-ref-doi-add-async
           (car dois)))
         ;; Multiple DOIs found
         ((> (length dois) 1)
          (ivy-read
           "Select a DOI"
           `(candidates . ,(let ((dois '()))
                             (with-current-buffer
                                 (url-retrieve-synchronously
                                  url)
                               (cl-loop
                                for
                                doi-pattern
                                in
                                org-ref-doi-regexps
                                do
                                (goto-char (point-min))
                                (while (re-search-forward
                                        doi-pattern
                                        nil
                                        t)
                                  (cl-pushnew
                                   ;; Cut off the doi, sometimes
                                   ;; false matches are long.
                                   (cons (format
                                          "%40s"
                                          (substring
                                           (match-string 1)
                                           0
                                           (min
                                            (length (match-string 1))
                                            40))
                                          ;; doi-pattern
                                          )
                                         (match-string 1))
                                   dois
                                   :test #'equal)))
                               (reverse dois))))
           :action 'org-ref-doi-add-async)
          ;; (bibtex-beginning-of-entry)
          ;; (delete-char -2)
          ))))))

(provide 'org-ref-elfeed)
;;; org-ref-elfeed.el ends here
