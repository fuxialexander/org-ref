;;; doi-utils-org.el --- doi interacts org -*- lexical-binding: t; -*-

;; * Code
(require 'doi-utils)
(require 'org-ref-utils)
(require 'org)                          ; org-add-link-type
(require 'org-bibtex)                   ; org-bibtex-yank
;;;###autoload
(defun doi-utils-doi-to-org-bibtex (doi)
  "Convert a DOI to an ‘org-bibtex’ form and insert it at point."
  (interactive "sDOI: ")
  (with-temp-buffer
    (insert (doi-utils-doi-to-bibtex-string doi))
    (bibtex-clean-entry)
    (kill-region (point-min) (point-max)))
  (org-bibtex-yank)
  (org-metaright)
  (org-metaright))


;;* A new doi link for org-mode
;; The idea is to add a menu to the doi link, so rather than just clicking to open the article, you can do other things.
;; 1. open doi
;; 2. open in wos
;; 3. open citing articles
;; 4. open related articles
;; 5. open bibtex entry
;; 6. get bibtex entry


;;;###autoload
(defun doi-utils-open (doi)
  "Open DOI in browser."
  (interactive "sDOI: ")
  (browse-url (concat doi-utils-dx-doi-org-url doi)))


;;;###autoload
(defun doi-utils-open-bibtex (doi)
  "Search through variable `reftex-default-bibliography' for DOI."
  (interactive "sDOI: ")
  (catch 'file
    (dolist (f reftex-default-bibliography)
      (find-file f)
      (when (search-forward doi (point-max) t)
        (bibtex-beginning-of-entry)
        (throw 'file t)))))


;;;###autoload
(defun doi-utils-crossref (doi)
  "Search DOI in CrossRef."
  (interactive "sDOI: ")
  (browse-url
   (format
    "http://search.crossref.org/?q=%s" doi)))


;;;###autoload
(defun doi-utils-google-scholar (doi)
  "Google scholar the DOI."
  (interactive "sDOI: ")
  (browse-url
   (format
    "http://scholar.google.com/scholar?q=%s" doi)))


;;;###autoload
(defun doi-utils-pubmed (doi)
  "Search Pubmed for the DOI."
  (interactive "sDOI: ")
  (browse-url
   (format
    "http://www.ncbi.nlm.nih.gov/pubmed/?term=%s"
    (url-hexify-string doi))))


(defvar doi-link-menu-funcs '()
  "Functions to run in doi menu.
Each entry is a list of (key menu-name function).  The function
must take one argument, the doi.")

(setq doi-link-menu-funcs
      '(("o" "pen" doi-utils-open)
        ("w" "os" doi-utils-wos)
        ("c" "iting articles" doi-utils-wos-citing)
        ("r" "elated articles" doi-utils-wos-related)
        ("s" "Google Scholar" doi-utils-google-scholar)
        ("f" "CrossRef" doi-utils-crossref)
        ("p" "ubmed" doi-utils-pubmed)
        ("b" "open in bibtex" doi-utils-open-bibtex)
        ("g" "et bibtex entry" doi-utils-add-bibtex-entry-from-doi)))


;;;###autoload
(defun doi-link-menu (link-string)
  "Generate the link menu message, get choice and execute it.
Options are stored in `doi-link-menu-funcs'.
Argument LINK-STRING Passed in on link click."
  (interactive)
  (message
   (concat
    (mapconcat
     (lambda (tup)
       (concat "[" (elt tup 0) "]"
               (elt tup 1) " "))
     doi-link-menu-funcs "") ": "))
  (let* ((input (read-char-exclusive))
         (choice (assoc
                  (char-to-string input) doi-link-menu-funcs)))
    (when choice
      (funcall
       (elt
        choice
        2)
       link-string))))

(org-ref-link-set-parameters "doi"
  :follow #'doi-link-menu
  :export (lambda (doi desc format)
            (cond
             ((eq format 'html)
              (format "<a href=\"%s%s\">%s</a>"
                      doi-utils-dx-doi-org-url
                      doi
                      (or desc (concat "doi:" doi))))
             ((eq format 'latex)
              (format "\\href{%s%s}{%s}"
                      doi-utils-dx-doi-org-url
                      doi
                      (or desc (concat "doi:" doi)))))))



;;* DOI functions for WOS

;; I came across this API http://wokinfo.com/media/pdf/OpenURL-guide.pdf to make
;; links to the things I am interested in here. Based on that document, here are
;; three links based on a doi:10.1021/jp047349j that take you to different Web
;; Of Science (WOS) pages.


;; 1. go to article in WOS: http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info:doi/10.1021/jp047349j
;; 2. citing articles: http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F10.1021/jp047349j&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.citing=yes
;; 3. related articles: http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F10.1021/jp047349j&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.related=yes

;; These are pretty easy to construct, so we can write functions that will
;; create them and open the url in our browser. There are some other options
;; that could be considered, but since we usually have a doi, it seems like the
;; best way to go for creating the links. Here are the functions.

;;;###autoload
(defun doi-utils-wos (doi)
  "Open Web of Science entry for DOI."
  (interactive "sDOI: ")
  (browse-url
   (format
    "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info:doi/%s" doi)))

;;;###autoload
(defun doi-utils-wos-citing (doi)
  "Open Web of Science citing articles entry for DOI.
May be empty if none are found."
  (interactive "sDOI: ")
  (browse-url
   (concat
    "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F"
    doi
    "&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.citing=yes")))

;;;###autoload
(defun doi-utils-wos-related (doi)
  "Open Web of Science related articles page for DOI."
  (interactive "sDOI: ")
  (browse-url
   (concat "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F"
           doi
           "&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.related=yes")))

;;* Getting a doi for a bibtex entry missing one

;; Some bibtex entries do not have a DOI, maybe because they were entered by
;; hand, or copied from a source that did not have it available. Here we develop
;; some functions to help you find the DOI using Crossref.

;; Here is our example bibtex entry.
;; #+BEGIN_SRC bibtex
;; @article{deml-2014-oxide,
;;   author =	 {Ann M. Deml and Vladan Stevanovi{\'c} and
;;                   Christopher L. Muhich and Charles B. Musgrave and
;;                   Ryan O'Hayre},
;;   title =	 {Oxide Enthalpy of Formation and Band Gap Energy As
;;                   Accurate Descriptors of Oxygen Vacancy Formation
;;                   Energetics},
;;   journal =	 {Energy Environ. Sci.},
;;   volume =	 7,
;;   number =	 6,
;;   pages =	 1996,
;;   year =	 2014,
;;   doi =		 {10.1039/c3ee43874k,
;;   url =		 {http://dx.doi.org/10.1039/c3ee43874k}},

;; }


;; The idea is to query Crossref in a way that is likely to give us a hit
;; relevant to the entry.

;; According to http://search.crossref.org/help/api we can send a query with a
;; free form citation that may give us something back. We do this to get a list
;; of candidates, and run a helm command to get the doi.


;;;###autoload
(defun doi-utils-crossref-citation-query ()
  "Query Crossref with the title of the bibtex entry at point.
Get a list of possible matches.  This opens a helm buffer to
select an entry.  The default action inserts a doi and url field
in the bibtex entry at point.  The second action opens the doi
url.  If there is already a doi field, the function raises an
error."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
	 (raw-json-string)
         (json-string)
         (json-data)
         (doi))
    (unless (string= ""(reftex-get-bib-field "doi" entry))
      (error "Entry already has a doi field"))

    (with-current-buffer
        (url-retrieve-synchronously
         (concat
          "http://search.crossref.org/dois?q="
          (url-hexify-string (org-ref-bib-citation))))
      (save-excursion
      	(goto-char (point-min))
      	(while (re-search-forward "<i>\\|</i>" nil t)
      	  (replace-match ""))
	(goto-char (point-min))
	(while (re-search-forward "&amp;" nil t)
	  (replace-match "&"))
      	(goto-char (point-min))
      	(while (re-search-forward "&quot;" nil t)
      	  (replace-match "\\\"" nil t)))
      (setq raw-json-string (buffer-substring url-http-end-of-headers (point-max)))
      ;; decode json string
      (setq json-string (decode-coding-string (string-make-unibyte raw-json-string) 'utf-8))
      (setq json-data (json-read-from-string json-string)))

    (let* ((name (format "Crossref hits for %s" (org-ref-bib-citation)))
           (helm-candidates (mapcar (lambda (x)
                                      (cons
                                       (concat
                                        (cdr (assoc 'fullCitation x)))
                                       (cdr (assoc 'doi x))))
                                    json-data))

           (source `((name . ,name)
                     (candidates . ,helm-candidates)
                     ;; just return the candidate
                     (action . (("Insert doi and url field" . (lambda (doi)
                                                                (bibtex-make-field "doi" t)
                                                                (backward-char)
                                                                ;; crossref returns doi url, but I prefer only a doi for the doi field
                                                                (insert (replace-regexp-in-string "^https?://\\(dx.\\)?doi.org/" "" doi))
                                                                (when (string= ""(reftex-get-bib-field "url" entry))
                                                                  (bibtex-make-field "url" t)
                                                                  (backward-char)
                                                                  (insert doi))))
                                ("Open url" . (lambda (doi)
                                                (browse-url doi))))))))
      (helm :sources source
	    :buffer "*doi utils*"))))



;;* Debugging a DOI

;; I wrote this function to help debug a DOI. This function generates an
;; org-buffer with the doi, gets the json metadata, shows the bibtex entry, and
;; the pdf link for it.
(defun doi-utils-get-json (doi)
  "Return json data as a string for DOI."
  (let ((url-request-method "GET")
        (url-mime-accept-string "application/citeproc+json")
        (json-data))
    (with-current-buffer
        (url-retrieve-synchronously
         (concat doi-utils-dx-doi-org-url doi))
      (setq json-data (buffer-substring url-http-end-of-headers (point-max)))
      (if (string-match "Resource not found" json-data)
          (progn
            (browse-url (concat doi-utils-dx-doi-org-url doi))
            (error "Resource not found.  Opening website"))
	json-data))))


;;;###autoload
(defun doi-utils-debug (doi)
  "Generate an org-buffer showing data about DOI."
  (interactive "sDOI: ")
  (switch-to-buffer "*debug-doi*")
  (erase-buffer)
  (org-mode)
  (insert (concat "doi:" doi) "\n\n")
  (insert "* JSON
"
	  (let ((url-request-method "GET")
		(url-mime-accept-string "application/citeproc+json"))
	    (pp
	     (json-read-from-string (with-current-buffer
					(url-retrieve-synchronously
					 (concat doi-utils-dx-doi-org-url doi))
				      (buffer-substring url-http-end-of-headers (point-max))))))
	  "\n\n")
  (goto-char (point-min)))

;;* Adding a bibtex entry from a crossref query

;; The idea here is to perform a query on Crossref, get a helm buffer of
;; candidates, and select the entry(ies) you want to add to your bibtex file.
;; You can select a region, e.g. a free form citation, or set of words, or you
;; can type the query in by hand.

;;;###autoload
(defun doi-utils-add-entry-from-crossref-query (query bibtex-file)
  "Search Crossref with QUERY and use helm to select an entry to add to BIBTEX-FILE."
  (interactive (list
                (read-string
                 "Query: "
                 ;; now set initial input
                 (cond
                  ;; If region is active assume we want it
                  ((region-active-p)
                   (replace-regexp-in-string
                    "\n" " "
                    (buffer-substring (region-beginning) (region-end))))
                  ;; type or paste it in
                  (t
                   nil)))
                (completing-read
                 "Bibfile: "
                 (append (f-entries "." (lambda (f) (f-ext? f "bib")))
                         org-ref-default-bibliography))))
  (let* ((raw-json-string)
	 (json-string)
	 (json-data)
	 (doi))

    (with-current-buffer
	(url-retrieve-synchronously
	 (concat
	  "http://search.crossref.org/dois?q="
	  (url-hexify-string query)))
      ;; replace html entities
      (save-excursion
      	(goto-char (point-min))
      	(while (re-search-forward "<i>\\|</i>" nil t)
      	  (replace-match ""))
	(goto-char (point-min))
	(while (re-search-forward "&amp;" nil t)
	  (replace-match "&"))
      	(goto-char (point-min))
      	(while (re-search-forward "&quot;" nil t)
      	  (replace-match "\\\"" nil t)))
      (setq raw-json-string (buffer-substring url-http-end-of-headers (point-max)))
      ;; decode json string
      (setq json-string (decode-coding-string (string-make-unibyte raw-json-string) 'utf-8))
      (setq json-data (json-read-from-string json-string)))

    (let* ((name (format "Crossref hits for %s"
			 ;; remove carriage returns. they cause problems in helm.
			 (replace-regexp-in-string "\n" " " query)))
	   (helm-candidates (mapcar (lambda (x)
				      (cons
				       (concat
					(cdr (assoc 'fullCitation x)))
				       (cdr (assoc 'doi x))))
				    json-data))
	   (source `((name . ,name)
		     (candidates . ,helm-candidates)
		     ;; just return the candidate
		     (action . (("Insert bibtex entry" .  (lambda (doi)
							    (with-current-buffer (find-file-noselect bibtex-file)
							      (cl-loop for doi in (helm-marked-candidates)
								       do
								       (doi-utils-add-bibtex-entry-from-doi
									(replace-regexp-in-string
									 "^https?://\\(dx.\\)?doi.org/" "" doi)
									,bibtex-file)))))
				("Open url" . (lambda (doi)
						(browse-url doi))))))))
      (helm :sources source
	    :buffer "*doi utils*"))))

(defalias 'crossref-add-bibtex-entry 'doi-utils-add-entry-from-crossref-query
  "Alias function for convenience.")

(provide 'doi-utils-org)
;;; doi-utils-org.el ends here
