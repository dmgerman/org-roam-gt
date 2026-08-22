;;; test-org-roam-gt-citations.el --- Tests for the scanning citation walk  -*- lexical-binding: t; -*-

;;; Commentary:
;; Buttercup tests for the citation handling in org-roam-gt.el.
;;
;; The standard the scan is held to is org's own parse: for each fixture the
;; references it reports must equal, key for key and position for position,
;; the ones `org-element-map' finds in a parse of the whole buffer.  That
;; comparison is the point of most of these tests, so they build the expected
;; value with the upstream walk rather than writing positions down by hand.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-roam-gt)

;;; Fixtures

(defconst org-roam-gt-test-citation-text
  (concat "#+title: citations\n"
          "\n"
          "* One\n"
          ":PROPERTIES:\n"
          ":ID:       cite-test-1\n"
          ":END:\n"
          "Plain [cite:@keyA] and two [cite:@keyB;@keyC] here.\n"
          "\n"
          "A styled [cite/t:@keyD] one.\n"
          "\n"
          "Split over [cite:@keyE;\n"
          "@keyF] lines.\n")
  "An org buffer holding citations of every shape the scan must handle.")

(defconst org-roam-gt-test-citation-noise-text
  (concat "#+title: noise\n"
          "\n"
          "* One\n"
          "#+begin_src text\n"
          "[cite:@notACitation]\n"
          "#+end_src\n"
          "\n"
          ": [cite:@alsoNot]\n"
          "\n"
          "Real [cite:@keyReal] one.\n")
  "An org buffer where citation syntax also appears where org does not read it.")

(defconst org-roam-gt-test-citation-unterminated-text
  (concat "#+title: unterminated\n"
          "\n"
          "* One\n"
          "An opening [cite:@keyG with no closing bracket.\n")
  "An org buffer holding citation syntax that never completes.")

(defun org-roam-gt-test--references-by-parse ()
  "Return (KEY . POSITION) for each citation reference, via a whole-buffer parse."
  (let ((found nil))
    (org-element-map (org-element-parse-buffer) 'citation-reference
      (lambda (reference)
        (push (cons (org-element-property :key reference)
                    (org-element-property :begin reference))
              found)))
    (nreverse found)))

(defun org-roam-gt-test--references-by-scan ()
  "Return (KEY . POSITION) for each citation reference, via the scanning walk."
  (let ((found nil))
    (org-roam-gt-map-citations-scanning
     (lambda (&rest _) (error "Original walk must not be called"))
     nil
     (list (lambda (reference)
             (push (cons (org-element-property :key reference)
                         (org-element-property :begin reference))
                   found))))
    (nreverse found)))

(defmacro org-roam-gt-test-with-citations (text &rest body)
  "Run BODY in an org buffer holding TEXT, with the scanning walk enabled."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (org-mode)
     (let ((org-roam-gt-scan-citations t))
       ,@body)))

;;; Scanning walk

(describe "org-roam-gt-map-citations-scanning"

  (it "reports the same references as a whole-buffer parse"
    (org-roam-gt-test-with-citations org-roam-gt-test-citation-text
      (expect (org-roam-gt-test--references-by-scan)
              :to-equal (org-roam-gt-test--references-by-parse))))

  (it "finds every key, including split and styled citations"
    (org-roam-gt-test-with-citations org-roam-gt-test-citation-text
      (expect (mapcar #'car (org-roam-gt-test--references-by-scan))
              :to-equal '("keyA" "keyB" "keyC" "keyD" "keyE" "keyF"))))

  ;; A reference starts at the separator before its key, which for a citation
  ;; split across lines is the newline, so the key follows the reported
  ;; position rather than sitting exactly on it.
  (it "reports positions from which the key can be read"
    (org-roam-gt-test-with-citations org-roam-gt-test-citation-text
      (dolist (reference (org-roam-gt-test--references-by-scan))
        (let ((key (car reference))
              (begin (cdr reference)))
          (expect (buffer-substring-no-properties
                   begin (min (point-max) (+ begin 2 (length key))))
                  :to-match (concat "@" (regexp-quote key)))))))

  (it "passes over citation syntax that org does not read as a citation"
    (org-roam-gt-test-with-citations org-roam-gt-test-citation-noise-text
      (expect (mapcar #'car (org-roam-gt-test--references-by-scan))
              :to-equal '("keyReal"))
      (expect (org-roam-gt-test--references-by-scan)
              :to-equal (org-roam-gt-test--references-by-parse))))

  ;; A prefix with no closing bracket is not a citation, so the scan takes the
  ;; branch that advances by the search match.  Reading the match data after
  ;; `org-element-context' has run its own searches once sent point backwards
  ;; here, and the loop never ended.
  (it "terminates on citation syntax that never completes"
    (org-roam-gt-test-with-citations org-roam-gt-test-citation-unterminated-text
      (let ((found (with-timeout (10 'timed-out)
                     (org-roam-gt-test--references-by-scan))))
        (expect found :not :to-be 'timed-out)
        (expect found :to-equal (org-roam-gt-test--references-by-parse)))))

  (it "runs each function in FNS over each reference"
    (org-roam-gt-test-with-citations org-roam-gt-test-citation-text
      (let ((first nil)
            (second nil)
            (keys '("keyA" "keyB" "keyC" "keyD" "keyE" "keyF")))
        (org-roam-gt-map-citations-scanning
         #'ignore nil
         (list (lambda (r) (push (org-element-property :key r) first))
               (lambda (r) (push (org-element-property :key r) second))))
        (expect (nreverse first) :to-equal keys)
        (expect (nreverse second) :to-equal keys))))

  (it "leaves point where it found it"
    (org-roam-gt-test-with-citations org-roam-gt-test-citation-text
      (goto-char (point-max))
      (let ((start (point)))
        (org-roam-gt-map-citations-scanning #'ignore nil (list #'ignore))
        (expect (point) :to-equal start))))

  (it "calls the original walk when scanning is disabled"
    (org-roam-gt-test-with-citations org-roam-gt-test-citation-text
      (let ((org-roam-gt-scan-citations nil)
            (seen nil)
            (fns (list #'ignore)))
        (org-roam-gt-map-citations-scanning
         (lambda (info fns) (setq seen (list info fns)))
         'the-info fns)
        (expect seen :to-equal (list 'the-info fns))))))

;;; Skipping the parse

(describe "org-roam-gt-element-parse-buffer-skipped"

  (it "parses as usual outside indexing"
    (org-roam-gt-test-with-citations org-roam-gt-test-citation-text
      (let ((org-roam-gt--suppress-element-parse nil))
        (expect (org-roam-gt-element-parse-buffer-skipped
                 #'org-element-parse-buffer)
                :to-equal (org-element-parse-buffer)))))

  (it "returns an empty document while indexing"
    (org-roam-gt-test-with-citations org-roam-gt-test-citation-text
      (let ((org-roam-gt--suppress-element-parse t))
        (expect (org-roam-gt-element-parse-buffer-skipped
                 (lambda (&rest _) (error "Buffer must not be parsed")))
                :to-equal (list 'org-data nil)))))

  (it "returns something org-element-map accepts"
    (let ((org-roam-gt--suppress-element-parse t))
      (expect (org-element-map
                  (org-roam-gt-element-parse-buffer-skipped #'ignore)
                  'citation-reference #'identity)
              :to-be nil))))

(describe "org-roam-gt-update-file-skipping-parse"

  (it "suppresses the parse for the duration of the call"
    (let ((org-roam-gt-scan-citations t)
          (during 'unset))
      (org-roam-gt-update-file-skipping-parse
       (lambda (&rest _) (setq during org-roam-gt--suppress-element-parse)))
      (expect during :to-be t)
      (expect org-roam-gt--suppress-element-parse :to-be nil)))

  (it "leaves the parse alone when scanning is disabled"
    (let ((org-roam-gt-scan-citations nil)
          (during 'unset))
      (org-roam-gt-update-file-skipping-parse
       (lambda (&rest _) (setq during org-roam-gt--suppress-element-parse)))
      (expect during :to-be nil)))

  (it "passes its arguments through and returns what the original returns"
    (let ((org-roam-gt-scan-citations t))
      (expect (org-roam-gt-update-file-skipping-parse
               (lambda (&rest args) (cons 'called args))
               "/tmp/one.org" 'no-require)
              :to-equal '(called "/tmp/one.org" no-require))))

  (it "restores the flag when the original signals"
    (let ((org-roam-gt-scan-citations t))
      (expect (org-roam-gt-update-file-skipping-parse
               (lambda (&rest _) (error "Indexing failed")))
              :to-throw 'error)
      (expect org-roam-gt--suppress-element-parse :to-be nil))))

;;; The two together

(describe "citation scanning with the parse skipped"

  ;; The reason the two advices are installed as one unit: with the parse
  ;; suppressed, the original walk would be handed an empty document and report
  ;; nothing, so the scan has to be what runs.
  (it "still finds every citation while a file is being indexed"
    (org-roam-gt-test-with-citations org-roam-gt-test-citation-text
      (let ((found nil))
        (org-roam-gt-update-file-skipping-parse
         (lambda (&rest _)
           (org-roam-gt-map-citations-scanning
            #'ignore
            (org-roam-gt-element-parse-buffer-skipped #'org-element-parse-buffer)
            (list (lambda (r) (push (org-element-property :key r) found))))))
        (expect (nreverse found)
                :to-equal '("keyA" "keyB" "keyC" "keyD" "keyE" "keyF"))))))

;;; Installation

(describe "org-roam-gt-citations--enable"

  (after-each
    (org-roam-gt-citations--disable))

  (it "installs all three advices"
    (org-roam-gt-citations--enable)
    (expect (advice-member-p #'org-roam-gt-map-citations-scanning
                             'org-roam-db-map-citations)
            :to-be-truthy)
    (expect (advice-member-p #'org-roam-gt-element-parse-buffer-skipped
                             'org-element-parse-buffer)
            :to-be-truthy)
    (expect (advice-member-p #'org-roam-gt-update-file-skipping-parse
                             'org-roam-db-update-file)
            :to-be-truthy))

  (it "removes all three on disable"
    (org-roam-gt-citations--enable)
    (org-roam-gt-citations--disable)
    (expect (advice-member-p #'org-roam-gt-map-citations-scanning
                             'org-roam-db-map-citations)
            :to-be nil)
    (expect (advice-member-p #'org-roam-gt-element-parse-buffer-skipped
                             'org-element-parse-buffer)
            :to-be nil)
    (expect (advice-member-p #'org-roam-gt-update-file-skipping-parse
                             'org-roam-db-update-file)
            :to-be nil)))

(provide 'test-org-roam-gt-citations)

;;; test-org-roam-gt-citations.el ends here
