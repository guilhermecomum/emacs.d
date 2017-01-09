;;; flycheck-stylelint.el --- Integrate stylelint with flycheck

;;; Commentary:

;; Taken from https://github.com/kouhin/dotfiles/blob/master/emacs.d/lisp/flycheck-stylelint.el
;; This package is from https://github.com/flycheck/flycheck/pull/903
;; Thanks to Philipp Baschke https://github.com/PhilippBaschke

;; To use it, add to your init.el

;;; Code:

(require 'flycheck)

(defconst flycheck-stylelint-args '("--formatter" "json" "--syntax=scss")
  "Common arguments to stylelint invocations.")

(flycheck-def-config-file-var flycheck-stylelintrc
    (css-stylelint scss-stylelint less-stylelint) nil
  :safe #'stringp)

(flycheck-def-option-var flycheck-stylelint-quiet
    nil (css-stylelint scss-stylelint less-stylelint)
  "Whether to run stylelint in quiet mode.
When non-nil, enable quiet mode, via `--quiet'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(flycheck . 26))

(defconst flycheck-stylelint-error-re
  (flycheck-rx-to-string
   '(: line-start (id (one-or-more word)) ": " (message) line-end)))

(defun flycheck-parse-stylelint (output checker buffer)
  "Parse stylelint errors from OUTPUT.
CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.
The CHECKER usually returns the errors as JSON.  If the CHECKER throws
an Error it returns an Error message with a stacktrace."
  (condition-case nil
      (flycheck-parse-stylelint-json output checker buffer)

    ;; The output could not be parsed as JSON
    (json-readtable-error

     ;; Extract a flycheck error from the output (with a regular expression)
     ;; For match-string 4/5 see flycheck-rx-message/flycheck-rx-id
     (when (string-match flycheck-stylelint-error-re output)
       (list (flycheck-error-new-at
              1 nil 'error
              (match-string 4 output)
              :id (match-string 5 output)
              :checker checker
              :buffer buffer
              :filename (buffer-file-name buffer)))))))

(defun flycheck-parse-stylelint-json (output checker buffer)
  "Parse stylelint JSON errors from OUTPUT.
CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
the BUFFER that was checked respectively.
See URL `http://stylelint.io/developer-guide/formatters/' for information
about the JSON format of stylelint.
See URL `http://edward.oconnor.cx/2006/03/json.el' for information
about json.el."
  (let ((json-object-type 'plist))

    ;; stylelint returns a vector of result objects
    ;; Since we only passed one file, the first element is enough
    (let* ((stylelint-output (elt (json-read-from-string output) 0))
           (filename (buffer-file-name buffer))

           ;; Turn all deprecations into warnings
           (deprecations
            (mapcar (lambda (d)
                      (flycheck-error-new-at
                       1 nil 'warning
                       (plist-get d :text)
                       :id "Deprecation Warning"
                       :checker checker
                       :buffer buffer
                       :filename filename))
                    (plist-get stylelint-output :deprecations)))

           ;; Turn all invalid options into errors
           (invalid-options
            (mapcar (lambda (io)
                      (flycheck-error-new-at
                       1 nil 'error
                       (plist-get io :text)
                       :id "Invalid Option"
                       :checker checker
                       :buffer buffer
                       :filename filename))
                    (plist-get stylelint-output :invalidOptionWarnings)))

           ;; Read all linting warnings
           (warnings
            (mapcar (lambda (w)
                      (flycheck-error-new-at
                       (plist-get w :line) (plist-get w :column)
                       (pcase (plist-get w :severity)
                         (`"error"   'error)
                         (`"warning" 'warning)
                         ;; Default to info for unknown .severity
                         (_          'info))
                       (plist-get w :text)
                       :id (plist-get w :rule)
                       :checker checker
                       :buffer buffer
                       :filename filename))
                    (plist-get stylelint-output :warnings))))

      ;; Return the combined errors (deprecations, invalid options, warnings)
      (append deprecations invalid-options warnings))))

(flycheck-define-checker css-stylelint
  "A CSS syntax and style checker using stylelint.
See URL `http://stylelint.io/'."
  :command ("stylelint"
            (eval flycheck-stylelint-args)
            (option-flag "--quiet" flycheck-stylelint-quiet)
            (config-file "--config" flycheck-stylelintrc))
  :standard-input t
  :error-parser flycheck-parse-stylelint
  :modes (scss-mode))

(add-to-list 'flycheck-checkers 'css-stylelint t)

(provide 'flycheck-stylelint)

;;; flycheck-stylelint.el ends here
