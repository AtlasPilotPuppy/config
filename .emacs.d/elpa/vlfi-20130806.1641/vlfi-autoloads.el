;;; vlfi-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (dired-vlfi vlfi) "vlfi" "vlfi.el" (20997 6568))
;;; Generated autoloads from vlfi.el

(autoload 'vlfi "vlfi" "\
View Large FILE.
Batches of the file data from FILE will be displayed in a read-only
buffer.  You can customize number of bytes displayed by customizing
`vlfi-batch-size'.

\(fn FILE)" t nil)

(autoload 'dired-vlfi "vlfi" "\
In Dired, visit the file on this line in VLFI mode.

\(fn)" t nil)

(eval-after-load "dired" '(define-key dired-mode-map "V" 'dired-vlfi))

(defadvice abort-if-file-too-large (around vlfi-if-file-too-large (size op-type &optional filename) compile activate) "\
If file SIZE larger than `large-file-warning-threshold', allow user to view file with `vlfi', open it normally, or abort.
OP-TYPE specifies the file operation being performed over FILENAME." (and large-file-warning-threshold size (> size large-file-warning-threshold) (let ((char nil)) (while (not (memq (setq char (read-event (propertize (format "File %s is large (%s): %s normally (o), %s with vlfi (v) or abort (a)" (if filename (file-name-nondirectory filename) "") (file-size-human-readable size) op-type op-type) (quote face) (quote minibuffer-prompt)))) (quote (111 79 118 86 97 65))))) (cond ((memq char (quote (111 79)))) ((memq char (quote (118 86))) (vlfi filename) (error "")) ((memq char (quote (97 65))) (error "Aborted"))))))

;;;***

;;;### (autoloads nil nil ("vlfi-pkg.el") (20997 6568 353024))

;;;***

(provide 'vlfi-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vlfi-autoloads.el ends here
