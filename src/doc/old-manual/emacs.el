(require 'cl)

(defvar ecl-search-string "")

(defun query-replace-ecl-doc  (from-string to-string &optional delimited start end)
  (interactive (query-replace-read-args "Query replace" nil))
  (let ((remaining (member (buffer-file-name (current-buffer)) ecl-doc-files)))
    (dolist (i (or remaining ecl-doc-files))
      (let ((b (find-buffer-visiting i)))
	(unless (equal b (current-buffer))
	  (switch-to-buffer b)
	  (beginning-of-buffer)))
      (perform-replace from-string to-string t nil delimited nil nil
		       start end))))

(defun query-replace-regexp-ecl-doc  (from-string to-string &optional delimited start end)
  (interactive (query-replace-read-args "Query replace" nil))
  (let ((remaining (member (buffer-file-name (current-buffer)) ecl-doc-files)))
    (dolist (i (or remaining ecl-doc-files))
      (let ((b (find-buffer-visiting i)))
	(unless (equal b (current-buffer))
	  (switch-to-buffer b)
	  (beginning-of-buffer)))
      (query-replace-regexp from-string to-string delimited start end))))

(defun search-ecl-doc  (string)
  (interactive "sString: ")
  (setq ecl-search-string string)
  (let ((remaining (member (buffer-file-name (current-buffer)) ecl-doc-files)))
    (dolist (i (or remaining ecl-doc-files))
      (let ((b (find-buffer-visiting i)))
	(unless (equal b (current-buffer))
	  (print b)
	  (switch-to-buffer b)
	  (beginning-of-buffer)))
      (print '*)
      (setq case-fold-search t)
      (if (search-forward string nil t)
	  (return)))))

(defun search-next-ecl-doc  ()
  (interactive)
  (search-ecl-doc  ecl-search-string))

(defun back-to-emacs ()
  (interactive)
  (switch-to-buffer "emacs.el"))

(defun next-ecl-doc  ()
  (interactive)
  (let ((remaining (member (buffer-file-name (current-buffer)) ecl-doc-files)))
    (when (cdr remaining)
      (switch-to-buffer (find-buffer-visiting (cadr remaining))))))

(global-set-key [?\M-p ?\C-i] 'back-to-emacs)
(global-set-key [?\M-p ?\C-s] 'search-ecl-doc )
(global-set-key [?\M-p ?\C-n] 'search-next-ecl-doc )
(global-set-key [?\M-p ?\C-m] 'next-ecl-doc )
(global-set-key [?\M-p ?\C-p] 'ecl-load-symbols)

(setq auto-mode-alist (acons "\\.d\\'" 'c-mode auto-mode-alist))

(setq ecl-doc-files
      (mapcar (lambda (x)
		;(set-buffer "emacs.el")
		(concat (subseq (buffer-file-name (current-buffer)) 0 -8) x))
	      '(
"asdf.xmlf"
"bibliography.xmlf"
"clos.xmlf"
"compiler.xmlf"
"copyright.xmlf"
"declarations.xmlf"
"discarded.xml"
"discarded.xmlf"
"ecl.xml"
"ecldev.xmlf"
"embed.xmlf"
"ffi.xmlf"
"gc.xmlf"
"internals.xmlf"
"interpreter.xmlf"
"intro.xmlf"
"io.xmlf"
"macros.xmlf"
"memory.xmlf"
"mop.xmlf"
"mp.xmlf"
"os.xmlf"
"pde.xmlf"
"preface.xmlf"
"ref_c_arrays.xml"
"ref_c_characters.xml"
"ref_c_conditions.xml"
"ref_c_conses.xml"
"ref_c_data_flow.xml"
"ref_c_environment.xml"
"ref_c_evaluation.xml"
"ref_c_filenames.xml"
"ref_c_files.xml"
"ref_c_hash_tables.xml"
"ref_c_numbers.xml"
"ref_c_objects.xml"
"ref_c_packages.xml"
"ref_c_printer.xml"
"ref_c_reader.xml"
"ref_c_sequences.xml"
"ref_c_streams.xml"
"ref_c_strings.xml"
"ref_c_structures.xml"
"ref_c_symbols.xml"
"ref_c_system_construction.xml"
"ref_c_types_and_classes.xml"
"ref_embed.xmlf"
"ref_memory.xmlf"
"ref_mp.xmlf"
"ref_os.xmlf"
"ref_signals.xmlf"
"schemas.xml"
"signals.xmlf"
"uffi/ref_aggregate.xml"
"uffi/ref_declare.xml"
"uffi/ref_func_libr.xml"
"uffi/ref_object.xml"
"uffi/ref_primitive.xml"
"uffi/ref_string.xml"
"uffi/schemas.xml"
"ansi_arrays.xml"
"ansi_characters.xml"
"ansi_conses.xml"
"ansi_data_flow.xml"
"ansi_environment.xml"
"ansi_evaluation.xml"
"ansi_filenames.xml"
"ansi_files.xml"
"ansi_hash_tables.xml"
"ansi_numbers.xml"
"ansi_objects.xml"
"ansi_overview.xml"
"ansi_packages.xml"
"ansi_printer.xml"
"ansi_reader.xml"
"ansi_sequences.xml"
"ansi_streams.xml"
"ansi_strings.xml"
"ansi_structures.xml"
"ansi_symbols.xml"
"ansi_system_construction.xml"
"ansi_types.xml"
                )))

(mapcar 'find-file ecl-doc-files)

(defun ecl-doc-revert ()
  (interactive)
  (mapcar '(lambda (x) (let ((a (find-buffer-visiting x)))
			 (and a (switch-to-buffer a)
			      (revert-buffer t t))))
	  ecl-doc-files))

(defun ecl-doc-save ()
  (interactive)
  (mapcar '(lambda (x) (let ((a (find-buffer-visiting x)))
			 (and a (switch-to-buffer a)
			      (save-buffer 0))))
	  ecl-doc-files))
