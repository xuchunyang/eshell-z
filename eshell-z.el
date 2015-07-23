;;; eshell-z.el --- cd to frequent directory in eshell

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; Package-Requires: ((seq "1.0"))
;; Keywords: convenience
;; Version: 0.1
;; Homepage: https://github.com/xuchunyang/eshell-z

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The =eshell-z= package is an Emacs port of [[https://github.com/rupa/z][z]].
;; It keeps track of where you’ve been and how many commands you invoke there,
;; and provides a convenient way to jump to the directories you actually
;; use. =eshell-z= and =z= can work together by sharing the same data file.
;;
;; =eshell-z= provides a new command =eshell/z= for cd to frequent directory in
;; eshell.
;;
;;; Usage:
;;
;;  ~ $ z -h
;;  usage: z [-rtxh] [regex1 regex2 ... regexn]
;;
;;      -r, --rank           match by rank only
;;      -t, --time           match by recent access only
;;      -l, --list           list only
;;      -x, --delete         remove the current directory from the datafile
;;      -h, --help           show a brief help message
;;
;;  examples:
;;
;;      z foo         cd to most frecent dir matching foo
;;      z foo bar     cd to most frecent dir matching foo, then bar
;;      z -r foo      cd to highest ranked dir matching foo
;;      z -t foo      cd to most recently accessed dir matching foo
;;      z -l foo      list all dirs matching foo (by frecency)
;;
;;; Setup:
;;
;; To use this package, add following code to your init.el or .emacs
;;
;;   (require 'eshell-z)
;;

;;; Code:

(require 'eshell)
(require 'em-dirs)

(defcustom eshell-z-freq-dir-hash-table-file-name
  (or (getenv "_Z_DATA")
      (expand-file-name ".z" (getenv "HOME")))
  "If non-nil, name of the file to read/write the freq-dir-hash-table.
If it is nil, the freq-dir-hash-table will not be written to disk."
  :type 'file
  :group 'eshell-dirs)

(defvar eshell-z-freq-dir-hash-table nil
  "The frequent directory that Eshell was in.")

(defun eshell-z--read-freq-dir-hash-table ()
  "Set `eshell-z-freq-dir-hash-table' from a history file."
  (let ((file eshell-z-freq-dir-hash-table-file-name))
    (cond
     ((or (null file)
          (equal file "")
          (file-directory-p file)
          (not (file-readable-p file)))
      nil)
     (t
      (setq eshell-z-freq-dir-hash-table
            (let ((m (make-hash-table :test 'equal)))
              (mapc (lambda (elt)
                      (let* ((entries (split-string elt "|"))
                             (key (car entries))
                             (freq (string-to-number (cadr entries)))
                             (time (car (last entries))))
                        (puthash key (cons key (list :freq freq :time time))
                                 m)))
                    (with-temp-buffer
                      (let ((jka-compr-compression-info-list nil))
                        (insert-file-contents file))
                      (split-string (buffer-string) "\n" t)))
              m))))))

;; Same as `hash-table-values' of `subr-x.el' in Emacs 24.4+
(defsubst eshell-z--hash-table-values (hash-table)
  "Return a list of values in HASH-TABLE."
  (let ((values '()))
    (maphash (lambda (_k v) (push v values)) hash-table)
    values))

(defun eshell-z--write-freq-dir-hash-table ()
  "Write `eshell-z-freq-dir-hash-table' to a history file."
  (let ((file eshell-z-freq-dir-hash-table-file-name))
    (cond
     ((or (null file)
          (equal file "")
          (null eshell-z-freq-dir-hash-table)
          (zerop (hash-table-count eshell-z-freq-dir-hash-table)))
      nil)
     ((and (file-exists-p file)
           (not (file-directory-p file))
           (not (file-writable-p file)))
      (message "Cannot write freq-dir-hash-table file %s" file))
     (t
      (with-temp-buffer
        (insert
         (mapconcat
          (lambda (val)
            (let ((dir (car val))
                  (freq (number-to-string (plist-get (cdr val) :freq)))
                  (time (plist-get (cdr val) :time)))
              (format "%s|%s|%s" dir freq time)))
          (eshell-z--hash-table-values eshell-z-freq-dir-hash-table) "\n"))
        (insert "\n")
        (let ((jka-compr-compression-info-list nil))
          (write-region (point-min) (point-max) file nil 'silent)))))))

(defun eshell-z--add ()
  "Add entry."
  (if eshell-z-freq-dir-hash-table-file-name
      (eshell-z--read-freq-dir-hash-table))
  (unless eshell-z-freq-dir-hash-table
    (setq eshell-z-freq-dir-hash-table (make-hash-table :test 'equal)))
  ;; $HOME isn't worth matching
  (unless (string= (expand-file-name default-directory)
                   (expand-file-name (concat (getenv "HOME") "/")))
    (let* (
           ;; Remove end slash, z doesn't use it
           (key (expand-file-name (substring default-directory 0 -1)))
           (val (gethash key eshell-z-freq-dir-hash-table)))
      (if val
          (puthash key (cons key
                             (list :freq (1+ (plist-get (cdr val) :freq))
                                   :time (number-to-string
                                          (truncate (time-to-seconds)))))
                   eshell-z-freq-dir-hash-table)
        (puthash key (cons key
                           (list :freq 1
                                 :time (number-to-string
                                        (truncate (time-to-seconds)))))
                 eshell-z-freq-dir-hash-table))))
  (if eshell-z-freq-dir-hash-table-file-name
      (eshell-z--write-freq-dir-hash-table)))

(defvar eshell-z--remove-p nil)

(defun eshell-z--remove ()
  "Remove entry."
  (if eshell-z--remove-p
      (progn
        (unless eshell-z-freq-dir-hash-table
          (setq eshell-z-freq-dir-hash-table (make-hash-table :test 'equal)))
        (remhash (expand-file-name (substring default-directory 0 -1))
                 eshell-z-freq-dir-hash-table)
        (if eshell-z-freq-dir-hash-table-file-name
            (eshell-z--write-freq-dir-hash-table))
        (setq eshell-z--remove-p nil))))

(add-hook 'eshell-post-command-hook #'eshell-z--add)
(add-hook 'eshell-post-command-hook #'eshell-z--remove 'append)

(defun eshell-z--frecent (value)
  "Calculate rank of a VALUE of `eshell-z-freq-dir-hash-table'.
Base on frequency and time."
  (let* ((freq (plist-get (cdr value) :freq))
         (time (string-to-number (plist-get (cdr value) :time)))
         (dx (- (truncate (time-to-seconds)) time)))
    (cond ((< dx 3600) (* freq 4))
          ((< dx 86400) (* freq 2))
          ((< dx 604800) (/ freq 2))
          (t (/ freq 4)))))

(defun eshell-z--rank (value)
  "Get freq of a VALUE of `eshell-z-freq-dir-hash-table'."
  (plist-get (cdr value) :freq))

(defun eshell-z--time (value)
  "Get time of a VALUE of `eshell-z-freq-dir-hash-table'."
  (string-to-number (plist-get (cdr value) :time)))

;; TODO: eshell command line arguments completion with pcomplete
;; TODO: Implement the -c option
;; TODO: Fix the internal representation of time, string => number
(defun eshell/z (&rest args)
  "cd to frequent directory in eshell."
  (eshell-eval-using-options
   "z" args
   '((?r "rank" nil rank-only "match by rank only")
     (?t "time" nil time-only "match by recent access only")
     (?l "list" nil list "list only")
     (?x "delete" nil delete "remove the current directory from the datafile" )
     (?h "help" nil nil "show a brief help message")
     :usage "[-rtlxh] [regex1 regex2 ... regexn]"
     :post-usage "examples:

    z foo         cd to most frecent dir matching foo
    z foo bar     cd to most frecent dir matching foo, then bar
    z -r foo      cd to highest ranked dir matching foo
    z -t foo      cd to most recently accessed dir matching foo
    z -l foo      list all dirs matching foo (by frecency)
")
   (if delete
       (setq eshell-z--remove-p t)
     (let ((paths (sort (eshell-z--hash-table-values eshell-z-freq-dir-hash-table)
                        (if rank-only
                            (lambda (elt1 elt2)
                              (> (eshell-z--rank elt1)
                                 (eshell-z--rank elt2)))
                          (if time-only
                              (lambda (elt1 elt2)
                                (> (eshell-z--time elt1)
                                   (eshell-z--time elt2)))
                            (lambda (elt1 elt2)
                              (> (eshell-z--frecent elt1)
                                 (eshell-z--frecent elt2))))))))
       (if list
           (eshell-print
            (mapconcat #'car (nreverse
                              (seq-filter
                               (lambda (elt)
                                 (string-match (mapconcat #'identity args ".*")
                                               (car elt)))
                               paths)) "\n"))
         (if (null args)
             (eshell/cd (list (completing-read "pattern " paths nil t)))
           (let ((path (car args))
                 (case-fold-search (eshell-under-windows-p)))
             (if (numberp path)
                 (setq path (number-to-string path)))
             ;; if we hit enter on a completion just go there
             (if (file-accessible-directory-p path)
                 (eshell/cd (list path))
               (let ((newdir
                      (caar (seq-filter
                             (lambda (elt)
                               (string-match (mapconcat #'identity args ".*")
                                             (car elt)))
                             paths))))
                 (if (file-accessible-directory-p newdir)
                     (eshell/cd (list newdir))))))))))
   nil))

(provide 'eshell-z)
;;; eshell-z.el ends here
