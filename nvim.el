;;; nvim.el --- Neovim API client                    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Homepage: https://github.com/xuchunyang/nvim.el
;; Package-Requires: ((emacs "25.1") (msgpack "0.1"))
;; Keywords: tools
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A basic Neovim API <https://neovim.io/doc/user/api.html> client in Emacs Lisp

;;; Code:

(require 'msgpack)

(cl-defstruct (nvim-connection (:constructor nil)
                               (:constructor nvim-connection--make)
                               (:copier nil))
  "Represent a nvim connection."
  (process (cl-assert nil))
  (id 0)
  (continuations (make-hash-table)))

(defun nvim-connection-receive (connection message)
  "Process MESSAGE just received from CONNECTION."
  (pcase-exhaustive message
    (`(1 ,msgid ,error ,result)
     (pcase (gethash msgid (nvim-connection-continuations connection))
       (`(,ok ,err)
        (remhash msgid (nvim-connection-continuations connection))
        (if error
            (funcall err error)
          (funcall ok result)))))
    (`(2 ,method ,params)
     (message "[nvim] notification ignored: %s(%s)" method params))))

(defun nvim-guess-server ()
  "Nvim server address, :h v:servername."
  (or (getenv "NVIM_LISTEN_ADDRESS")
      (car (file-expand-wildcards (concat temporary-file-directory "nvim*/*")))))

(defun nvim-process-filter (proc string)
  "Called when new data STRING has arrived for PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (save-excursion
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
      (when (condition-case nil
                (save-excursion
                  (msgpack-try-read)
                  t)
              (end-of-buffer nil))
        (let ((msgpack-message
               (condition-case-unless-debug oops
                   (msgpack-read)
                 (error
                  (display-warning
                   'nvim
                   (format "Invalid MessagePack: %s %s" (cdr oops) (buffer-string))
                   :warning)
                  nil))))
          (when msgpack-message
            (with-temp-buffer
              (nvim-connection-receive
               (process-get proc 'nvim-connection)
               msgpack-message))))
        (delete-region (point-min) (point))))))


(cl-defun nvim-async-request (connection
                              method
                              params
                              &key success-fn error-fn)
  (let ((id (cl-incf (nvim-connection-id connection)))
        (method
         (pcase-exhaustive method
           ((pred keywordp) (substring (symbol-name method) 1))
           ((pred symbolp) (symbol-name method))
           ((pred stringp) method)))
        (params (vconcat params)))
    (puthash id
             (list (or success-fn
                       (lambda (_)
                         (message "[nvim] success ignored: %d" id)))
                   (or error-fn
                       (lambda (err)
                         (message "[nvim] error ignored: %s" err))))
             (nvim-connection-continuations connection))
    (process-send-string
     (nvim-connection-process connection)
     (msgpack-encode
      (vector 0 id method params)))
    id))

(cl-defun nvim-request (connection method params)
  (let* ((tag (cl-gensym "nvim-request-catch-tag"))
         id
         (retval
          (unwind-protect
              (catch tag
                (setq id
                      (nvim-async-request
                       connection method params
                       :success-fn (lambda (result)
                                     (throw tag `(done ,result)))
                       :error-fn (lambda (err)
                                   (throw tag `(err ,err)))))
                (while t (accept-process-output nil 30)))
            ;; user-quit (C-g)
            (remhash id (nvim-connection-continuations connection)))))
    (pcase retval
      (`(done ,result) result)
      (`(err ,err) (error "[nvim] request id=%d failed: %s" id err)))))

(defun nvim-connect (&optional server)
  (unless server
    (setq server (nvim-guess-server)))
  (cl-assert server nil "No server")
  (let ((buffer (get-buffer-create " *nvim*"))
        proc connection)
    (with-current-buffer buffer
      (buffer-disable-undo)
      (set-buffer-multibyte nil))
    (setq proc (make-network-process
                :name "nvim"
                :buffer buffer
                :service server
                :family 'local
                :coding 'binary
                :filter-multibyte nil
                :filter #'nvim-process-filter))
    (setq connection (nvim-connection--make :process proc))
    (setf (process-get proc 'nvim-connection)  connection)
    connection))

(defun nvim-connection-close (connection)
  (delete-process (nvim-connection-process connection)))

(defun nvim-connection-running-p (connection)
  (process-live-p (nvim-connection-process connection)))

(defun nvim--expand-method (method)
  "Expand METHOD, e.g., list-bufs -> nvim_list_bufs."
  (concat "nvim_"
          (replace-regexp-in-string (rx "-") "_" (symbol-name method))))

(defvar nvim--conn nil)

(defmacro nvim (method &rest args)
  `(progn
     (unless (and nvim--conn (nvim-connection-running-p nvim--conn))
       (setq nvim--conn (nvim-connect)))
     (nvim-request nvim--conn ,(nvim--expand-method method) ',args)))

(provide 'nvim)
;;; nvim.el ends here
