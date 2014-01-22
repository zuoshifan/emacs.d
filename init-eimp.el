(autoload 'eimp-mode "eimp" "Emacs Image Manipulation Package." t)
(add-hook 'image-mode-hook 'eimp-mode)

;;; Switching to the next/previous image, these are new feature of Emacs 24.4 image-mode
;;; copyed from http://lists.gnu.org/archive/html/emacs-diffs/2013-01/msg00248.html
(defun image-next-file (&optional n)
  "Visit the next image in the same directory as the current image file.
With optional argument N, visit the Nth image file after the
current one, in cyclic alphabetical order.

This command visits the specified file via `find-alternate-file',
replacing the current Image mode buffer."
  (interactive "p")
  (unless (derived-mode-p 'image-mode)
    (error "The buffer is not in Image mode"))
  (unless buffer-file-name
    (error "The current image is not associated with a file"))
  (let* ((file (file-name-nondirectory buffer-file-name))
        (images (image-mode--images-in-directory file))
        (idx 0))
    (catch 'image-visit-next-file
      (dolist (f images)
       (if (string= f file)
           (throw 'image-visit-next-file (1+ idx)))
       (setq idx (1+ idx))))
    (setq idx (mod (+ idx (or n 1)) (length images)))
    (find-alternate-file (nth idx images))))

(defun image-previous-file (&optional n)
  "Visit the preceding image in the same directory as the current file.
With optional argument N, visit the Nth image file preceding the
current one, in cyclic alphabetical order.

This command visits the specified file via `find-alternate-file',
replacing the current Image mode buffer."
  (interactive "p")
  (image-next-file (- n)))

(defun image-mode--images-in-directory (file)
  (let* ((dir (file-name-directory buffer-file-name))
        (files (directory-files dir nil
                                (image-file-name-regexp) t)))
    ;; Add the current file to the list of images if necessary, in
    ;; case it does not match `image-file-name-regexp'.
    (unless (member file files)
      (push file files))
    (sort files 'string-lessp)))

(eval-after-load "eimp"
  '(progn
     (define-key eimp-minor-mode-map (kbd "n") 'image-next-file)
     (define-key eimp-minor-mode-map (kbd "p") 'image-previous-file)     ))

(provide 'init-eimp)
