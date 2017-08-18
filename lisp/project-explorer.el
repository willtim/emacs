;;; project-explorer.el

(defun dwim-toggle-or-open ()
  "Toggle subtree or open the file."
  (interactive)
  (if (file-directory-p (dired-get-file-for-visit))
      (progn
	(dired-subtree-toggle)
	(revert-buffer))
    (dired-find-file)))

(defun mouse-dwim-to-toggle-or-open (event)
  "Toggle subtree or the open file on mouse-click in dired."
  (interactive "e")
  (let* ((window (posn-window (event-end event)))
	 (buffer (window-buffer window))
	 (pos (posn-point (event-end event))))
    (progn
      (with-current-buffer buffer
	(goto-char pos)
	(dwim-toggle-or-open)))))

;;(defun cycle-project-explorer ()

(defun toggle-project-explorer ()
  "Toggle the project explorer window."
  (interactive)
  (let* ((buffer (dired-noselect (projectile-project-root)))
	(window (get-buffer-window buffer)))
    (if window
	(hide-project-explorer)
        (show-project-explorer))))

(defun show-project-explorer ()
  "Project dired buffer on the side of the frame.
Shows the projectile root folder using dired on the left side of
the frame and makes it a dedicated window for that buffer."
  (let ((buffer (dired-noselect (projectile-project-root))))
    (progn
      (with-current-buffer buffer (dired-hide-details-mode 1))
      (display-buffer-in-side-window buffer '((side . left) (window-width . 0.2)))
      (set-window-dedicated-p (get-buffer-window buffer) t))))

(defun hide-project-explorer ()
  "Hide the project-explorer window."
  (let ((buffer (dired-noselect (projectile-project-root))))
    (progn
      (delete-window (get-buffer-window buffer))
      (kill-buffer buffer))))

(provide 'project-explorer)
