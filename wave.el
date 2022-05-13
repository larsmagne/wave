;;; wave.el --- Editing Sound Files
;; Copyright (C) 2001, 2002, 2003, 2010, 2011 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: music

;; This file is not part of GNU Emacs.

;; Wave is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Wave is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Wave; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'message)

(defface wave-face
  '((((class color)
      (background dark))
     (:foreground "white"))
    (((class color)
      (background light))
     (:foreground "black")))
  "Face used for the data.")

(defface wave-split-face
  '((((class color))
     (:foreground "yellow" :background "red")))
  "Face used for split marks.")

(defvar wave-split-positions nil)
(defvar wave-file nil)
(defvar wave-summary nil)
(defvar wave-scale nil)			 

(defvar wave-mode-map nil)
(unless wave-mode-map
  (setq wave-mode-map (copy-keymap text-mode-map))
  (define-key wave-mode-map "s" 'wave-set-split-position)
  (define-key wave-mode-map "r" 'wave-remove-split-position)
  (define-key wave-mode-map "z" 'wave-zoom)
  (define-key wave-mode-map [(meta right)] 'wave-next-screen)
  (define-key wave-mode-map [(meta left)] 'wave-prev-screen)
  (define-key wave-mode-map "q" 'wave-quit)
  (define-key wave-mode-map "<" 'wave-goto-prev-split)
  (define-key wave-mode-map ">" 'wave-goto-next-split)
  (define-key wave-mode-map "Q" 'wave-quit-playing)
  (define-key wave-mode-map "p" 'wave-skip-backwards)
  (define-key wave-mode-map "n" 'wave-skip)
  (define-key wave-mode-map "S" 'wave-split-file)
  (define-key wave-mode-map "\r" 'wave-toggle-play))

(defun wave-mode (&optional arg)
  "Mode for editing WAVE files.

\\{wave-mode-map}"
  (interactive)
  (let ((table (copy-syntax-table text-mode-syntax-table)))
    (set-syntax-table table)
    (modify-syntax-entry ?' "w"))
  (setq major-mode 'wave-mode)
  (setq mode-name "Wave")
  (use-local-map wave-mode-map)
  (set (make-local-variable 'wave-summary) nil)
  (run-hooks 'wave-mode-hook))

;;;###autoload
(defun wave-file (file tracks)
  "Summarize FILE."
  (interactive "fFile name: \nnTracks: ")
  (wave-file-1 (expand-file-name file) nil nil tracks))

(defun wave-file-1 (file &optional start length tracks)
  (let ((auto-splits nil))
    (if (eq major-mode 'wave-mode)
	(switch-to-buffer (generate-new-buffer (concat file " Wave")))
      (pop-to-buffer (generate-new-buffer (concat file " Wave"))))
    (wave-mode)
    (when (and (null start)
	       nil
	       (null length)
	       (plusp tracks))
      (let ((auto-start (wave-find-start file))
	    (auto-end (wave-find-end file)))
	(when nil
	  (setq auto-splits
		(cons auto-start (append (wave-auto-split file tracks
							  auto-start auto-end)
					 (list auto-end)))))
	(when nil
	  (setq auto-splits (wave-auto-split file tracks
					     auto-start auto-end)))))
    (setq wave-summary (wave-analyze-file file start length))
    (when (eq start nil)
      (setq wave-split-positions auto-splits
	    wave-file file)
      (let ((max 0))
	(mapcar (lambda (elem)
		  (setq max (max (cadr (assq 'value elem)) max)))
		(cdr wave-summary))
	(setq wave-scale (/ (* 1.0 (1- (window-height))) max))))
    (wave-generate-summary)))

(defun wave-find-end (file)
  (let* ((file-seconds (/ (nth 7 (file-attributes file))
			  2 2 44100))
	 (summary (cdr (wave-analyze-file file nil nil (* file-seconds 4))))
	 start value frame backup-start)
    (setq summary (nreverse summary))
    (setq backup-start (cadr (assoc 'position (car summary))))
    (while (and summary
		(not start))
      (setq frame (pop summary))
      (setq value (cadr (assoc 'value frame)))
      (when (and (> value 50)
		 (< value 500)
		 (> (wave-mean-series summary 4) 1000))
	(setq start (cadr (assoc 'position frame)))))
    (or start backup-start)))

(defun wave-find-start (file)
  (let* ((file-seconds (/ (nth 7 (file-attributes file))
			  2 2 44100))
	 (summary (cdr (wave-analyze-file file nil nil (* file-seconds 4))))
	 (backup-summary summary)
	 start value frame found-click)
    (while (and summary
		(not found-click))
      (setq frame (pop summary))
      (setq value (cadr (assoc 'value frame)))
      (when (> value 2000)
	(setq found-click t)))
    (unless summary
      (setq summary backup-summary))
    (while (and summary
		(not start))
      (setq frame (pop summary))
      (setq value (cadr (assoc 'value frame)))
      (when (and (< value 800)
		 (> (wave-mean-series summary 4) 1000))
	(setq start (cadr (assoc 'position frame)))))
    start))

(defun wave-mean-series (summary length)
  (let ((done 0)
	(sum 0))
    (while (and summary
		(< done length))
      (cl-incf sum (cadr (assoc 'value (pop summary))))
      (cl-incf done))
    (if (zerop done)
	0
      (/ sum done))))
	 
(defun wave-auto-split (file tracks start end)
  (let* ((file-seconds (/ (nth 7 (file-attributes file))
			  2 2 44100))
	 (summary (wave-analyze-file file nil nil (* file-seconds 4))))
    (cl-loop for bottom in
	     (wave-n-best-bottoms
	      (wave-find-bottoms (wave-smoothe-curve (cdr summary)))
	      (1- tracks))
	     collect (cadr (car bottom)))))
    
(defun wave-generate-summary (&optional smoothe)
  (let* ((general (car wave-summary))
	 (summary (cdr wave-summary))
	 (height (* (- (window-height) 1) 5)))
    (erase-buffer)
    (dotimes (i height)
      (insert "\n"))
    (dolist (column (if smoothe (wave-smoothe-curve summary) summary))
      (wave-insert-column (* wave-scale (cadr (assq 'value column)))
			  (cadr (assq 'position column))
			  (cadr (assq 'frame-size general))
			  (cadr (assq 'value column))))
    (goto-char (point-min))
    (forward-line 1)
    (put-text-property (point) (point-max) 'face 'wave-face)
    (forward-line -1)
    (while (progn
	     (unless (get-text-property (point) 'face)
	       (put-text-property (point) (1+ (point)) 'face 'wave-face))
	     (not (eolp)))
      (forward-char 1))
    (if (not wave-split-positions)
	(goto-char (point-min))
      (wave-goto-last-split))
    (set-buffer-modified-p nil)))

(defun wave-goto-last-split ()
  (goto-char (point-min))
  (end-of-line)
  (while (and (not (bobp))
	      (not (eq (get-text-property (point) 'face)
		       'wave-split-face)))
    (forward-char -1)))
   
(defun wave-goto-next-split ()
  (interactive)
  (forward-char 1)
  (while (and (not (eolp))
	      (not (eq (get-text-property (point) 'face)
		       'wave-split-face)))
    (forward-char 1)))

(defun wave-goto-prev-split ()
  (interactive)
  (forward-char -1)
  (while (and (not (bolp))
	      (not (eq (get-text-property (point) 'face)
		       'wave-split-face)))
    (forward-char -1)))
 
(defun wave-insert-column (height position frame-size value)
  (goto-char (point-min))
  (end-of-line)
  (dotimes (i height)
    (insert "#")
    (forward-line 1)
    (end-of-line))
  (while (not (eobp))
    (insert "o")
    (forward-line 1)
    (end-of-line))
  (goto-char (point-min))
  (end-of-line)
  (put-text-property (1- (point)) (point) 'position position)
  (put-text-property (1- (point)) (point) 'value value)
  (let ((current-split (wave-split-position-p position frame-size)))
    (when current-split
      (put-text-property (1- (point)) (point) 'face 'wave-split-face)
      (put-text-property (1- (point)) (point) 'split-position current-split))))

(defun wave-split-position-p (position frame-size)
  (let ((pos-p nil))
    (dolist (split wave-split-positions)
      (when (and (>= split (- position frame-size))
		 (< split position))
	(setq pos-p split)))
    pos-p))

(defun wave-analyze-file (file &optional start length frames)
  (with-temp-buffer
    (call-process "~/src/wave/summarize"
		  nil (current-buffer) nil
		  "-s" (if start (number-to-string start) "0")
		  "-l" (if length (number-to-string length) "-1")
		  ;;"--summary-size" (format "%d" (* 2 44100 0.5))
		  "-f" (if frames (number-to-string frames)
			 (or "80"
			     (let ((file-seconds
				    (/ (nth 7 (file-attributes file))
				       2 2 44100)))
			       (format "%s" (/ file-seconds 4)))))
		  file)
    (goto-char (point-min))
    (while (re-search-forward "[0-9])" nil t)
      (forward-char -1)
      (insert ".0"))
    (goto-char (point-min))
    (read (current-buffer))))

(defun wave-split-file ()
  "Split the file according to the specifications."
  (interactive)
  (wave-quit-playing)
  (let ((default-directory (file-name-directory wave-file)))
    (with-temp-buffer
      (call-process "~/src/wave/bsplit"
		    nil (current-buffer) nil
		    wave-file
		    (with-temp-buffer
		      (insert
		       (mapconcat 'number-to-string
				  (mapcar
				   (lambda (s)
				     (* s 2))
				   (sort
				    (copy-sequence wave-split-positions) '<))
				  ":"))
		      (goto-char (point-min))
		      (while (search-forward ".0" nil t)
			(replace-match ""))
		      (message (buffer-string))
		      (buffer-string)))
      (goto-char (point-min))
      (read (current-buffer)))
    (let ((files (directory-files (file-name-directory wave-file)
				  t "split-.*.raw")))
      (dolist (file files)
	(rename-file file (replace-regexp-in-string
			   "split-\\([0-9]*\\).raw$" 
			   (replace-regexp-in-string
			    ".raw$" "-\\\\1.raw" wave-file)
			   (file-name-nondirectory file)))))
    (other-window 1)
    (jukebox-redisplay)))

(defun wave-set-split-position ()
  "Mark the current column as a split position."
  (interactive)
  (push (get-text-property (point) 'position) wave-split-positions)
  (put-text-property (point) (1+ (point)) 'face 'wave-split-face))

(defun wave-remove-split-position ()
  "Remove the current split position."
  (interactive)
  (let ((position (get-text-property (point) 'split-position)))
    (unless (member position wave-split-positions)
      (error "Not a split position"))
    (setq wave-split-positions (delete position wave-split-positions)))
  (put-text-property (point) (1+ (point)) 'face 'default))

(defun wave-zoom (&optional n)
  "Zoom to the next N frames."
  (interactive "p")
  (let ((position (get-text-property (point) 'position))
	(frame-size (cadr (assq 'frame-size (car wave-summary)))))
    (wave-file-1 wave-file (- position frame-size) (* frame-size n))))

(defun wave-next-screen ()
  "Go to the next screen frames."
  (interactive)
  (let ((position (get-text-property (1- (line-end-position)) 'position))
	(buffer (current-buffer)))
    (wave-file-1 wave-file position
		 (- position
		    (get-text-property (line-beginning-position) 'position)))
    (kill-buffer buffer)))

(defun wave-prev-screen ()
  "Go to the prev screen frames."
  (interactive)
  (let ((buffer (current-buffer))
	(size
	 (- (get-text-property (1- (line-end-position)) 'position)
	    (get-text-property (line-beginning-position) 'position))))
    (wave-file-1 wave-file
		 (- (get-text-property (line-beginning-position) 'position) size)
		 size)
    (kill-buffer buffer)))

(defun wave-quit ()
  "Pop back to the previous wave buffer."
  (interactive)
  (when (eq major-mode 'wave-mode)
    (kill-buffer (current-buffer))
    (when (eq major-mode 'wave-mode)
      (wave-generate-summary))))

(defvar wave-play-process nil)

(defun wave-quit-playing ()
  "Kill the player."
  (interactive)
  (ignore-errors
    (kill-process wave-play-process)))

(defun wave-skip ()
  "Toggle playing."
  (interactive)
  (forward-char 1)
  (wave-toggle-play))

(defun wave-skip-backwards ()
  "Toggle playing."
  (interactive)
  (forward-char -1)
  (wave-toggle-play))

(defun wave-toggle-play ()
  "Toggle playing."
  (interactive)
  (when (and wave-play-process
	     (memq (process-status wave-play-process) '(open run)))
    (kill-process wave-play-process))
  (while (and
	  wave-play-process
	  (memq (process-status wave-play-process) '(open run)))
    (kill-process wave-play-process))
  (let ((frame-size (cadr (assq 'frame-size (car wave-summary)))))
    (setq wave-play-process
	  (start-process "aplay" (get-buffer-create " *aplay*")
			 "/usr/src/alsa-utils-1.0.13/aplay/aplay"
			 "-f" "S16_LE" "-r" "44100" "-c" "2"
			 "-t" "raw"
			 "-S" (number-to-string
			     (* (- (get-text-property (point) 'position) frame-size)
				2))
			 wave-file))))

(defun wave-smoothe-curve (data)
  (let ((length 5)
	(raw-value nil)
	(array (make-vector (length data) 0))
	(output-array (make-vector (length data) 0))
	(i 0)
	(acc 0))
    (dolist (elem data)
      (aset array i (cadr (assoc 'value elem)))
      (cl-incf i))
    (dotimes (j (1- length))
      (aset output-array j (aref array j))
      (cl-incf acc (aref array j)))
    (cl-loop for j from length upto (1- (length array))
	     do (progn
		  (cl-incf acc (aref array j))
		  (aset output-array (- j (/ length 2))
			(float (round (/ acc length))))
		  (cl-decf acc (aref array (- j length)))))
    (cl-loop for j from (- (length array) (/ length 2)) upto (1- (length array))
	     do (aset output-array j (aref array j)))
    (setq i 0)
    (let ((result nil))
      (dolist (elem data)
	(push (list (car elem)
		    (list 'value (aref output-array i)))
	      result)
	 (cl-incf i))
      (nreverse result))))

(defun wave-find-bottoms (data)
  (setq d data)
  (let ((array (make-vector (length data) 0))
	(darray (make-vector (length data) 0))
	(i 0)
	(prev 0)
	(mean 0.0)
	(sound-cut 0)
	(had-top 0)
	value extremas possible-extremas
	start-plateau)
    (dolist (elem data)
      (aset array i (cadr (assoc 'value elem)))
      (aset darray i elem)
      (cl-incf mean (cadr (assoc 'value elem)))
      (cl-incf i))
    (setq mean (truncate (/ mean (length array)))
	  sound-cut (truncate (* mean 1)))
    (message "mean: %s" mean)
    (cl-loop for i from 1 upto (- (length array) 2)
	     do (progn
		  (setq value (aref array i))
		  (if (= value (aref array (1+ i)))
		      (unless start-plateau
			(setq start-plateau i))
		    (when (and (< value (aref array (1+ i)))
			       (< value prev)
			       (< value (* mean 0.8)))
		      (push (if start-plateau
				(- i (/ (- i start-plateau) 2))
			      i)
			    possible-extremas)
		      (setq start-plateau nil
			    had-top 0))
		    (when (> value sound-cut)
		      (when possible-extremas
			(message "%s" possible-extremas)
			(push (cl-loop with min = 40000
				       with min-point
				       for i in possible-extremas
				       when (< (aref array i) min)
				       do (setq min (aref array i)
						min-point i)
				       finally (return i))
			      extremas)
			(setq possible-extremas nil))
		      (cl-incf had-top))
		    (setq prev value))))
    (cdr
     (cl-loop for i in (nreverse extremas)
	      collect (aref darray i)))))

(defun wave-n-best-bottoms (bottoms n)
  (let ((bottoms (cl-copy-list bottoms))
	low
	(low-elem nil)
	result)
    (while (plusp n)
      (setq low 40000)
      (push
       (cl-loop for bottom in bottoms
		for value = (cadr (assoc 'value bottom))
		when (< value low)
		do (progn
		     (setq low value)
		     (setq low-elem bottom))
		finally (return low-elem))
       result)
      (cl-decf n)
      (setq bottoms (delete (car result) bottoms)))
    (nreverse result)))

(provide 'wave)

;;; wave.el ends here
