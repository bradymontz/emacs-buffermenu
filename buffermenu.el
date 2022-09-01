;; buffermenu.el: modifies the emacs buffer menu to organize files by directory
;;
;; This file does three things:
;; 
;; 1. Modifies the emacs buffer menu so that files are sorted
;; according to their directory. I personally find this useful for working
;; on large software projects where I might have 6 different Makefiles
;; open at the same time, for instance. This allows me to quickly go to the
;; correct one.
;;
;; 2. Adds a new menu called mouse-bufferkill-menu. It looks exactly
;; like the buffer menu, except that selecting a buffer immediatly kills it.
;; I find this useful for quickly killing a set of buffers.
;;
;; 3. Binds the buffer menu to (control button1) and the bufferkill menu
;; to (control button2)
;;
;; This code test with emacs version 28, prior versions YMMV
;; 
;; Please send comments, criticisms, and suggestions to:
;;
;;        Brady Montz (bradymontz@mac.com)

;; Copyright (C) 1997-2022 Brady Montz
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA


(defun mouse-buffer-menu-buf-dir (buf)
  (let* ((name (buffer-name buf))
         (path (buffer-file-name buf))
	 (misc-p (or (not path) 
		     (not (null (string-match "\\`*" name))))))
    
    (if misc-p "*Misc*" (file-name-directory path))))

(defun buffer-is-hidden (buf)
  (eq ?\s (aref (buffer-name buf) 0)))

(defun buffer-name-with-flags (buf)
  (let ((name (buffer-name buf))
        (modified (buffer-modified-p buf))
        (read-only (with-current-buffer buf
                     buffer-read-only)))
    (if (or modified read-only)
        (string-join (list name (concat "" (if read-only "%") (if modified "*") )) " ")
      name)))

(defun my-mouse-buffer-menu-map (title)
  (let ((buffers (buffer-list))
        split-by-dir)
    ;; build up the list of non-hidden buffers grouped by directory
    (dolist (buf buffers)
      (unless (buffer-is-hidden buf)
        (let* ((name (buffer-name buf))
               (dir (mouse-buffer-menu-buf-dir buf))
               elt)
          (setq elt (assoc dir split-by-dir))
          (unless elt
            (setq elt (list dir buf)
                  split-by-dir (cons elt split-by-dir)))
          (or (memq buf (cdr elt))
              (setcdr elt (cons buf (cdr elt)))))))

    ;; sort across the list of directory groups, by directory name, putting *Misc* last
    (setq split-by-dir
          (sort split-by-dir
                (lambda (elt1 elt2)
                  (let ((name1 (car elt1))
                        (name2 (car elt2)))
                    (cond ((string= name1 "*Misc*") nil)
                          ((string= name2 "*Misc*") t)
                          (t (string-collate-lessp (downcase name1) (downcase name2))))))))

    ;; sort within each directory group, by buffer name
    (dolist (d split-by-dir)
      (let ((bufs (cdr d)))
        (setcdr d (sort bufs (lambda (b1 b2) (string-collate-lessp (downcase (buffer-name b1)) (downcase (buffer-name b2))))))))

    ;; map it to the format that x-popup-menu wants
    (cons title
          (mapcar
           (lambda (x)
             (let ((dir (car x))
                   (buf-list (cdr x)))
               (cons dir (mapcar
                          (lambda (buf) (cons (buffer-name-with-flags buf) buf))
                          buf-list))))
           split-by-dir))))

(defun my-mouse-buffer-menu-body (event title func)
  (mouse-minibuffer-check event)
  (let ((buf (x-popup-menu event (my-mouse-buffer-menu-map title)))
        (window (posn-window (event-start event))))
    (when buf
      (select-window
       (if (framep window) (frame-selected-window window)
         window))
      (apply func (list buf)))))

(defun my-mouse-buffer-menu (event)
  (interactive "e")
  (my-mouse-buffer-menu-body event "Switch to buffer" 'switch-to-buffer))

(defun my-mouse-bufferkill-menu (event)
  (interactive "e")
  (my-mouse-buffer-menu-body event "Kill buffer" 'kill-buffer))

(global-set-key [C-mouse-1] #'my-mouse-buffer-menu)
(global-set-key [C-down-mouse-1] #'my-mouse-buffer-menu)
(global-set-key [C-down-mouse-2] #'my-mouse-bufferkill-menu)

