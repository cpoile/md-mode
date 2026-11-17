;;; md-reading.el --- Centered reading layout with wide tables -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Christopher Poile
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Window-specific overlays center and wrap rendered text while leaving table
;; widgets at full window width.  Neither text nor source annotations change.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'face-remap)
(require 'subr-x)

(defgroup md-reading nil
  "Typography and layout of rendered Markdown."
  :group 'text)

(defcustom md-reading-width nil
  "Maximum reading column width in pixels, or nil for ordinary layout.
Rendered tables always keep the full window width."
  :type '(choice (const :tag "Window width" nil) (integer :tag "Pixels"))
  :group 'md-reading)

(defcustom md-reading-padding 16
  "Minimum space in pixels on either side of non-table content."
  :type 'natnum
  :group 'md-reading)

(defcustom md-reading-font-family nil
  "Font family for rendered text and tables, or nil to retain existing fonts."
  :type '(choice (const :tag "Existing fonts" nil) string)
  :group 'md-reading)

(defcustom md-reading-text-point-increase 0
  "Number of points to add to the default font for rendered prose."
  :type 'number
  :group 'md-reading)

(defcustom md-reading-font-weight nil
  "Base weight of rendered reading text, or nil to retain the default.
The value `normal' corresponds to regular weight (CSS weight 400).
Faces for bold emphasis and headings retain their own weight."
  :type '(choice (const :tag "Default weight" nil)
                 (const :tag "Regular (400)" normal)
                 (const light) (const medium) (const semibold))
  :group 'md-reading)

(defcustom md-reading-line-spacing nil
  "Additional rendered line spacing, or nil to retain existing spacing.
Integers specify pixels; floats specify a fraction of the frame line height."
  :type '(choice (const :tag "Existing spacing" nil) number)
  :group 'md-reading)

(defcustom md-reading-bold-color nil
  "Foreground color for rendered bold text, or nil for the theme color."
  :type '(choice (const :tag "Theme color" nil) color)
  :group 'md-reading)

(defcustom md-reading-plain-links nil
  "Whether rendered links use normal weight without an inherited background.
Link color and underlining remain distinct from surrounding prose."
  :type 'boolean
  :group 'md-reading)

(defcustom md-reading-table-point-increase 0
  "Number of points to add to the default font for rendered tables.
This is independent of `md-reading-text-point-increase'."
  :type 'number
  :group 'md-reading)

(defcustom md-reading-table-stripe-darkening nil
  "Fraction by which to darken alternating table rows, or nil for theme faces.
For example, 0.08 makes striped rows eight percent darker than the
default background.  This is recalculated when entering rendered view."
  :type '(choice (const :tag "Theme face" nil) float)
  :group 'md-reading)

(defvar-local md-reading--active nil
  "Non-nil while the buffer has reading typography enabled.")

(defvar-local md-reading--font-remaps nil
  "Face-remapping cookies owned by the reading view.")

(defvar-local md-reading--layouts nil
  "Window layout records, each (WINDOW SIGNATURE . OVERLAYS).")

(defvar-local md-reading--line-spacing-state nil
  "Saved `line-spacing' locality and value, or nil when unchanged.")

(defvar-local md-reading--link-remapping nil
  "Cons of the reading link entry and its original remapping entry.")

(defvar md-reading--updating nil
  "Non-nil while reading overlays are being rebuilt.")

(defun md-reading--stripe-background ()
  "Return a darker shade of the default background for alternating rows."
  (when md-reading-table-stripe-darkening
    (let* ((background (face-background 'default nil t))
           ;; Parse RGB literals directly: terminal color lookup can round
           ;; them to ANSI colors before the shade has been calculated.
           (rgb (or (when (and (stringp background)
                               (string-match-p
                                "\\`#[[:xdigit:]]\\{6\\}\\'" background))
                      (cl-loop for offset in '(1 3 5)
                               collect (/ (string-to-number
                                           (substring background offset
                                                      (+ offset 2)) 16)
                                          255.0)))
                    (color-name-to-rgb background)))
           (scale (- 1 (max 0 (min 1 md-reading-table-stripe-darkening)))))
      (when rgb
        (apply #'color-rgb-to-hex
               (append (mapcar (lambda (channel) (* channel scale)) rgb)
                       '(2)))))))

(defun md-reading-prepare ()
  "Prepare buffer-local reading fonts before measuring rendered widgets."
  (unless md-reading--active
    (setq md-reading--active t)
    (when md-reading-line-spacing
      (setq md-reading--line-spacing-state
            (cons (local-variable-p 'line-spacing) line-spacing))
      (setq-local line-spacing md-reading-line-spacing))
    (when (or md-reading-font-family md-reading-font-weight
              (/= md-reading-text-point-increase 0)
              (/= md-reading-table-point-increase 0))
      (let* ((base-height (face-attribute 'default :height nil t))
             (increase (* 10 md-reading-text-point-increase))
             (table-height (+ base-height
                              (* 10 md-reading-table-point-increase)))
             (family (when md-reading-font-family
                       (list :family md-reading-font-family)))
             (weight (when md-reading-font-weight
                       (list :weight md-reading-font-weight))))
        (push (apply #'face-remap-add-relative 'default
                     :height (lambda (height)
                               (+ (if (numberp height) height base-height)
                                  increase))
                     (append family weight))
              md-reading--font-remaps)
        ;; Table measuring and drawing both use fixed-pitch, even when the
        ;; selected family is proportional.  Explicit height avoids inheriting
        ;; the separate prose increase through default.
        (push (apply #'face-remap-add-relative 'fixed-pitch
                     :height (round table-height) family)
              md-reading--font-remaps)
        ;; Inline code follows the prose size, except inside a table where
        ;; the leading fixed-pitch face preserves the table's smaller size.
        (push (apply #'face-remap-add-relative 'md-render-inline-code
                     :height (round (+ (face-attribute 'default :height nil t)
                                       increase)) family)
              md-reading--font-remaps)
        ;; Block code uses the frame's original font, size and weight, rather
        ;; than inheriting the reading or table remapping through fixed-pitch.
        (dolist (face '(md-render-source-block md-render-source-block-language))
          (push (face-remap-add-relative
                 face :family (face-attribute 'default :family nil t)
                 :height (face-attribute 'default :height nil t)
                 :weight (face-attribute 'default :weight nil t))
                md-reading--font-remaps))))
    (when-let* ((background (md-reading--stripe-background)))
      (push (face-remap-add-relative 'md-render-table-zebra
                                     :background background)
            md-reading--font-remaps))
    (when md-reading-bold-color
      (push (face-remap-add-relative 'md-render-bold
                                     :foreground md-reading-bold-color
                                     :weight 'bold)
            md-reading--font-remaps))
    (when md-reading-plain-links
      (let* ((old (assq 'md-render-link face-remapping-alist))
             (foreground (or (and (facep 'markdown-link-face)
                                  (face-foreground 'markdown-link-face nil t))
                             (face-foreground 'link nil t)
                             (face-foreground 'default nil t)))
             ;; A relative remap would still inherit the base link face's
             ;; background.  Replace the complete entry while reading, so
             ;; table stripes can show through links too.
             (entry `(md-render-link
                      (:foreground ,foreground :weight normal :underline t))))
        (setq md-reading--link-remapping (cons entry old))
        (setq-local face-remapping-alist
              (if old
                  (mapcar (lambda (item) (if (eq item old) entry item))
                          face-remapping-alist)
                (cons entry face-remapping-alist)))))
    (add-hook 'window-size-change-functions #'md-reading--window-change nil t)
    (add-hook 'text-scale-mode-hook #'md-reading-refresh nil t)
    (add-hook 'kill-buffer-hook #'md-reading-clear nil t)))

(defun md-reading-clear ()
  "Remove reading overlays and restore the buffer's original typography."
  (dolist (record md-reading--layouts)
    (mapc #'delete-overlay (cddr record)))
  (mapc #'face-remap-remove-relative md-reading--font-remaps)
  (when md-reading--line-spacing-state
    (if (car md-reading--line-spacing-state)
        (setq-local line-spacing (cdr md-reading--line-spacing-state))
      (kill-local-variable 'line-spacing)))
  (when md-reading--link-remapping
    (let ((entry (car md-reading--link-remapping))
          (old (cdr md-reading--link-remapping)))
      (setq-local face-remapping-alist
            (if old
                (mapcar (lambda (item) (if (eq item entry) old item))
                        face-remapping-alist)
              (delq entry face-remapping-alist)))))
  (setq md-reading--layouts nil
        md-reading--font-remaps nil
        md-reading--line-spacing-state nil
        md-reading--link-remapping nil
        md-reading--active nil)
  (remove-hook 'window-size-change-functions #'md-reading--window-change t)
  (remove-hook 'text-scale-mode-hook #'md-reading-refresh t)
  (remove-hook 'kill-buffer-hook #'md-reading-clear t))

(defun md-reading--measure (string window)
  "Return STRING's pixel width using the fonts of WINDOW's buffer."
  (if (and (display-graphic-p (window-frame window))
           (>= emacs-major-version 31))
      (string-pixel-width string (current-buffer))
    (* (string-width string) (window-font-width window))))

(defun md-reading--fit-length (string width window)
  "Return the length of STRING fitting WIDTH pixels in WINDOW."
  (length
   (if (and (display-graphic-p (window-frame window))
            (fboundp 'truncate-string-pixelwise)
            (>= emacs-major-version 31))
       (truncate-string-pixelwise string width (current-buffer))
     (truncate-string-to-width
      string (max 1 (floor width (max 1 (window-font-width window))))))))

(defun md-reading--row-overlay (start end prefix wrap window)
  "Inset START through END using PREFIX and WRAP in WINDOW."
  (let ((overlay (make-overlay start end nil t nil)))
    (overlay-put overlay 'window window)
    (overlay-put overlay 'line-prefix prefix)
    (overlay-put overlay 'wrap-prefix wrap)
    (overlay-put overlay 'md-reading t)
    overlay))

(defun md-reading--line-overlays (start end width margin window)
  "Lay out the line START to END within WIDTH and MARGIN in WINDOW."
  (let* ((spacer (propertize " " 'display `(space :width (,margin))))
         (initial (or (get-text-property start 'line-prefix) line-prefix ""))
         (continuation (or (get-text-property start 'wrap-prefix)
                           wrap-prefix initial))
         (prefix (concat spacer initial))
         (wrap (concat spacer continuation))
         (row-start start)
         (text-start start)
         overlays)
    (while (< text-start end)
      (let* ((text (buffer-substring text-start end))
             (_ (remove-text-properties
                 0 (length text) '(line-prefix nil wrap-prefix nil) text))
             (available (max 1 (- width
                                  (md-reading--measure
                                   (if (= text-start start) initial continuation)
                                   window))))
             (fit (max 1 (md-reading--fit-length text available window))))
        (if (>= fit (length text))
            (setq text-start end)
          (let* ((candidate (+ text-start fit))
                 (bounds
                  (save-excursion
                    (goto-char candidate)
                    (cond
                     ((looking-at "[ \t]+")
                      (cons (point) (match-end 0)))
                     ((re-search-backward "[ \t]+" text-start t)
                      (and (> (match-beginning 0) text-start)
                           (cons (match-beginning 0) (match-end 0)))))))
                 (break-start (if bounds (car bounds) candidate))
                 (break-end (if bounds (cdr bounds) candidate))
                 (break (make-overlay break-start break-end nil t nil)))
            (push (md-reading--row-overlay row-start break-start
                                           prefix wrap window) overlays)
            (overlay-put break 'window window)
            (overlay-put break 'md-reading t)
            (if bounds
                (overlay-put break 'display "\n")
              (overlay-put break 'before-string "\n"))
            (push break overlays)
            (setq row-start break-start
                  text-start break-end
                  prefix wrap)))))
    (push (md-reading--row-overlay row-start (min (1+ end) (point-max))
                                   prefix wrap window) overlays)
    overlays))

(defun md-reading--window-change (window)
  "Update reading layout when WINDOW changes its buffer or size."
  (when (and md-reading--active
             (eq (window-buffer window) (current-buffer)))
    (md-reading--layout-window window)))

(defun md-reading--layout-window (window &optional force)
  "Center non-table content in WINDOW, rebuilding when FORCE is non-nil."
  (when (and md-reading--active md-reading-width
             (not md-reading--updating))
    (let* ((md-reading--updating t)
           (body (window-body-width window t))
           (width (max 1 (min md-reading-width
                              (- body (* 2 md-reading-padding)))))
           (margin (max 0 (/ (- body width) 2)))
           (signature (list body md-reading-width md-reading-padding
                            face-remapping-alist (buffer-chars-modified-tick)))
           (record (assq window md-reading--layouts)))
      (unless (and (not force) (equal signature (cadr record)))
        (dolist (old md-reading--layouts)
          (when (or (eq (car old) window)
                    (not (window-live-p (car old)))
                    (not (eq (window-buffer (car old)) (current-buffer))))
            (mapc #'delete-overlay (cddr old))
            (setq md-reading--layouts (delq old md-reading--layouts))))
        (let (overlays)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (while (not (eobp))
                (unless (get-text-property (point) 'md-render-table-source)
                  (setq overlays
                        (nconc (md-reading--line-overlays
                                (point) (line-end-position) width margin window)
                               overlays)))
                (forward-line 1))))
          (push (cons window (cons signature overlays)) md-reading--layouts))))))

(defun md-reading-apply ()
  "Apply reading layout to each window displaying the rendered buffer."
  (when md-reading--active
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (md-reading--layout-window window))))

(defun md-reading-refresh ()
  "Recalculate reading layout after a typography change."
  (when md-reading--active
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (md-reading--layout-window window t))))

(provide 'md-reading)

;;; md-reading.el ends here
