;;; md-reading-tests.el --- Reading column regression tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Exercise source preservation, table exceptions and window-local layout.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'md-mode)

(defconst md-reading-tests--source
  (concat "# A long heading with **bold** and a [link](https://example.com)\n\n"
          "- A long list item with enough words to wrap several times in a narrow reading column.\n\n"
          "A paragraph with **bold**, *italic*, `code`, and enough ordinary words to wrap.\n\n"
          "| Name | Description |\n| --- | --- |\n| Alpha | A wide table cell |\n\n"
          "```text\nA long line of code with enough words to need wrapping as well.\n```\n")
  "Markdown fixture with prose, a table and a source block.")

(ert-deftest md-reading-keeps-markdown-and-full-width-tables ()
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert md-reading-tests--source)
      (md-mode)
      (set-buffer-modified-p nil)
      (let ((md-reading-width 35)
            (md-reading-padding 2)
            (md-render-wrap-lines t)
            (md-render-table-wrap-columns t))
        (md-mode-render)
        (should (equal (md-render-reconstruct (point-min) (point-max))
                       md-reading-tests--source))
        (should-not (buffer-modified-p))
        (should (cl-some (lambda (overlay)
                           (equal (overlay-get overlay 'display) "\n"))
                         (overlays-in (point-min) (point-max))))
        (goto-char (point-min))
        (search-forward "Alpha")
        (should (get-text-property (1- (point)) 'md-render-table-source))
        (should-not (cl-some (lambda (overlay)
                               (overlay-get overlay 'md-reading))
                             (overlays-at (1- (point)))))
        (should (= md-mode--table-widget-width (window-body-width)))
        (md-mode-show-source)
        (should (equal (buffer-string) md-reading-tests--source))
        (should-not md-reading--layouts)
        (should-not md-reading--font-remaps)))))

(ert-deftest md-reading-fonts-are-independent-and-buffer-local ()
  (with-temp-buffer
    (let ((md-reading-font-family "Atkinson Hyperlegible Next")
          (md-reading-text-point-increase 2)
          (md-reading-table-point-increase 1)
          (original face-remapping-alist))
      (md-reading-prepare)
      (let* ((prose (cadr (assq 'default face-remapping-alist)))
             (table (cadr (assq 'fixed-pitch face-remapping-alist)))
             (base (face-attribute 'default :height nil t)))
        (should (equal (plist-get prose :family) md-reading-font-family))
        (should (equal (plist-get table :family) md-reading-font-family))
        (should (= (funcall (plist-get prose :height) base) (+ base 20)))
        (should (= (plist-get table :height) (+ base 10)))
        (let ((cookies md-reading--font-remaps))
          (md-reading-prepare)
          (should (equal cookies md-reading--font-remaps))))
      (md-reading-clear)
      (should (equal original face-remapping-alist)))))

(ert-deftest md-reading-resize-replaces-overlays-without-editing-text ()
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert "A long paragraph with many ordinary words to wrap in the reading column.\n")
      (set-buffer-modified-p nil)
      (let ((md-reading-width 35)
            (md-reading-padding 2))
        (md-reading-prepare)
        (md-reading-apply)
        (let ((overlays (copy-sequence (cddr (car md-reading--layouts))))
              (text (buffer-string)))
          (cl-letf (((symbol-function 'window-body-width)
                     (lambda (&rest _) 25)))
            (md-reading--window-change (selected-window)))
          (should (cl-every (lambda (overlay)
                              (null (overlay-buffer overlay))) overlays))
          (should (equal (buffer-string) text))
          (should-not (buffer-modified-p))
          (should (= (length md-reading--layouts) 1))
          (should (= (caadr (car md-reading--layouts)) 25)))
        (md-reading-clear)))))

(ert-deftest md-reading-scopes-layout-to-each-window ()
  (save-window-excursion
    (with-temp-buffer
      (insert "A paragraph long enough to wrap at the selected reading width.\n")
      (set-window-buffer (selected-window) (current-buffer))
      (let* ((first (selected-window))
             (second (split-window-right))
             (md-reading-width 20)
             (md-reading-padding 1))
        (set-window-buffer second (current-buffer))
        (md-reading-prepare)
        (md-reading-apply)
        (should (= (length md-reading--layouts) 2))
        (dolist (window (list first second))
          (dolist (overlay (cddr (assq window md-reading--layouts)))
            (should (eq (overlay-get overlay 'window) window))))
        (md-reading-clear)))))

(ert-deftest md-reading-wraps-unbroken-words-without-replacing-them ()
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert (make-string 120 ?x))
      (let ((md-reading-width 20)
            (md-reading-padding 1))
        (md-reading-prepare)
        (md-reading-apply)
        (should (cl-some (lambda (overlay)
                           (equal (overlay-get overlay 'before-string) "\n"))
                         (overlays-in (point-min) (point-max))))
        (should (equal (buffer-string) (make-string 120 ?x)))
        (md-reading-clear)))))

(ert-deftest md-reading-render-failure-restores-fonts ()
  (with-temp-buffer
    (insert "Text\n")
    (md-mode)
    (let ((md-reading-font-family "Atkinson Hyperlegible Next")
          (original face-remapping-alist))
      (cl-letf (((symbol-function 'md-render-replace-markup)
                 (lambda (&rest _) (error "Rendering failed"))))
        (should-error (md-mode-render)))
      (should-not md-reading--active)
      (should-not md-reading--font-remaps)
      (should (equal original face-remapping-alist)))))

(ert-deftest md-reading-invalidates-table-measurements-before-rendering ()
  (with-temp-buffer
    (insert "Text\n")
    (md-mode)
    (setq md-render--table-char-pixel-cache '(8 . 8)
          md-render--table-widget-measure-cache '(old-font . nil)
          md-render--table-face-width-cache (make-hash-table))
    (cl-letf (((symbol-function 'md-render-replace-markup)
               (lambda (&rest _)
                 (should-not md-render--table-char-pixel-cache)
                 (should-not md-render--table-widget-measure-cache)
                 (should-not md-render--table-face-width-cache))))
      (md-mode-render))
    (md-mode-show-source)))

(ert-deftest md-reading-gui-keeps-prose-inside-the-column ()
  (skip-unless (display-graphic-p))
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert "A paragraph with a **bold span**, a [link](https://example.com), and an unbroken word: "
              (make-string 120 ?x) ".\n")
      (let ((md-mode-hook nil)
            (md-render-view-mode-hook nil)
            (md-reading-width 240)
            (md-reading-font-family "Atkinson Hyperlegible Next")
            (md-render-wrap-lines t))
        (md-mode)
        (md-mode-render)
        (goto-char (point-min))
        (redisplay t)
        (let ((left (car (posn-x-y (posn-at-point (point-min)))))
              (positions nil))
          (while (not (eobp))
            (when-let* ((position (posn-at-point (point)))
                        (xy (posn-x-y position)))
              (unless (memq (char-after) '(?\s ?\n))
                (push xy positions)))
            (forward-char 1))
          (should (> (length (delete-dups (mapcar #'cdr positions))) 1))
          (dolist (xy positions)
            (should (>= (car xy) left))
            (should (<= (car xy) (+ left md-reading-width)))))))))

(provide 'md-reading-tests)

;;; md-reading-tests.el ends here
