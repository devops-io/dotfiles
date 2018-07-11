;;; git.el --- A user interface for git

;; Copyright (C) 2005, 2006, 2007, 2008, 2009 Alexandre Julliard <julliard@winehq.org>

;; Version: 1.0

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; This file contains an interface for the git version control
;; system. It provides easy access to the most frequently used git
;; commands. The user interface is as far as possible identical to
;; that of the PCL-CVS mode.
;;
;; To install: put this file on the load-path and place the following
;; in your .emacs file:
;;
;;    (require 'git)
;;
;; To start: `M-x git-status'
;;
;; TODO
;;  - diff against other branch
;;  - renaming files from the status buffer
;;  - creating tags
;;  - fetch/pull
;;  - revlist browser
;;  - git-show-branch browser
;;

;;; Compatibility:
;;
;; This file works on GNU Emacs 21 or later. It may work on older
;; versions but this is not guaranteed.
;;
;; It may work on XEmacs 21, provided that you first install the ewoc
;; and log-edit packages.
;;

(eval-when-compile (require 'cl))
(require 'ewoc)
(require 'log-edit)
(require 'easymenu)


;;;; Customizations
;;;; ------------------------------------------------------------

(defgroup git nil
  "A user interface for the git versioning system."
  :group 'tools)

(defcustom git-committer-name nil
  "User name to use for commits.
The default is to fall back to the repository config,
then to `add-log-full-name' and then to `user-full-name'."
  :group 'git
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Name")))

(defcustom git-committer-email nil
  "Email address to use for commits.
The default is to fall back to the git repository config,
then to `add-log-mailing-address' and then to `user-mail-address'."
  :group 'git
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Email")))

(defcustom git-commits-coding-system nil
  "Default coding system for the log message of git commits."
  :group 'git
  :type '(choice (const :tag "From repository config" nil)
                 (coding-system)))

(defcustom git-append-signed-off-by nil
  "Whether to append a Signed-off-by line to the commit message before editing."
  :group 'git
  :type 'boolean)

(defcustom git-reuse-status-buffer t
  "Whether `git-status' should try to reuse an existing buffer
if there is already one that displays the same directory."
  :group 'git
  :type 'boolean)

(defcustom git-per-dir-ignore-file ".gitignore"
  "Name of the per-directory ignore file."
  :group 'git
  :type 'string)

(defcustom git-show-uptodate nil
  "Whether to display up-to-date files."
  :group 'git
  :type 'boolean)

(defcustom git-show-ignored nil
  "Whether to display ignored files."
  :group 'git
  :type 'boolean)

(defcustom git-show-unknown t
  "Whether to display unknown files."
  :group 'git
  :type 'boolean)


(defface git-status-face
  '((((class color) (background light)) (:foreground "purple"))
    (((class color) (background dark)) (:foreground "salmon")))
  "Git mode face used to highlight added and modified files."
  :group 'git)

(defface git-unmerged-face
  '((((class color) (background light)) (:foreground "red" :bold t))
    (((class color) (background dark)) (:foreground "red" :bold t)))
  "Git mode face used to highlight unmerged files."
  :group 'git)

(defface git-unknown-face
  '((((class color) (background light)) (:foreground "goldenrod" :bold t))
    (((class color) (back