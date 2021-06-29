;;; doom-aiern-modeline-core-test.el --- Unit tests for doom-aiern-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-aiern-modeline

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;;  Unit tests for doom-aiern-modeline.
;;

;;; Code:

(require 'cl-lib)
(require 'doom-aiern-modeline-core)

(ert-deftest doom-aiern-modeline-icon/octicon-icon ()
  (let ((doom-aiern-modeline-icon t)
        (doom-aiern-modeline-unicode-fallback t))
    (should
     (string= (substring-no-properties
               (doom-aiern-modeline-icon 'octicon "octoface" "☻" ":)" 'error))
              ""))))

(ert-deftest doom-aiern-modeline-icon/octicon-unicode ()
  (let ((doom-aiern-modeline-icon nil)
        (doom-aiern-modeline-unicode-fallback t))
    (should
     (string= (substring-no-properties
               (doom-aiern-modeline-icon 'octicon "octoface" "☻" ":)" 'error))
              "☻"))))

(ert-deftest doom-aiern-modeline-icon/octicon-text ()
  (let ((doom-aiern-modeline-icon nil)
        (doom-aiern-modeline-unicode-fallback nil))
    (should
     (string= (substring-no-properties
               (doom-aiern-modeline-icon 'octicon "octoface" "☻" ":)" 'error))
              ":)"))))

(ert-deftest doom-aiern-modeline-project-root/ffip ()
  (let ((default-directory "/home/user/project/")
        (doom-aiern-modeline-project-detection 'ffip)
        (doom-aiern-modeline--project-detected-p t)
        (doom-aiern-modeline--project-root nil))
    (cl-flet ((ffip-get-project-root-directory () "/home/user/project-ffip/"))
      (should (string= (ffip-get-project-root-directory) "/home/user/project-ffip/")))))

(ert-deftest doom-aiern-modeline-project-root/projectile ()
  (let ((default-directory "/home/user/projectile/")
        (doom-aiern-modeline-project-detection 'projectile)
        (doom-aiern-modeline--project-detected-p t)
        (doom-aiern-modeline--project-root nil))
    (cl-flet ((projectile-project-root () default-directory))
      (should (string= (doom-aiern-modeline-project-root) "/home/user/projectile/")))))

(ert-deftest doom-aiern-modeline-project-root/project ()
  (let ((default-directory "/home/user/project-current/")
        (doom-aiern-modeline-project-detection 'project)
        (doom-aiern-modeline--project-detected-p t)
        (doom-aiern-modeline--project-root nil))
    (cl-flet ((project-current (&optional _maybe-prompt _dir)
                               `(vc . ,default-directory)))
      (should (string= (doom-aiern-modeline-project-root) "/home/user/project-current/")))))

(ert-deftest doom-aiern-modeline-project-root/default ()
  (let ((default-directory "/home/user/project/")
        (doom-aiern-modeline-project-detection nil)
        (doom-aiern-modeline--project-detected-p t))
    (should (string= (doom-aiern-modeline-project-root) "/home/user/project/"))))

(ert-deftest doom-aiern-modeline-buffer-file-name/invalid ()
  :expected-result :failed
  (let* ((default-directory "/home/user/project/")
         (buffer-file-name "/home/user/project/relative/test.txt")
         (buffer-file-truename "/home/user/project/relative/test.txt")
         (doom-aiern-modeline--project-detected-p t)
         (doom-aiern-modeline--project-root default-directory)
         (doom-aiern-modeline-buffer-file-name-style 'invalid))
    (cl-flet ((doom-aiern-modeline-project-p () t)
              (doom-aiern-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-aiern-modeline-buffer-file-name))
                "test.txt")))))

(ert-deftest doom-aiern-modeline-buffer-file-name/auto-in-project ()
  (let* ((default-directory "/home/user/project/")
         (buffer-file-name "/home/user/project/relative/test.txt")
         (buffer-file-truename "/home/user/project/relative/test.txt")
         (doom-aiern-modeline--project-detected-p t)
         (doom-aiern-modeline--project-root default-directory)
         (doom-aiern-modeline-buffer-file-name-style 'auto))
    (cl-flet ((doom-aiern-modeline-project-p () t)
              (doom-aiern-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-aiern-modeline-buffer-file-name))
                "project/relative/test.txt")))))

(ert-deftest doom-aiern-modeline-buffer-file-name/auto-file-name ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-aiern-modeline--project-detected-p t)
        (doom-aiern-modeline-buffer-file-name-style 'auto))
    (cl-flet ((doom-aiern-modeline-project-p () nil)
              (doom-aiern-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-aiern-modeline-buffer-file-name))
                "%b")))))

(ert-deftest doom-aiern-modeline-buffer-file-name/file-name ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-aiern-modeline--project-detected-p t)
        (doom-aiern-modeline-buffer-file-name-style 'file-name))
    (should
     (string= (substring-no-properties (doom-aiern-modeline-buffer-file-name))
              "test.txt"))))

(ert-deftest doom-aiern-modeline-buffer-file-name/buffer-name ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-aiern-modeline--project-detected-p t)
        (doom-aiern-modeline-buffer-file-name-style 'buffer-name))
    (should
     (string= (substring-no-properties (doom-aiern-modeline-buffer-file-name))
              "%b"))))

(ert-deftest doom-aiern-modeline-buffer-file-name/truncate-upto-project ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-aiern-modeline--project-detected-p t)
        (doom-aiern-modeline-buffer-file-name-style 'truncate-upto-project))
    (cl-flet ((doom-aiern-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-aiern-modeline-buffer-file-name))
                "/h/u/project/relative/test.txt")))))

(ert-deftest doom-aiern-modeline-buffer-file-name/truncate-from-project ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-aiern-modeline--project-detected-p t)
        (doom-aiern-modeline-buffer-file-name-style 'truncate-from-project))
    (cl-flet ((doom-aiern-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-aiern-modeline-buffer-file-name))
                "/home/user/project/r/test.txt")))))

(ert-deftest doom-aiern-modeline-buffer-file-name/truncate-with-project ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-aiern-modeline--project-detected-p t)
        (doom-aiern-modeline-buffer-file-name-style 'truncate-with-project))
    (cl-flet ((doom-aiern-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-aiern-modeline-buffer-file-name))
                "project/r/test.txt")))))

(ert-deftest doom-aiern-modeline-buffer-file-name/truncate-except-project ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-aiern-modeline--project-detected-p t)
        (doom-aiern-modeline-buffer-file-name-style 'truncate-except-project))
    (cl-flet ((doom-aiern-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-aiern-modeline-buffer-file-name))
                "/h/u/project/r/test.txt")))))

(ert-deftest doom-aiern-modeline-buffer-file-name/truncate-upto-root ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-aiern-modeline--project-detected-p t)
        (doom-aiern-modeline-buffer-file-name-style 'truncate-upto-root))
    (cl-flet ((doom-aiern-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-aiern-modeline-buffer-file-name))
                "/h/u/p/relative/test.txt")))))

(ert-deftest doom-aiern-modeline-buffer-file-name/truncate-all ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-aiern-modeline-buffer-file-name-style 'truncate-all))
    (cl-flet ((doom-aiern-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-aiern-modeline-buffer-file-name))
                "/h/u/p/r/test.txt")))))

(ert-deftest doom-aiern-modeline-buffer-file-name/truncate-nil ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-aiern-modeline-buffer-file-name-style 'truncate-nil))
    (cl-flet ((doom-aiern-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-aiern-modeline-buffer-file-name))
                "/home/user/project/relative/test.txt")))))

(ert-deftest doom-aiern-modeline-buffer-file-name/relative-to-project ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-aiern-modeline--project-detected-p t)
        (doom-aiern-modeline-buffer-file-name-style 'relative-to-project))
    (cl-flet ((doom-aiern-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-aiern-modeline-buffer-file-name))
                "relative/test.txt")))))

(ert-deftest doom-aiern-modeline-buffer-file-name/relative-from-project ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-aiern-modeline--project-detected-p t)
        (doom-aiern-modeline-buffer-file-name-style 'relative-from-project))
    (cl-flet ((doom-aiern-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-aiern-modeline-buffer-file-name))
                "project/relative/test.txt")))))

(ert-deftest doom-aiern-modeline--buffer-file-name/truncate-upto-project ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path nil)
        (doom-aiern-modeline--project-detected-p t))
    (should
     (string= (substring-no-properties
               (doom-aiern-modeline--buffer-file-name file-path true-file-path 'shrink))
              "/h/u/project/relative/test.txt"))))

(ert-deftest doom-aiern-modeline--buffer-file-name/truncate-from-project ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path nil)
        (doom-aiern-modeline--project-detected-p t))
    (should
     (string= (substring-no-properties
               (doom-aiern-modeline--buffer-file-name file-path true-file-path nil 'shrink))
              "/home/user/project/r/test.txt"))))

(ert-deftest doom-aiern-modeline--buffer-file-name/truncate-with-project ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path nil)
        (doom-aiern-modeline--project-detected-p t))
    (should
     (string= (substring-no-properties
               (doom-aiern-modeline--buffer-file-name file-path true-file-path 'shrink 'shrink 'hide))
              "project/r/test.txt"))))

(ert-deftest doom-aiern-modeline--buffer-file-name/truncate-except-project ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path nil)
        (doom-aiern-modeline--project-detected-p t))
    (should
     (string= (substring-no-properties
               (doom-aiern-modeline--buffer-file-name file-path true-file-path 'shrink 'shrink))
              "/h/u/project/r/test.txt"))))

(ert-deftest doom-aiern-modeline--buffer-file-name-truncate/truncate-upto-root ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path "/home/user/project/relative/test.txt")
        (doom-aiern-modeline--project-detected-p t))
    (should
     (string= (substring-no-properties
               (doom-aiern-modeline--buffer-file-name-truncate file-path true-file-path))
              "/h/u/p/relative/test.txt"))))

(ert-deftest doom-aiern-modeline--buffer-file-name-truncate/truncate-all ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path "/home/user/project/relative/test.txt")
        (doom-aiern-modeline--project-detected-p t))
    (should
     (string= (substring-no-properties
               (doom-aiern-modeline--buffer-file-name-truncate file-path true-file-path t))
              "/h/u/p/r/test.txt"))))

(ert-deftest doom-aiern-modeline--buffer-file-name/truncate-nil ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path nil)
        (doom-aiern-modeline--project-detected-p t))
    (should
     (string= (substring-no-properties
               (doom-aiern-modeline--buffer-file-name file-path true-file-path 'nil))
              "/home/user/project/relative/test.txt"))))

(ert-deftest doom-aiern-modeline--buffer-file-name-relative/relative-to-project ()
  (let ((default-directory "/home/user/project/")
        (file-path nil)
        (true-file-path "/home/user/project/relative/test.txt")
        (doom-aiern-modeline--project-detected-p t))
    (cl-flet ((doom-aiern-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties
                 (doom-aiern-modeline--buffer-file-name-relative file-path true-file-path))
                "relative/test.txt")))))

(ert-deftest doom-aiern-modeline--buffer-file-name-relative/relative-from-project ()
  (let ((default-directory "/home/user/project/")
        (file-path nil)
        (true-file-path "/home/user/project/relative/test.txt")
        (doom-aiern-modeline--project-detected-p t))
    (cl-flet ((doom-aiern-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties
                 (doom-aiern-modeline--buffer-file-name-relative file-path true-file-path 'include-project))
                "project/relative/test.txt")))))

;;; doom-aiern-modeline-core-test.el ends here
