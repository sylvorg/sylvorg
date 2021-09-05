;;; doom-aiern-modeline-env-test.el --- Unit tests for doom-aiern-modeline-env -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Justin Barclay

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
;;  Unit tests for doom-aiern-modeline-env
;;

;;; Code:

(require 'doom-aiern-modeline-env)

(ert-deftest doom-aiern-modeline-env--ruby/parse-ruby-version-string ()
  (should
   (string= (doom-aiern-modeline-env--ruby-parse "ruby 2.3.3p222 (2016-11-21 revision 56859) [x86_64-darwin16]")
            "2.3.3")))

(ert-deftest doom-aiern-modeline-env--elixir/parse-elixr-version-string ()
  (should
   (string= (doom-aiern-modeline-env--elixir-parse "IEx 1.7.4 (compiled with Erlang/OTP 21)")
            "1.7.4")))

(ert-deftest doom-aiern-modeline-env--rustc/parse-rustc-version-string ()
  (should
   (string= (doom-aiern-modeline-env--rust-parse "rustc 1.32.0-nightly (14997d56a 2018-12-05)")
            "1.32.0")))

(ert-deftest doom-aiern-modeline-env--go/parse-go-version-string ()
  (should
   (string= (doom-aiern-modeline-env--go-parse "go version go1.11.4 darwin/amd64")
            "1.11.4")))

(ert-deftest doom-aiern-modeline-env--perl/parse-perl-version-string ()
  (should
   (string= (doom-aiern-modeline-env--perl-parse
             "This is perl 5, version 18, subversion 2 (v5.18.2) built for darwin-thread-multi-2level
(with 2 registered patches, see perl -V for more detail)

Copyright 1987-2013, Larry Wall

Perl may be copied only under the terms of either the Artistic License or the
GNU General Public License, which may be found in the Perl 5 source kit.

Complete documentation for Perl, including FAQ lists, should be found on
this system using \"man perl\" or \"perldoc perl\".  If you have access to the
Internet, point your browser at http://www.perl.org/, the Perl Home Page.")
            "5.18.2")))

(ert-deftest doom-aiern-modeline-env--python/parse-python-version-string ()
  (should
   (string= (doom-aiern-modeline-env--python-parse "Python 2.7.15")
            "2.7.15")))

;;; doom-aiern-modeline-env-test.el ends here
