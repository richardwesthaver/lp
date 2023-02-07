;;; lp-tests.el --- Tests for lp.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'lp)

(ert-deftest test-lp-c ()
  (should
   (equal
    (lp-with-input "ab"
      (lp-c ?a)
      (lp-c ?b))
    "b"))
  (should
   (equal
    (lp-with-input "ab"
      (lp-query (lp-c ?a) :beg))
    1)))

(ert-deftest test-lp-sat ()
  (should
   (equal
    (lp-with-input "ab"
      (lp-c ?a)
      (lp-sat (lambda (c) (char-equal c ?b))))
    "b"))
  (should
   (equal
    (lp-with-input "ab"
      (lp-c ?a)
      (lp-query (lp-sat (lambda (c) (char-equal c ?b))) :end))
    3)))

(ert-deftest test-lp-eol ()
  (should
   (equal
    (lp-with-input "\na"
      (lp-newline)
      (lp-c ?a))
    "a"))
  (should
   (equal
    (lp-with-input "\r\na"
      (lp-crlf)
      (lp-c ?a))
    "a"))
  (should
   (equal
    (lp-with-input "\r\na"
      (lp-eol)
      (lp-c ?a))
    "a"))
  (should
   (equal
    (lp-with-input "\na"
      (lp-eol)
      (lp-c ?a))
    "a"))
  (should
   (equal
    (lp-with-input "\ra"
      (lp-eol)
      (lph-ch ?a))
    (lp-err2 "\n" "a"))))

(ert-deftest test-lp-eof ()
  (should
   (equal
    (lp-with-input "\r\na"
      (lp-eol)
      (lp-c ?a)
      (lp-eof))
    nil)))

(ert-deftest test-lp-re ()
  (should
   (equal
    (lp-with-input "abc"
      (lp-query
       (lp-re "\\(a\\)\\(bc\\)")
       :group 2))
    "bc")))

(ert-deftest test-lp-make-alt ()
  (should
   (equal
    (lp-make-alt '(?-))
    "-"))
  (should
   (equal
    (lp-make-alt '(?- ?\] ?a ?^))
    "]a^-"))
  (should
   (equal
    (lp-make-alt '(?- ?^))
    "-^"))
  (should
   (equal
    (lp-make-alt '(?^ ?\"))
    "\"^")))

(ert-deftest test-lp-c-in ()
  (should
   (equal
    (lp-with-input "^]-"
      (lp-many-as-string (lp-c-in ?^ ?\] ?-)))
    "^]-"))
  (should
   (equal
    (lp-with-input "^-"
      (lp-many-as-string (lp-c-in ?^ ?-)))
    "^-")))

(ert-deftest test-lp-c-not-in ()
  (should
   (equal
    (lp-with-input "-[]"
      (lp-c-not-in ?\] ?^)
      (lp-c-in ?\[ ?\])
      (lp-c-not-in ?- ?^))
    "]")))

(ert-deftest test-lp-str ()
  (should
   (equal
    (lp-with-input "abc"
      (lp-str "abc"))
    "abc"))
  (should
   (equal
    (lp-with-input "abc"
      (lp-or (lp-str "ac")
                 (lp-c ?a)))
    "a")))

(ert-deftest test-lp-string ()
  (should
   (equal
    (lp-with-input "abc"
      (lp-string "abc"))
    "abc"))
  (should
   (equal
    (lp-with-input "abc"
      (lp-or (lp-string "ac")
                 (lp-c ?a)))
    (lp-err2 "c" "b")))
  (should
   (equal
    (lp-with-input "abc"
      (lp-or (lp-try (lp-string "ac"))
                 (lp-c ?a)))
    "a")))

(ert-deftest test-lp-or ()
  (should
   (equal
    (lp-with-input "1"
      (lp-or (lp-a)
             (lp-d)))
    "1"))
  (should
   (equal
    (lp-with-input "124"
      (lp-or (lp-string "13")
                 (lp-c ?1)))
    (lp-err2 "3" "2")))
  (should
   (equal
    (lp-with-input "124"
      (lp-or (lp-str "13")
                 (lp-c ?1)))
    "1")))

(ert-deftest test-lp-collect-optional ()
  (should
   (equal
    (lp-with-input "abc-def"
      (lp-coll-s
       (lp-and
         (lp-c ?a)
         (lp-str "bc"))
       (lp-optional (lp-c ?-))
       (lp-and
         (lp-ret (lp-str "de")
           (lp-c ?f)))))
    "bc-de"))
  (should
   (equal
    (lp-with-input "abcdef"
      (lp-coll-s
       (lp-and
         (lp-c ?a)
         (lp-str "bc"))
       (lp-optional (lp-c ?-))
       (lp-and
         (lp-ret (lp-str "de")
           (lp-c ?f)))))
    "bcde")))

(ert-deftest test-lp-try ()
  (should
   (equal
    (lp-with-input "abc"
      (lp-or (lp-try (lp-string "abd"))
                 (lp-str "abc")))
    "abc")))

(ert-deftest test-lp-lookahead ()
  (should
   (equal
    (lp-with-input "abc"
      (lp-lookahead (lp-str "abc"))
      (point))
    (point-min)))
  (should
   (equal
    (lp-with-input "abc"
      (lp-start
       (lp-lookahead
        (lp-and
          (lp-c ?a)
          (lp-c ?c))))
      (point))
    (1+ (point-min))))
  (should
   (equal
    (lp-with-input "abc"
      (lp-start
       (lp-try
        (lp-lookahead
         (lp-and
           (lp-c ?a)
           (lp-c ?c)))))
      (point))
    (point-min))))

(ert-deftest test-lp-error-handles ()
  (should
   (equal
    (lp-with-input "abc"
      (lp-with-err "foo"
        (lp-str "abd")))
    (lp-err "foo")))
  (should
   (equal
    (lp-with-input "abc"
      (lp-with-err "foo"
        (lp-str "abc")))
    "abc"))
  (should
   (equal
    (condition-case err
        (lp-with-input "abc"
          (lp-ensure-with-err "foo"
            (lp-str "abd")))
      (error (cdr err)))
    '("foo")))
  (should
   (equal
    (condition-case err
        (lp-with-input "abc"
          (lp-ensure-with-err "foo"
            (lp-str "abc")))
      (error (cdr err)))
    "abc")))

(ert-deftest test-lp-many ()
  (should
   (equal
    (lp-with-input "aaaaab"
      (lp-coll-s
       (lp-many-as-string (lp-c ?a))
       (lp-many-as-string (lp-c ?c))
       (lp-many1-as-string (lp-c ?b))))
    "aaaaab"))
  (should
   (equal
    (lp-with-input "aaaaab"
      (lp-coll-s
       (lp-many-as-string (lp-c ?a))
       (lp-many-as-string (lp-c ?c))
       (lp-many1-as-string (lp-c ?b))
       (lp-many1-as-string (lp-c ?c))))
    (lp-err2 "c" "`EOF'")))
  (should
   (equal
    (lp-with-input "abababaa"
      (lp-many1-as-string (lp-string "ab")))
    (lp-err2 "b" "a")))
  (should
   (equal
    (lp-with-input "abababaa"
      (lp-many1-as-string (lp-try (lp-string "ab")))
      (lp-str "aa"))
    "aa"))
  (should
   (equal
    (lp-with-input "abababaa"
      (lp-many1-as-string (lp-str "ab"))
      (lp-str "aa"))
    "aa")))


(ert-deftest test-lp-till ()
  (should
   (equal
    (lp-with-input "abcd"
      (lp-many-till-as-string (lp-c*) (lp-c ?d)))
    "abc"))
  (should
   (equal
    (lp-with-input "abcd"
      (lp-many-till-as-string (lp-c*) (lp-c ?d) :both))
    '("abc" . "d")))
  (should
   (equal
    (lp-with-input "abcd"
      (lp-many-till-as-string (lp-c*) (lp-c ?d) :end))
    "d"))
  (should
   (equal
    (lp-with-input "abcd"
      (lp-with-err "eof"
        (lp-many-till-as-string (lp-c*) (lp-c ?e))))
    (lp-err "eof")))
  (should
   (equal
    (lp-with-input "abc"
      (lp-until-as-string (lp-c ?c)))
    "ab"))
  (should
   (equal
    (lp-with-input "abc"
      (lp-until-as-string (lp-c ?c) :end))
    "c"))
  (should
   (equal
    (lp-with-input "abc"
      (lp-query (lp-until-as-string (lp-c ?c)) :beg))
    1)))

(ert-deftest test-lp-not-followed-by ()
  (should
   (equal
    (lp-with-input "abd"
      (lp-coll*
       (lp-str "ab")
       (lp-not-followed-by (lp-c ?c))
       (lp-c ?d)))
    '("ab" "d")))
  (should
   (equal
    (lp-with-input "abd"
      (lp-coll*
       (lp-str "ab")
       (lp-or (lp-not-followed-by (lp-c ?d))
                  (lp-not-followed-by (lp-c ?c)))
       (lp-c ?d)))
    '("ab" "d"))))

(ert-deftest test-lp-endby ()
  (should
   (equal
    (lp-with-input "abc\ndef"
      (lp-endby (lp-many-as-string (lp-a))
                    (lp-eol-or-eof)))
    '("abc" "def"))))

(ert-deftest test-lp-sepby ()
  (should
   (equal
    (lp-with-input "ab,cd,ef"
      (lp-sepby (lp-many-as-string (lp-re "[^,]"))
                    (lp-c ?,)))
    '("ab" "cd" "ef"))))

(ert-deftest test-lp-between ()
  (should
   (equal
    (lp-with-input "{abc}"
      (lp-between
       (lp-c ?\{) (lp-c ?\})
       (lp-or
        (lp-str "ac")
        (lp-many-as-string (lp-a)))))
    "abc"))
  (should
   (equal
    (lp-with-input "{abc}"
      (lp-between
       (lp-c ?\{) (lp-c ?\})
       (lp-or
        (lp-string "ac")
        (lp-many-as-string (lp-a)))))
    (lp-err2 "c" "b"))))

(ert-deftest test-lp-count ()
  (should
   (equal
    (lp-with-input "aaaab"
      (lp-ret (lp-count-as-string 3 (lp-c ?a))
        (lp-many1 (lp-c-in ?a ?b))))
    "aaa")))

(ert-deftest test-lp-option ()
  (should
   (equal
    (lp-with-input "ab"
      (lp-option "opt" (lp-string "ac")))
    (lp-err2 "c" "b")))
  (should
   (equal
    (lp-with-input "ab"
      (lp-option "opt" (lp-str "ac")))
    "opt"))
  (should
   (equal
    (lp-with-input "ab"
      (lp-option "opt" (lp-string "ab")))
    "ab")))

(ert-deftest test-lp-optional ()
  (should
   (equal
    (lp-with-input "abcdef"
      (lp-coll-s
       (lp-str "abc")
       (lp-optional (lp-c ?-))
       (lp-str "def")))
    "abcdef"))
  (should
   (equal
    (lp-with-input "abc-def"
      (lp-coll-s
       (lp-str "abc")
       (lp-optional (lp-c ?-))
       (lp-str "def")))
    "abc-def"))
  (should
   (equal
    (lp-with-input "abcdef"
      (lp-coll-s
       (lp-str "abc")
       (lp-from-maybe (lp-optional-maybe (lp-c ?-)))
       (lp-str "def")))
    "abcdef"))
  (should
   (equal
    (lp-with-input "abc-def"
      (lp-coll-s
       (lp-str "abc")
       (lp-from-maybe (lp-optional-maybe (lp-c ?-)))
       (lp-str "def")))
    "abc-def")))

(provide 'lp-tests)
;;; lp-tests.el ends here
