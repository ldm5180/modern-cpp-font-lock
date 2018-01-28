;;; modern-cpp-font-lock.el --- Font-locking for "Modern C++"  -*- lexical-binding: t; -*-

;; Copyright © 2016, by Ludwig PACIFICI

;; Author: Ludwig PACIFICI <ludwig@lud.cc>
;; URL: https://github.com/ludwigpacifici/modern-cpp-font-lock
;; Version: 0.1.3
;; Created: 12 May 2016
;; Keywords: languages, c++, cpp, font-lock

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Syntax highlighting support for "Modern C++" - until C++20 and
;; Technical Specification. This package aims to provide a simple
;; highlight of the C++ language without dependency.

;; It is recommended to use it in addition with the c++-mode major
;; mode for extra highlighting (user defined types, functions, etc.)
;; and indentation.

;; Melpa: [M-x] package-install [RET] modern-cpp-font-lock [RET]
;; In your init Emacs file add:
;;     (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
;; or:
;;     (modern-c++-font-lock-global-mode t)

;; For the current buffer, the minor-mode can be turned on/off via the
;; command:
;;     [M-x] modern-c++-font-lock-mode [RET]

;; More documentation:
;; https://github.com/ludwigpacifici/modern-cpp-font-lock/blob/master/README.md

;; Feedback is welcome!

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defgroup modern-c++-font-lock nil
  "Provides font-locking as a Minor Mode for Modern C++"
  :group 'faces)

(eval-and-compile
  (defun modern-c++-string-lenght< (a b) (< (length a) (length b)))
  (defun modern-c++-string-lenght> (a b) (not (modern-c++-string-lenght< a b))))

(defcustom modern-c++-types
  (eval-when-compile
    (sort '("bool" "char" "char16_t" "char32_t" "double" "float" "int" "long" "short" "signed" "unsigned" "void" "wchar_t")
          'modern-c++-string-lenght>))
  "List of C++ types. See doc:
http://en.cppreference.com/w/cpp/language/types"
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'modern-c++-font-lock)

(defcustom modern-c++-preprocessors
  (eval-when-compile
    (sort '("#define" "#defined" "#elif" "#else" "#endif" "#error" "#if" "#ifdef" "#ifndef" "#include" "#line" "#pragma STDC CX_LIMITED_RANGE" "#pragma STDC FENV_ACCESS" "#pragma STDC FP_CONTRACT" "#pragma once" "#pragma pack" "#pragma" "#undef" "_Pragma" "__DATE__" "__FILE__" "__LINE__" "__STDCPP_STRICT_POINTER_SAFETY__" "__STDCPP_THREADS__" "__STDC_HOSTED__" "__STDC_ISO_10646__" "__STDC_MB_MIGHT_NEQ_WC__" "__STDC_VERSION__" "__STDC__" "__TIME__" "__VA_AR_GS__" "__cplusplus" "__has_include")
          'modern-c++-string-lenght>))
  "List of C++ preprocessor words. See doc:
http://en.cppreference.com/w/cpp/keyword and
http://en.cppreference.com/w/cpp/preprocessor"
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'modern-c++-font-lock)

(defcustom modern-c++-keywords
  (eval-when-compile
    (sort '("alignas" "alignof" "and" "and_eq" "asm" "atomic_cancel" "atomic_commit" "atomic_noexcept" "auto" "bitand" "bitor" "break" "case" "catch" "class" "compl" "concept" "const" "constexpr" "const_cast" "continue" "co_await" "co_return" "co_yield" "decltype" "default" "delete" "do" "dynamic_cast" "else" "enum" "explicit" "export" "extern" "final" "for" "friend" "goto" "if" "import" "inline" "module" "mutable" "namespace" "new" "noexcept" "not" "not_eq" "operator" "or" "or_eq" "override" "private" "protected" "public" "register" "reinterpret_cast" "requires" "return" "sizeof" "static" "static_assert" "static_cast" "struct" "switch" "synchronized" "template" "this" "thread_local" "throw" "transaction_safe" "transaction_safe_dynamic" "try" "typedef" "typeid" "typename" "union" "using" "virtual" "volatile" "while" "xor" "xor_eq")
          'modern-c++-string-lenght>))
  "List of C++ keywords. See doc:
http://en.cppreference.com/w/cpp/keyword"
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'modern-c++-font-lock)

(defcustom modern-c++-attributes
  (eval-when-compile
    (sort '("carries_dependency" "deprecated" "fallthrough" "maybe_unused" "nodiscard" "noreturn" "optimize_for_synchronized")
          'modern-c++-string-lenght>))
  "List of C++ attributes. See doc:
http://en.cppreference.com/w/cpp/language/attributes"
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'modern-c++-font-lock)

(defcustom modern-c++-containers
  (eval-when-compile
    (sort '("array" "vector" "deque" "forward_list" "list" "set" "map" "multiset" "multimap" "unordered_set" "unordered_map" "unordered_multiset" "unordered_multimap" "stack" "queue" "priority_queue")
          'modern-c++-string-lenght>))
  "List of C++ attributes. See doc:
http://en.cppreference.com/w/cpp/language/attributes"
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'modern-c++-font-lock)

(defcustom modern-c++-algorithms
  (eval-when-compile
    (sort '("all_of" "any_of" "none_of" "for_each" "for_each_n" "count" "count_if" "mismatch" "equal" "find" "find_if" "find_if_not" "find_end" "find_first_of" "adjacent_find" "search" "search_n" "copy" "copy_if" "copy_n" "copy_backward" "move" "move_backward" "fill" "fill_n" "transform" "generate" "generate_n" "remove" "remove_if" "remove_copy" "remove_copy_if" "replace" "replace_if" "replace_copy" "replace_copy_if" "swap" "swap_ranges" "iter_swap" "reverse" "reverse_copy" "rotate" "rotate_copy" "random_shuffle" "shuffle" "sample" "unique" "unique_copy" "is_partitioned" "partition" "partition_copy" "stable_partition" "partition_point" "is_sorted" "is_sorted_until" "sort" "partial_sort" "partial_sort_copy" "stable_sort" "nth_element" "lower_bound" "upper_bound" "binary_search" "equal_range" "merge" "inplace_merge" "includes" "set_difference" "set_intersection" "set_symmetric_difference" "set_union" "is_heap" "is_heap_until" "make_heap" "push_heap" "pop_heap" "sort_heap" "max" "max_element" "min" "min_element" "minmax" "clamp" "lexicographical_compare" "is_permutation" "next_permutation" "prev_permutation" "iota" "accumulate" "inner_product" "adjacent_difference" "partial_sum" "reduce" "exclusive_scan" "inclusive_scan" "transform_reduce" "transform_exclusive_scan" "transform_inclusive_scan" "uninitialized_copy" "uninitialized_copy_n" "uninitialized_fill" "uninitialized_fill_n" "uninitialized_move" "uninitialized_move_n" "uninitialized_default_contruct" "uninitialized_default_construct_n" "uninitialized_value_construct" "uninitialized_value_construct_n" "destroy_at" "destroy" "destroy_n" "qsort" "bsearch" "memset")
          'modern-c++-string-lenght>))
  "List of C++ algorithms. See doc:
http://en.cppreference.com/w/cpp/algorithm"
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'modern-c++-font-lock)

(defcustom modern-c++-operators
  (eval-when-compile
    (sort '("...")
          'modern-c++-string-lenght>))
  "List of C++ assignment operators. Left Intentionally almost
empty. The user will choose what should be font-locked. By
default I want to avoid a 'christmas tree' C++ code. For more
information, see doc:
http://en.cppreference.com/w/cpp/language/operators"
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'modern-c++-font-lock)

(defvar modern-c++-font-lock-keywords nil)

(defun modern-c++-generate-font-lock-keywords ()
  (let ((types-regexp (regexp-opt modern-c++-types 'symbols))
        (preprocessors-regexp (regexp-opt modern-c++-preprocessors))
        (keywords-regexp (regexp-opt modern-c++-keywords 'words))
        (containers-regexp (regexp-opt modern-c++-containers 'words))
        (attributes-regexp (concat "\\[\\[\\(" (regexp-opt modern-c++-attributes 'words) "\\).*\\]\\]"))
        (algorithms-regexp (regexp-opt modern-c++-algorithms 'words))
        (operators-regexp (regexp-opt modern-c++-operators)))
    (setq modern-c++-font-lock-keywords
          `(
            ;; Note: order below matters, because once colored, that part
            ;; won't change. In general, longer words first
            (,types-regexp (0 font-lock-type-face))
            (,preprocessors-regexp (0 font-lock-preprocessor-face))
            (,attributes-regexp (1 font-lock-constant-face))
            (,operators-regexp (0 font-lock-function-name-face))
            (,containers-regexp (0 font-lock-keyword-face))
            (,algorithms-regexp (0 font-lock-keyword-face))
            (,keywords-regexp (0 font-lock-keyword-face))))))

(defcustom modern-c++-literal-boolean
  t
  "Enable font-lock for boolean literals. For more information,
see documentation:
http://en.cppreference.com/w/cpp/language/bool_literal"
  :type 'boolean
  :group 'modern-c++-font-lock)

(defvar modern-c++-font-lock-literal-boolean nil)

(defun modern-c++-generate-font-lock-literal-boolean ()
  (let ((literal-boolean-regexp (regexp-opt
                                 (eval-when-compile (sort '("false" "true") 'modern-c++-string-lenght>))
                                 'words)))
    (setq modern-c++-font-lock-literal-boolean
          `(
            ;; Note: order below matters, because once colored, that part
            ;; won't change. In general, longer words first
            (,literal-boolean-regexp (0 font-lock-constant-face))))))

(defcustom modern-c++-literal-integer
  t
  "Enable font-lock for integer literals. For more information,
see documentation:
http://en.cppreference.com/w/cpp/language/integer_literal"
  :type 'boolean
  :group 'modern-c++-font-lock)

(defvar modern-c++-font-lock-literal-integer nil)

(defun modern-c++-generate-font-lock-literal-integer ()
  (eval-when-compile
    (let* ((integer-suffix-regexp (regexp-opt (sort '("ull" "LLu" "LLU" "llu" "llU" "uLL" "ULL" "Ull" "ll" "LL" "ul" "uL" "Ul" "UL" "lu" "lU" "LU" "Lu" "u" "U" "l" "L") 'modern-c++-string-lenght>)))
           (not-alpha-numeric-regexp "[^0-9a-zA-Z'\\\._]")
           (literal-binary-regexp (concat not-alpha-numeric-regexp "\\(0[bB]\\)\\([01']+\\)\\(" integer-suffix-regexp "?\\)"))
           (literal-octal-regexp (concat not-alpha-numeric-regexp "\\(0\\)\\([0-7']+\\)\\(" integer-suffix-regexp "?\\)"))
           (literal-hex-regexp (concat not-alpha-numeric-regexp "\\(0[xX]\\)\\([0-9a-fA-F']+\\)\\(" integer-suffix-regexp "?\\)"))
           (literal-dec-regexp (concat not-alpha-numeric-regexp "\\([1-9][0-9']*\\)\\(" integer-suffix-regexp "\\)")))
      (setq modern-c++-font-lock-literal-integer
            `(
              ;; Note: order below matters, because once colored, that part
              ;; won't change. In general, longer words first
              (,literal-binary-regexp (1 font-lock-keyword-face)
                                      (2 font-lock-constant-face)
                                      (3 font-lock-keyword-face))
              (,literal-octal-regexp (1 font-lock-keyword-face)
                                     (2 font-lock-constant-face)
                                     (3 font-lock-keyword-face))
              (,literal-hex-regexp (1 font-lock-keyword-face)
                                   (2 font-lock-constant-face)
                                   (3 font-lock-keyword-face))
              (,literal-dec-regexp (1 font-lock-constant-face)
                                   (2 font-lock-keyword-face)))))))

(defcustom modern-c++-literal-null-pointer
  t
  "Enable font-lock for null pointer literals. For more information,
see documentation:
http://en.cppreference.com/w/cpp/language/nullptr"
  :type 'boolean
  :group 'modern-c++-font-lock)

(defvar modern-c++-font-lock-literal-null-pointer nil)

(defun modern-c++-generate-font-lock-literal-null-pointer ()
  (let ((literal-null-pointer-regexp (regexp-opt
                                      (eval-when-compile (sort '("nullptr") 'modern-c++-string-lenght>))
                                      'words)))
    (setq modern-c++-font-lock-literal-null-pointer
          `(
            ;; Note: order below matters, because once colored, that part
            ;; won't change. In general, longer words first
            (,literal-null-pointer-regexp (0 font-lock-constant-face))))))

(defcustom modern-c++-literal-string
  t
  "Enable font-lock for string literals. For more information,
see documentation:
http://en.cppreference.com/w/cpp/language/string_literal"
  :type 'boolean
  :group 'modern-c++-font-lock)

(defvar modern-c++-font-lock-literal-string nil)

(defun modern-c++-generate-font-lock-literal-string ()
  (eval-when-compile
    (let* ((simple-string-regexp "\"[^\"]*\"")
           (raw "R")
           (prefix-regexp
            (regexp-opt (sort '("L" "u8" "u" "U") 'modern-c++-string-lenght>)))
           (literal-string-regexp
            (concat "\\(" prefix-regexp "?\\)\\(" simple-string-regexp "\\)\\(s?\\)"))
           (delimiter-group-regexp "\\([^\\s-\\\\()]\\{1,16\\}\\)")
           (raw-delimiter-literal-string-regexp
            (concat "\\(" prefix-regexp "?" raw
                    "\"" delimiter-group-regexp "(\\)"
                    "\\(\\(.\\|\n\\)*?\\)"
                    "\\()\\2\"s?\\)"))
           (raw-literal-string-regexp
            (concat "\\(" prefix-regexp "?" raw "\"(\\)"
                    "\\(\\(.\\|\n\\)*?\\)"
                    "\\()\"s?\\)")))
      (setq modern-c++-font-lock-literal-string
            `(
              ;; Note: order below matters, because once colored, that part
              ;; won't change. In general, longer words first
              (,raw-delimiter-literal-string-regexp (1 font-lock-constant-face)
                                                    (3 font-lock-string-face)
                                                    (5 font-lock-constant-face))
              (,raw-literal-string-regexp (1 font-lock-constant-face)
                                          (2 font-lock-string-face)
                                          (4 font-lock-constant-face))
              (,literal-string-regexp (1 font-lock-constant-face)
                                      (2 font-lock-string-face)
                                      (3 font-lock-constant-face)))))))

(defcustom modern-c++-stl-cstdint
  t
  "Enable font-lock for header <cstdint>. For more information,
see documentation:
http://en.cppreference.com/w/cpp/header/cstdint"
  :type 'boolean
  :group 'modern-c++-font-lock)

(defvar modern-c++-font-lock-stl-cstdint nil)

(defun modern-c++-generate-font-lock-stl-cstdint ()
  (let ((stl-cstdint-types (regexp-opt
                            (eval-when-compile
                              (sort (mapcar (function (lambda (x) (concat "std::" x)))
                                            '("int8_t" "int16_t" "int32_t" "int64_t" "int_fast8_t" "int_fast16_t" "int_fast32_t" "int_fast64_t" "int_least8_t" "int_least16_t" "int_least32_t" "int_least64_t" "intmax_t" "intptr_t" "uint8_t" "uint16_t" "uint32_t" "uint64_t" "uint_fast8_t" "uint_fast16_t" "uint_fast32_t" "uint_fast64_t" "uint_least8_t" "uint_least16_t" "uint_least32_t" "uint_least64_t" "uintmax_t" "uintptr_t"))
                                    'modern-c++-string-lenght>))
                            'symbols))
        (stl-cstdint-macro (regexp-opt
                            (eval-when-compile
                              (sort '("INT8_MIN" "INT16_MIN" "INT32_MIN" "INT64_MIN" "INT_FAST8_MIN" "INT_FAST16_MIN" "INT_FAST32_MIN" "INT_FAST64_MIN" "INT_LEAST8_MIN" "INT_LEAST16_MIN" "INT_LEAST32_MIN" "INT_LEAST64_MIN" "INTPTR_MIN" "INTMAX_MIN" "INT8_MAX" "INT16_MAX" "INT32_MAX" "INT64_MAX" "INT_FAST8_MAX" "INT_FAST16_MAX" "INT_FAST32_MAX" "INT_FAST64_MAX" "INT_LEAST8_MAX" "INT_LEAST16_MAX" "INT_LEAST32_MAX" "INT_LEAST64_MAX" "INTPTR_MAX" "INTMAX_MAX" "UINT8_MAX" "UINT16_MAX" "UINT32_MAX" "UINT64_MAX" "UINT_FAST8_MAX" "UINT_FAST16_MAX" "UINT_FAST32_MAX" "UINT_FAST64_MAX" "UINT_LEAST8_MAX" "UINT_LEAST16_MAX" "UINT_LEAST32_MAX" "UINT_LEAST64_MAX" "UINTPTR_MAX" "UINTMAX_MAX" "INT8_C" "INT16_C" "INT32_C" "INT64_C" "INTMAX_C" "UINT8_C" "UINT16_C" "UINT32_C" "UINT64_C" "UINTMAX_C" "PTRDIFF_MIN" "PTRDIFF_MAX" "SIZE_MAX" "SIG_ATOMIC_MIN" "SIG_ATOMIC_MAX" "WCHAR_MIN" "WCHAR_MAX" "WINT_MIN" "WINT_MAX")
                                    'modern-c++-string-lenght>))
                            'symbols)))
    (setq modern-c++-font-lock-stl-cstdint
          `(
            ;; Note: order below matters, because once colored, that part
            ;; won't change. In general, longer words first
            (,stl-cstdint-types (0 font-lock-type-face))
            (,stl-cstdint-macro (0 font-lock-preprocessor-face))))))

(defun modern-c++-font-lock-add-keywords (&optional mode)
  "Install keywords into major MODE, or into current buffer if nil."
  (font-lock-add-keywords mode (modern-c++-generate-font-lock-keywords) nil)
  (when modern-c++-literal-boolean
    (font-lock-add-keywords mode (modern-c++-generate-font-lock-literal-boolean) nil))
  (when modern-c++-literal-integer
    (font-lock-add-keywords mode (modern-c++-generate-font-lock-literal-integer) nil))
  (when modern-c++-literal-null-pointer
    (font-lock-add-keywords mode (modern-c++-generate-font-lock-literal-null-pointer) nil))
  (when modern-c++-literal-string
    (font-lock-add-keywords mode (modern-c++-generate-font-lock-literal-string) nil))
  (when modern-c++-stl-cstdint
    (font-lock-add-keywords mode (modern-c++-generate-font-lock-stl-cstdint) nil)))

(defun modern-c++-font-lock-remove-keywords (&optional mode)
  "Remove keywords from major MODE, or from current buffer if nil."
  (font-lock-remove-keywords mode modern-c++-font-lock-keywords)
  (when modern-c++-literal-boolean
    (font-lock-remove-keywords mode modern-c++-font-lock-literal-boolean))
  (when modern-c++-literal-integer
    (font-lock-remove-keywords mode modern-c++-font-lock-literal-integer))
  (when modern-c++-literal-null-pointer
    (font-lock-remove-keywords mode modern-c++-font-lock-literal-null-pointer))
  (when modern-c++-literal-string
    (font-lock-remove-keywords mode modern-c++-font-lock-literal-string))
  (when modern-c++-stl-cstdint
    (font-lock-remove-keywords mode modern-c++-font-lock-stl-cstdint)))

;;;###autoload
(define-minor-mode modern-c++-font-lock-mode
  "Provides font-locking as a Minor Mode for Modern C++"
  :init-value nil
  :lighter " mc++fl"
  :group 'modern-c++-font-lock
  (if modern-c++-font-lock-mode
      (modern-c++-font-lock-add-keywords)
    (modern-c++-font-lock-remove-keywords))
  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

;;;###autoload
(define-global-minor-mode modern-c++-font-lock-global-mode modern-c++-font-lock-mode
  (lambda ()
    (when (apply 'derived-mode-p '(c++-mode))
      (modern-c++-font-lock-mode 1)))
  :group 'modern-c++-font-lock)

(provide 'modern-cpp-font-lock)

;; coding: utf-8

;;; modern-cpp-font-lock.el ends here
