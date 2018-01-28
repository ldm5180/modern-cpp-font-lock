;;; gtest-cpp-font-lock.el --- Font-locking for "GTest and GMock"  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defgroup gtest-c++-font-lock nil
  "Provides font-locking as a Minor Mode for GTest and GMock"
  :group 'faces)

(eval-and-compile
  (defun gtest-c++-string-length< (a b) (< (length a) (length b)))
  (defun gtest-c++-string-length> (a b) (not (gtest-c++-string-length< a b))))

(defcustom gtest-c++-keywords
  (eval-when-compile
    (sort '("TEST" "TEST_F" "TEST_P" "INSTANTIATE_TEST_CASE_P" "TYPED_TEST" "TYPED_TEST_CASE" "RUN_ALL_TESTS" "GTEST_FLAG" "GTEST_SHARD_INDEX" "GTEST_TOTAL_SHARDS" "SUCCEED" "FAIL" "ADD_FAILURE" "ADD_FAILURE_AT" "SCOPED_TRACE" "ASSERT_THROW" "ASSERT_ANY_THROW" "ASSERT_NO_THROW" "ASSERT_FLOAT_EQ" "ASSERT_DOUBLE_EQ" "ASSERT_NEAR" "ASSERT_DEATH" "ASSERT_DEATH_IF_SUPPORTED" "ASSERT_EXIT" "ASSERT_TRUE" "ASSERT_FALSE" "ASSERT_EQ" "ASSERT_LT" "ASSERT_LE" "ASSERT_GT" "ASSERT_GE" "ASSERT_STREQ" "ASSERT_STRNE" "ASSERT_STRCASEEQ" "ASSERT_STRCASENE" "ASSERT_NO_FATAL_FAILURE" "EXPECT_THROW" "EXPECT_ANY_THROW" "EXPECT_NO_THROW" "EXPECT_FLOAT_EQ" "EXPECT_DOUBLE_EQ" "EXPECT_NEAR" "EXPECT_DEATH" "EXPECT_DEATH_IF_SUPPORTED" "EXPECT_EXIT" "EXPECT_TRUE" "EXPECT_FALSE" "EXPECT_EQ" "EXPECT_LT" "EXPECT_LE" "EXPECT_GT" "EXPECT_GE" "EXPECT_STREQ" "EXPECT_STRNE" "EXPECT_STRCASEEQ" "EXPECT_STRCASENE" "EXPECT_NO_FATAL_FAILURE" "GTEST" "SHOULD" "DISABLED_SHOULD" "ASSERT_NE" "EXPECT_NE" "EXPECT_CALL" )
          'gtest-c++-string-length>))
  "List of C++ keywords. See doc:
http://en.cppreference.com/w/cpp/keyword"
  :type '(choice (const :tag "Disabled" nil)
                 (repeat string))
  :group 'gtest-c++-font-lock)


(defvar gtest-c++-font-lock-keywords nil)

(defun gtest-c++-generate-font-lock-keywords ()
  (let ((keywords-regexp (regexp-opt gtest-c++-keywords 'words)))
    (setq gtest-c++-font-lock-keywords
          `(
            ;; Note: order below matters, because once colored, that part
            ;; won't change. In general, longer words first
            (,keywords-regexp (0 font-lock-keyword-face))))))

(defun gtest-c++-font-lock-add-keywords (&optional mode)
  "Install keywords into major MODE, or into current buffer if nil."
  (font-lock-add-keywords mode (gtest-c++-generate-font-lock-keywords) nil))

(defun gtest-c++-font-lock-remove-keywords (&optional mode)
  "Remove keywords from major MODE, or from current buffer if nil."
  (font-lock-remove-keywords mode gtest-c++-font-lock-keywords))

;;;###autoload
(define-minor-mode gtest-c++-font-lock-mode
  "Provides font-locking as a Minor Mode for GTest and GMock"
  :init-value nil
  :lighter " gtest"
  :group 'gtest-c++-font-lock
  (if gtest-c++-font-lock-mode
      (gtest-c++-font-lock-add-keywords)
    (gtest-c++-font-lock-remove-keywords))
  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

;;;###autoload
(define-global-minor-mode gtest-c++-font-lock-global-mode gtest-c++-font-lock-mode
  (lambda ()
    (when (apply 'derived-mode-p '(c++-mode))
      (gtest-c++-font-lock-mode 1)))
  :group 'gtest-c++-font-lock)

(provide 'gtest-cpp-font-lock)

;; coding: utf-8

;;; gtest-cpp-font-lock.el ends here
