(require 'org)
(require 'org-element)

(defun mhb-ai-special-block ()
  "如果实在 #+begin_mai ... #+end_mai block 中，返回 cnotext，否则 nil"
  (cl-loop with ctx = (org-element-context)
	   while (and ctx
		      (not (and (equal 'special-block (org-element-type ctx))
				(equal "mai" (org-element-property :type ctx)))))
	   do (setq ctx (org-element-property :parent ctx))
	   finally return ctx))

(defun mhb-ai-get-block-head (&optional ctx)
  "提取 #+begin_mai ... #+end_mai block 的 block heading 参数, list of key-value pairs.
@ctx 是整个 #+begin_mai ... #+end_mai block context"
  (let* ((ctx (or ctx (mhb-ai-special-block)))
	 (head-start (org-element-property :post-affiliated ctx))
	 (head-end (org-element-property :contents-begin ctx)))
    (if (or (not head-start) (not head-end))
	(error "找不到 special block heading，不是在 #+begin_xxx ... #+end_xxx block 中调用的？")
      (save-match-data
	(let* ((hdstring (string-trim (buffer-substring-no-properties head-start head-end)))
	       (hdstring (string-trim-left (string-replace "#+begin_mai" "" hdstring))))
	  (org-babel-parse-header-arguments hdstring))))))

(defun mhb-ai-get-block-content (&optional ctx)
  "提取 #+begin_mai ... #end_mai block 中的 content，封装成 list of dictionary, 用于 API 调用"
  (let* ((ctx (or ctx (mhb-ai-special-block)))
	 (content-start (org-element-property :contents-begin ctx))
	 (content-end (org-element-property :contents-end ctx))
	 (raw-content (string-trim (buffer-substring-no-properties content-start content-end))))
    (print raw-content)
    (with-temp-buffer
      (erase-buffer)
      (insert raw-content)
      (goto-char (point-min))
      (let* (;; [MHB]: [AI]: 标记的位置们，TODO：增加 [SYS] 等 api 支持的其他 role
	     (split-markers (cl-loop while (search-forward-regexp "\\[MHB\\]:\\|\\[AI\\]:" nil t)
				     collect (save-excursion
					       (goto-char (match-beginning 0))
					       (point))))
	     ;; 把 point-min point-max 加进去，视情况
	     (split-markers (if (not split-markers)
				(list (point-min) (point-max))
			      (progn
				(if (not (equal 1 (car split-markers)))
				    `(1 ,@split-markers ,(point-max))
				  `(,@split-markers ,(point-max))))))
	     ;; 根据 @split-markers 把 @raw-content 分割成 parts，每个 part 都是以 [MHB]: 或 [AI]: 开头
	     (parts (cl-loop for (start end) on split-markers by #'cdr
			     collect (string-trim (buffer-substring-no-properties start (or end (point-max))))))
	     ;; 如果 raw-content 开头不是 [MHB]: 或 [AI]: 等，修改增加前缀 [MHB]:
	     (parts (if (not (equal 0 (car split-markers)))
			(progn
			  (setf (car parts) (concat "[MHB]: " (car parts)))
			  parts)
		      parts))
	     ;; 构造 association list，用于传给 json
	     (messages (cl-loop for part in parts
				collect (cl-destructuring-bind (type &rest content) (split-string part ":")
					  (let* ((type (string-trim type))
						 (content (string-trim (string-join content ":")))
						 ;; (content (encode-coding-string (string-trim (string-join content ":")) 'utf-8))
						 )
					    (list :role (cond
							 ((string= "[MHB]" type) 'user)
							 ((string= "[AI]" type) 'assistant)
							 (t 'assistant))
						  :content content)))))
	     ;; 删除 content 为空的 message
	     (messages (cl-remove-if (lambda (x) (string-empty-p (plist-get x :content))) messages)))
	(apply #'vector messages)
	(print (apply #'vector messages))))))

(provide 'mhb-ai-block)
