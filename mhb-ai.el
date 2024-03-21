(require 'json)
(require 'org)
(require 'org-element)
(require 'mhb-ai-block)
(require 'auth-source)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= mode 定义
(defvar mhb-ai-mode-map (make-sparse-keymap)
  "Keymap for `mhb-ai-mode")

(define-minor-mode mhb-ai-mode
  "Minor mode for `org-mode integrating with mhb ai facilities"
  :init-value nil
  :lighter " mhb-ai"
  :keymap mhb-ai-mode-map
  :group 'mhb-ai
  (add-hook 'org-ctrl-c-ctrl-c-hook #'mhb-ai-ctrl-c-ctrl-c -10 t))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 变量定义

(defvar mhb-ai--api-endpoint "https://api.lingyiwanwu.com/v1/chat/completions"
  "mhb-ai 模块调用 remote API 的地址")

(defvar mhb-ai--write-rsp-callback nil
  "url-retrieve 返回的json，解析出 delta strin 后的回调函数。写入到特定位置")

(defvar mhb-ai--current-special-block nil
  "当前操作的 special block")

(defvar mhb-ai--current-special-block-buffer nil
  "当前操作的 special block 所在的 buffer")


(defvar mhb-ai--output-buffer nil
  "remote API 返回的回答内容写到哪里")

(defvar mhb-ai--url-request-buffer-for-stream nil
  "当前 url-retrieve 绑定到的 buffer，url-retrieve 获取到的 response stream 写入到这个 buffer 中")

(defvar mhb-ai--url-request-buffer-last-marker nil
  "记录最后一次 read 处理截止的位置，新一次read该从这里开始")

(defvar mhb-ai--response-insert-marker nil
  "remote API 异步返回的内容该写到哪里")

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 函数定义
;;;###autoload
(defun mhb-ai-ctrl-c-ctrl-c ()
  "在 #+begin_mai ... #+end_mai block 中执行 C-c C-c 时触发"
  (when-let ((ctx (mhb-ai-special-block)))
    (mhb-ai-complete-block)
    t))

(defun mhb-ai-complete-block ()
  "cursor 在 #+begin_mai ... #+end_mai block 中，解析 block heading，block content，fire API requesting"
  (let* ((ctx (mhb-ai-special-block))
	 (buffer (current-buffer))
	 ;; blockhead 是一个 association list
	 (blockhead (mhb-ai-get-block-head ctx))
	 ;; messages 是一个 array
	 (messages (mhb-ai-get-block-content ctx))
	 (callback (lambda (deltastr role)
		     (mhb-ai--write-response deltastr role)
		     ;; (funcall (mhb-ai--write-response-2ed-order ctx buffer) deltastr)
		     )))
    ;; 设置全局变量
    ;; special block context
    (setq mhb-ai--current-special-block ctx)
    ;; buffer
    (setq mhb-ai--current-special-block-buffer buffer)
    ;; 回调函数
    (setq mhb-ai--write-rsp-callback callback)
    (mhb-ai-stream-fire blockhead messages)))

(defun mhb-ai-stream-fire (blockhead messages)
  "调用remote API，stream 式调用. @ctx 是 special-block, @buffer 是 special-block 所在的 buffer"
  (let* ((url-request-method "POST")
	 ;; apikey
	 (apikey (let ((credentials (auth-source-search :host "api.lingyiwanwu.com"
							:user "apikey"
							:require '(:secret))))
		   (funcall (plist-get (car credentials) :secret))))
	 (authstr (encode-coding-string (format "Bearer %s" apikey) 'utf-8))
	 (url-request-extra-headers `(("Content-Type" . "application/json")
	 			      ("Authorization" . ,authstr)))
	 (jsonencoded (json-encode `((stream . "true")
				     (temperature . 0.7)
				     (model . ,(cdr (assoc :model blockhead)))
				     (messages . ,messages))))
	 (url-request-data
	  (encode-coding-string
	   jsonencoded
	   'utf-8)))
    ;; 通过 url-retrieve 异步调用，绑定的接收 response 的 buffer 绑定到变量 mhb-ai--url-request-buffer-for-stream
    ;; 不使用 proxy
    (setq mhb-ai-original-url-proxy-services url-proxy-services)
    (setq url-proxy-services nil)
    (setq mhb-ai--url-request-buffer-for-stream
	  (url-retrieve mhb-ai--api-endpoint
			(lambda (_events)
			  ;; 执行完了，reset stream
			  (mhb-ai-reset-stream-state)
			  ;; reset proxy
			  (setq url-proxy-services mhb-ai-original-url-proxy-services)
			  )))
    ;; 异步 url-retrieve 已经开始，配置 stream 处理逻辑，启动
    (mhb-ai-set-stream-state)))


(defun mhb-ai--help-get-content-from-rsp-json-string (jsb &optional key)
  "从 json string 中提取 content, @jsb 是 json object "
  (let* ((key (or key 'content))
	 (choices (or (alist-get 'choices jsb)
		      (plist-get jsb 'choices)))
         ;; (first-choice (car choices))
					; Assuming we need the first item in "choices"
	 (first-choice (and (arrayp choices) (> (length choices) 0) (aref choices 0)))
         (delta (plist-get first-choice 'delta))
         (result (plist-get delta key)))
    (if result
	(decode-coding-string result 'utf-8)
      nil)
    ))


(defun mhb-ai--write-response (str role)
  "把 remote API 异步 stream 返回的内容写到 buffer 中。@str 是已经解析得到的 delta string. @role 是解析到的 role"
  (if (and (or str role) mhb-ai--current-special-block mhb-ai--current-special-block-buffer)
      (let* (;; 确定插入位子
	   (insert-pos (or (and mhb-ai--response-insert-marker
				(marker-position mhb-ai--response-insert-marker))
			   (org-element-property :contents-end mhb-ai--current-special-block))))
      (save-excursion
	(with-current-buffer mhb-ai--current-special-block-buffer      
	  (goto-char insert-pos)
	  ;; 保证 #+end_mai 在单独的一行
	  (when (string-prefix-p "#+end_mai" (buffer-substring-no-properties (point) (line-end-position)))
	    (insert "\n")
	    (backward-char))
	  ;; 插入 role 相关的头
	  (if role
	      (print role))
	  (insert (cond
		   ((string= role "assistant") "\n\n[AI]: ")
		   ((string= role "user") "\n\n[MHB]: \n")
		   ((string= role "system") "\n\n[SYS]: ")
		   (t "")))
	  (if (string= role "user")
	      (backward-char))
	  ;; 插入 content 相关
	  (if str
	      (insert str))
	  (setq mhb-ai--response-insert-marker (point-marker))
	  )))))

(defun mhb-ai--url-request-buffer-onchange-callback (_beg _end _len)
  "url-retrieve 绑定的 buffer，内容变化时的 callback。这个函数绑定到 after-change-functions hook，使得 buffer 出现变化时被调度执行"
  (save-excursion
    (with-current-buffer mhb-ai--url-request-buffer-for-stream
      (save-match-data      
	;; 回到之前的位置
	(if mhb-ai--url-request-buffer-last-marker
	    (goto-char (marker-position mhb-ai--url-request-buffer-last-marker))
	  (goto-char (point-min)))
	(let ((errored nil))
	  (while (and (not errored) (search-forward "data: " nil t))
	    (let* ((line (buffer-substring-no-properties (point) (line-end-position))))
	      (if (string= line "[DONE]")
		  (progn
		    ;; remote API finished responding
		    (funcall mhb-ai--write-rsp-callback "" "user")
		    (end-of-line)
		    (setq mhb-ai--url-request-buffer-last-marker (point-marker)))
		;; 尝试解析 json，如果失败就放弃，可能是因为本次 buffer change 并不完整，下次出现 buffer change 时再重来。能重来，是因为 mhb-ai--url-buffer-last-position-marker 没变
		(let ((json-object-type 'plist)
		      (json-key-type 'symbol)
		      (json-array-type 'vector))
		  (condition-case _err
		      (let* ((data (json-read-from-string line))
			     ;; extract "role" from remote API response
			     (role (mhb-ai--help-get-content-from-rsp-json-string data 'role))
			     ;; extract "content" from remote API response
			     (deltarsp (mhb-ai--help-get-content-from-rsp-json-string data 'content)))
			
			;; REAL AI RESPONSE HANDLE LOGIC GOES HERE. real business logic goes here, e.g. insert 'data to custom buffer
			(if (and (or role deltarsp) mhb-ai--write-rsp-callback)
			    (funcall mhb-ai--write-rsp-callback deltarsp role))
			;; 移动到行尾
			(end-of-line)
			;; 设置 marker，下次从这里开始，避免数据解析重复
			(setq mhb-ai--url-request-buffer-last-marker (point-marker)))
		    (error
		     ;; (print _err)
		     (setq errored t))))))))))))

(defun mhb-ai-reset-stream-state ()
  "url-retrieve interact with remote API, reset stream handling logic"
  ;; 清空 after-change-functions hook 的绑定，这个绑定是 buffer specific 的
  (when (and mhb-ai--url-request-buffer-for-stream (buffer-live-p mhb-ai--url-request-buffer-for-stream))
    (with-current-buffer mhb-ai--url-request-buffer-for-stream
      (remove-hook 'after-change-functions #'mhb-ai--url-request-stream-onchange-callback)))
  ;; 清空回调函数
  (setq mhb-ai--write-rsp-callback nil)
  ;; 清空 context
  (setq mhb-ai--current-special-block nil)
  ;; 清空 buffer
  (setq mhb-ai--current-special-block-buffer nil)
  ;; 重置 request buffer 处理的 position
  (setq mhb-ai--url-request-buffer-last-marker nil)
  ;; 重置 write response 的 marker
  (setq mhb-ai--response-insert-marker nil)
  )

(defun mhb-ai-set-stream-state ()
  "url-retrieve interact with remote API, set stream handling logic"
  (unless (member 'mhb-ai--url-request-buffer-onchange-callback after-change-functions)
    (with-current-buffer mhb-ai--url-request-buffer-for-stream
      (add-hook 'after-change-functions #'mhb-ai--url-request-buffer-onchange-callback nil t))))


(provide 'mhb-ai)
