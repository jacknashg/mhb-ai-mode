# mhb-ai-mode
interact with 01.ai LLM from within Emacs, heavily inspired by org-ai.

在 Emacs 里和“01万物”大模型（<https://platform.lingyiwanwu.com/>) 交互。

# 依赖
1. Emacs packages:
	```emacs-lisp
	(require 'json)
	(require 'org)
	(require 'org-element)
	(require 'auth-source) ;; 用于从 ~/.auth 文件取 apikey
	```
2. 01万物大模型 apkkey（在这里申请：<https://platform.lingyiwanwu.com/> ）

# 咋用
1. 项目克隆到本地（比如 /tmp/mhb-ai-mode）
2. 修改 Emacs init 文件，增加从 /tmp/mhb-ai-mode 加载文件
	```emacs-lsp
	(use-package mhb-ai
		:load-path "/Users/nashjackmu/wolfman/scripts/mhb-ai-20240321.001"
		:init
		(add-hook 'org-mode-hook #'mhb-ai-mode)
		:config
		(message "mhb-ai loaded"))
	```
3. 打开一个 org 文件，插入 #+begin_mai ... #+end_mai block，在 block 里写问题，C-c C-c 调用 API，AI 的回答会写到 block 里，就像这样：
	<https://jacknashg.github.io/assets/imgs/240321syxg.gif>
	
	
# 说明
  在 Mac 上运行成功。Windows 和 Linux 上没有试过，但是由于代码足够简单，应该能够运行。



