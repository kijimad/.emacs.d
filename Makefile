.DEFAULT_GOAL := help

gen-html: ## literate orgから、HTMLファイルを生成する
	@emacs --batch -l ./publish.el --funcall kd/publish

gen-el: ## literate orgから、emacs lispファイルを出力する
	@emacs --batch -l ./publish.el --funcall kd/gen-el && echo success🎉

help: ## ヘルプを表示する
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
