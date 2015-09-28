(use-package fiplr
  :config
  (setq fiplr-root-markers '(".git"))
  (setq fiplr-ignored-globs
    '((directories (
        ".git"
        "node_modules"
        "bower_components"
        "__pycache__"
      )) (files (
        "*.so"
        "*.o"
        "*.obj"
        "*.jpg"
        "*.png"
        "*.gif"
        "*.pdf"
        "*.gz"
        "*.zip"
        "*.7z"
      ))))
)
