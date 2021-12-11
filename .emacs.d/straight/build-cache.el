
:tanat

"28.0.50"

#s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("org-elpa" ("2021-12-11 13:20:20" nil (:local-repo nil :package "org-elpa" :type git)) "melpa" ("2021-12-11 13:20:21" nil (:type git :host github :repo "melpa/melpa" :build nil :package "melpa" :local-repo "melpa")) "gnu-elpa-mirror" ("2021-12-11 13:20:21" nil (:type git :host github :repo "emacs-straight/gnu-elpa-mirror" :build nil :package "gnu-elpa-mirror" :local-repo "gnu-elpa-mirror")) "el-get" ("2021-12-11 13:20:21" nil (:type git :host github :repo "dimitri/el-get" :build nil :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :flavor melpa :package "el-get" :local-repo "el-get")) "emacsmirror-mirror" ("2021-12-11 13:20:21" nil (:type git :host github :repo "emacs-straight/emacsmirror-mirror" :build nil :package "emacsmirror-mirror" :local-repo "emacsmirror-mirror")) "straight" ("2021-12-11 13:20:21" ("emacs") (:type git :host github :repo "raxod502/straight.el" :files ("straight*.el") :branch "master" :package "straight" :local-repo "straight.el")) "use-package" ("2021-12-11 13:20:21" ("emacs" "bind-key") (:type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package" :package "use-package" :local-repo "use-package")) "bind-key" ("2021-12-11 13:20:21" nil (:flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :package "bind-key" :local-repo "use-package" :type git :repo "jwiegley/use-package" :host github)) "yk-util" ("2021-12-11 13:20:21" ("emacs" "dash") (:type git :host github :repo "y-kitamu/yk_elisp" :package "yk-util" :local-repo "yk_elisp")) "dash" ("2021-12-11 13:20:22" ("emacs") (:type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el" :package "dash" :local-repo "dash.el")) "magit" ("2021-12-11 13:20:22" ("emacs" "dash" "git-commit" "magit-section" "transient" "with-editor") (:type git :flavor melpa :files ("lisp/magit" "lisp/magit*.el" "lisp/git-rebase.el" "Documentation/magit.texi" "Documentation/AUTHORS.md" "LICENSE" (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el") "magit-pkg.el") :host github :repo "magit/magit" :package "magit" :local-repo "magit")) "git-commit" ("2021-12-11 13:20:22" ("emacs" "dash" "transient" "with-editor") (:flavor melpa :files ("lisp/git-commit.el" "lisp/git-commit-pkg.el" "git-commit-pkg.el") :package "git-commit" :local-repo "magit" :type git :repo "magit/magit" :host github)) "transient" ("2021-12-11 13:20:22" ("emacs") (:type git :flavor melpa :files ("lisp/*.el" "docs/transient.texi" "transient-pkg.el") :host github :repo "magit/transient" :package "transient" :local-repo "transient")) "with-editor" ("2021-12-11 13:20:22" ("emacs") (:type git :flavor melpa :host github :repo "magit/with-editor" :package "with-editor" :local-repo "with-editor")) "magit-section" ("2021-12-11 13:20:22" ("emacs" "dash") (:flavor melpa :files ("lisp/magit-section.el" "lisp/magit-section-pkg.el" "Documentation/magit-section.texi" "magit-section-pkg.el") :package "magit-section" :local-repo "magit" :type git :repo "magit/magit" :host github)) "helm-ls-git" ("2021-12-11 13:20:22" ("helm") (:type git :flavor melpa :host github :repo "emacs-helm/helm-ls-git" :package "helm-ls-git" :local-repo "helm-ls-git")) "helm" ("2021-12-11 13:20:22" ("emacs" "async" "popup" "helm-core") (:type git :flavor melpa :files ("*.el" "emacs-helm.sh" (:exclude "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") "helm-pkg.el") :host github :repo "emacs-helm/helm" :package "helm" :local-repo "helm")) "async" ("2021-12-11 13:20:22" ("emacs") (:type git :flavor melpa :host github :repo "jwiegley/emacs-async" :package "async" :local-repo "emacs-async")) "popup" ("2021-12-11 13:20:22" ("emacs") (:type git :flavor melpa :files ("popup.el" "popup-pkg.el") :host github :repo "auto-complete/popup-el" :package "popup" :local-repo "popup-el")) "helm-core" ("2021-12-11 13:20:22" ("emacs" "async") (:flavor melpa :files ("helm-core-pkg.el" "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") :package "helm-core" :local-repo "helm" :type git :repo "emacs-helm/helm" :host github)) "git-gutter" ("2021-12-11 13:20:22" ("emacs") (:type git :flavor melpa :host github :repo "emacsorphanage/git-gutter" :package "git-gutter" :local-repo "git-gutter")) "projectile" ("2021-12-11 13:20:22" ("emacs") (:type git :flavor melpa :files ("projectile.el" "projectile-pkg.el") :host github :repo "bbatsov/projectile" :package "projectile" :local-repo "projectile")) "helm-projectile" ("2021-12-11 13:20:22" ("helm" "projectile" "cl-lib") (:type git :flavor melpa :host github :repo "bbatsov/helm-projectile" :package "helm-projectile" :local-repo "helm-projectile")) "org" ("2021-12-11 13:20:22" ("emacs") (:type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :local-repo "org" :depth full :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*")) :package "org")) "open-junk-file" ("2021-12-11 13:20:22" nil (:type git :flavor melpa :host github :repo "rubikitch/open-junk-file" :package "open-junk-file" :local-repo "open-junk-file")) "lispxmp" ("2021-12-11 13:20:22" nil (:type git :flavor melpa :host github :repo "rubikitch/lispxmp" :package "lispxmp" :local-repo "lispxmp")) "paredit" ("2021-12-11 13:20:22" nil (:type git :flavor melpa :files ("paredit.el" "paredit-pkg.el") :repo "https://mumble.net/~campbell/git/paredit.git" :package "paredit" :local-repo "paredit")) "auto-async-byte-compile" ("2021-12-11 13:20:22" nil (:type git :flavor melpa :host github :repo "rubikitch/auto-async-byte-compile" :package "auto-async-byte-compile" :local-repo "auto-async-byte-compile")) "powershell" ("2021-12-11 13:20:22" ("emacs") (:type git :flavor melpa :host github :repo "jschaf/powershell.el" :package "powershell" :local-repo "powershell.el")) "dockerfile-mode" ("2021-12-11 13:20:22" ("emacs") (:type git :flavor melpa :host github :repo "spotify/dockerfile-mode" :package "dockerfile-mode" :local-repo "dockerfile-mode")) "docker-compose-mode" ("2021-12-11 13:20:22" ("emacs" "dash" "yaml-mode") (:type git :flavor melpa :files (:defaults (:exclude "docker-compose-mode-helpers.el") "docker-compose-mode-pkg.el") :host github :repo "meqif/docker-compose-mode" :package "docker-compose-mode" :local-repo "docker-compose-mode")) "yaml-mode" ("2021-12-11 13:20:22" ("emacs") (:type git :flavor melpa :host github :repo "yoshiki/yaml-mode" :package "yaml-mode" :local-repo "yaml-mode")) "cmake-mode" ("2021-12-11 13:20:22" ("emacs") (:type git :flavor melpa :files ("Auxiliary/*.el" "cmake-mode-pkg.el") :repo "https://gitlab.kitware.com/cmake/cmake.git" :package "cmake-mode" :local-repo "cmake")) "protobuf-mode" ("2021-12-11 13:20:23" nil (:type git :flavor melpa :files ("editors/protobuf-mode.el" "protobuf-mode-pkg.el") :host github :repo "google/protobuf" :package "protobuf-mode" :local-repo "protobuf")) "csv-mode" ("2021-12-11 13:20:23" ("emacs" "cl-lib") (:type git :host github :repo "emacs-straight/csv-mode" :files ("*" (:exclude ".git")) :package "csv-mode" :local-repo "csv-mode")) "go-mode" ("2021-12-11 13:20:23" nil (:type git :flavor melpa :files ("go-mode.el" "go-mode-pkg.el") :host github :repo "dominikh/go-mode.el" :package "go-mode" :local-repo "go-mode.el")) "typescript-mode" ("2021-12-11 13:20:23" ("emacs") (:type git :flavor melpa :host github :repo "emacs-typescript/typescript.el" :package "typescript-mode" :local-repo "typescript.el")) "cuda-mode" ("2021-12-11 13:20:23" nil (:type git :flavor melpa :host github :repo "chachi/cuda-mode" :package "cuda-mode" :local-repo "cuda-mode")) "rust-mode" ("2021-12-11 13:20:23" ("emacs") (:type git :flavor melpa :host github :repo "rust-lang/rust-mode" :package "rust-mode" :local-repo "rust-mode")) "cargo" ("2021-12-11 13:20:23" ("emacs" "markdown-mode") (:type git :flavor melpa :host github :repo "kwrooijen/cargo.el" :package "cargo" :local-repo "cargo.el")) "markdown-mode" ("2021-12-11 13:20:23" ("emacs") (:type git :flavor melpa :host github :repo "jrblevin/markdown-mode" :package "markdown-mode" :local-repo "markdown-mode")) "flycheck-rust" ("2021-12-11 13:20:23" ("emacs" "flycheck" "dash" "seq" "let-alist") (:type git :flavor melpa :host github :repo "flycheck/flycheck-rust" :package "flycheck-rust" :local-repo "flycheck-rust")) "flycheck" ("2021-12-11 13:20:23" ("dash" "pkg-info" "let-alist" "seq" "emacs") (:type git :flavor melpa :host github :repo "flycheck/flycheck" :package "flycheck" :local-repo "flycheck")) "pkg-info" ("2021-12-11 13:20:23" ("epl") (:type git :flavor melpa :host github :repo "emacsorphanage/pkg-info" :package "pkg-info" :local-repo "pkg-info")) "epl" ("2021-12-11 13:20:23" ("cl-lib") (:type git :flavor melpa :host github :repo "cask/epl" :package "epl" :local-repo "epl")) "let-alist" ("2021-12-11 13:20:23" ("emacs") (:type git :host github :repo "emacs-straight/let-alist" :files ("*" (:exclude ".git")) :package "let-alist" :local-repo "let-alist")) "web-mode" ("2021-12-11 13:20:23" ("emacs") (:type git :flavor melpa :host github :repo "fxbois/web-mode" :package "web-mode" :local-repo "web-mode")) "prettier-js" ("2021-12-11 13:20:23" nil (:type git :flavor melpa :host github :repo "prettier/prettier-emacs" :package "prettier-js" :local-repo "prettier-emacs")) "glsl-mode" ("2021-12-11 13:20:23" nil (:type git :flavor melpa :host github :repo "jimhourihan/glsl-mode" :package "glsl-mode" :local-repo "glsl-mode")) "clang-format+" ("2021-12-11 13:20:23" ("emacs" "clang-format") (:type git :flavor melpa :host github :repo "SavchenkoValeriy/emacs-clang-format-plus" :package "clang-format+" :local-repo "emacs-clang-format-plus")) "clang-format" ("2021-12-11 13:20:23" ("cl-lib") (:type git :flavor melpa :host github :repo "emacsmirror/clang-format" :package "clang-format" :local-repo "clang-format")) "rainbow-delimiters" ("2021-12-11 13:20:23" nil (:type git :flavor melpa :host github :repo "Fanael/rainbow-delimiters" :package "rainbow-delimiters" :local-repo "rainbow-delimiters")) "zenburn-theme" ("2021-12-11 13:20:22" nil (:type git :flavor melpa :host github :repo "bbatsov/zenburn-emacs" :package "zenburn-theme" :local-repo "zenburn-emacs")) "vline" ("2021-12-11 13:20:22" ("emacs") (:type git :flavor melpa :host github :repo "buzztaiki/vline" :package "vline" :local-repo "vline")) "col-highlight" ("2021-12-11 13:20:22" ("vline") (:type git :host github :repo "emacsmirror/col-highlight" :package "col-highlight" :local-repo "col-highlight")) "powerline" ("2021-12-11 13:20:22" ("cl-lib") (:type git :flavor melpa :host github :repo "milkypostman/powerline" :package "powerline" :local-repo "powerline")) "total-lines" ("2021-12-11 13:20:22" ("emacs") (:type git :flavor melpa :host github :repo "hinrik/total-lines" :package "total-lines" :local-repo "total-lines")) "cursor-chg" ("2021-12-11 13:20:22" nil (:type git :host github :repo "emacsmirror/cursor-chg" :package "cursor-chg" :local-repo "cursor-chg")) "ag" ("2021-12-11 13:20:23" ("dash" "s" "cl-lib") (:type git :flavor melpa :host github :repo "Wilfred/ag.el" :package "ag" :local-repo "ag.el")) "s" ("2021-12-11 13:20:23" nil (:type git :flavor melpa :files ("s.el" "s-pkg.el") :host github :repo "magnars/s.el" :package "s" :local-repo "s.el")) "wgrep-ag" ("2021-12-11 13:20:23" ("wgrep") (:type git :flavor melpa :files ("wgrep-ag.el" "wgrep-ag-pkg.el") :host github :repo "mhayashi1120/Emacs-wgrep" :package "wgrep-ag" :local-repo "Emacs-wgrep")) "wgrep" ("2021-12-11 13:20:23" nil (:flavor melpa :files ("wgrep.el" "wgrep-pkg.el") :package "wgrep" :local-repo "Emacs-wgrep" :type git :repo "mhayashi1120/Emacs-wgrep" :host github)) "undo-tree" ("2021-12-11 13:20:23" nil (:type git :host github :repo "emacs-straight/undo-tree" :files ("*" (:exclude ".git")) :package "undo-tree" :local-repo "undo-tree")) "buffer-move" ("2021-12-11 13:20:23" nil (:type git :flavor melpa :host github :repo "lukhas/buffer-move" :package "buffer-move" :local-repo "buffer-move")) "fast-scroll" ("2021-12-11 13:20:23" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "ahungry/fast-scroll" :package "fast-scroll" :local-repo "fast-scroll")) "edit-server" ("2021-12-11 13:20:23" nil (:type git :flavor melpa :files ("servers/edit-server.el" "edit-server-pkg.el") :host github :repo "stsquad/emacs_chrome" :package "edit-server" :local-repo "emacs_chrome")) "multi-term" ("2021-12-11 13:20:23" nil (:type git :flavor melpa :host github :repo "manateelazycat/multi-term" :package "multi-term" :local-repo "multi-term")) "gxref" ("2021-12-11 13:20:23" ("emacs") (:type git :flavor melpa :host github :repo "dedi/gxref" :package "gxref" :local-repo "gxref")) "eldoc" ("2021-12-11 13:20:23" ("emacs") (:type git :host github :repo "emacs-straight/eldoc" :files ("*" (:exclude ".git")) :package "eldoc" :local-repo "eldoc")) "ein" ("2021-12-11 13:20:23" ("emacs" "websocket" "anaphora" "request" "deferred" "polymode" "dash" "with-editor") (:type git :flavor melpa :files ("lisp/*" (:exclude "lisp/zeroein.el") "ein-pkg.el") :host github :repo "millejoh/emacs-ipython-notebook" :package "ein" :local-repo "emacs-ipython-notebook")) "websocket" ("2021-12-11 13:20:23" ("cl-lib") (:type git :flavor melpa :host github :repo "ahyatt/emacs-websocket" :package "websocket" :local-repo "emacs-websocket")) "anaphora" ("2021-12-11 13:20:23" nil (:type git :flavor melpa :host github :repo "rolandwalker/anaphora" :package "anaphora" :local-repo "anaphora")) "request" ("2021-12-11 13:20:23" ("emacs") (:type git :flavor melpa :files ("request.el" "request-pkg.el") :host github :repo "tkf/emacs-request" :package "request" :local-repo "emacs-request")) "deferred" ("2021-12-11 13:20:23" ("emacs") (:type git :flavor melpa :files ("deferred.el" "deferred-pkg.el") :host github :repo "kiwanami/emacs-deferred" :package "deferred" :local-repo "emacs-deferred")) "polymode" ("2021-12-11 13:20:23" ("emacs") (:type git :flavor melpa :host github :repo "polymode/polymode" :package "polymode" :local-repo "polymode")) "pdf-tools" ("2021-12-11 13:20:23" ("emacs" "tablist" "let-alist") (:type git :flavor melpa :files ("lisp/*.el" "README" ("build" "Makefile") ("build" "server") (:exclude "lisp/tablist.el" "lisp/tablist-filter.el") "pdf-tools-pkg.el") :host github :repo "vedang/pdf-tools" :package "pdf-tools" :local-repo "pdf-tools")) "tablist" ("2021-12-11 13:20:23" ("emacs") (:type git :flavor melpa :host github :repo "politza/tablist" :package "tablist" :local-repo "tablist")) "yatex" ("2021-12-11 13:20:23" nil (:type git :host github :repo "emacsmirror/yatex" :package "yatex" :local-repo "yatex")) "rainbow-mode" ("2021-12-11 13:20:23" nil (:type git :host github :repo "emacs-straight/rainbow-mode" :files ("*" (:exclude ".git")) :package "rainbow-mode" :local-repo "rainbow-mode")) "migemo" ("2021-12-11 13:20:23" ("cl-lib") (:type git :flavor melpa :host github :repo "emacs-jp/migemo" :package "migemo" :local-repo "migemo")) "google-translate" ("2021-12-11 13:20:23" ("emacs" "popup") (:type git :flavor melpa :host github :repo "atykhonov/google-translate" :package "google-translate" :local-repo "google-translate")) "avy" ("2021-12-11 13:20:23" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "abo-abo/avy" :package "avy" :local-repo "avy")) "helm-descbinds" ("2021-12-11 13:20:23" ("helm") (:type git :flavor melpa :host github :repo "emacs-helm/helm-descbinds" :package "helm-descbinds" :local-repo "helm-descbinds")) "helm-tramp" ("2021-12-11 13:20:23" ("emacs" "helm") (:type git :flavor melpa :host github :repo "masasam/emacs-helm-tramp" :package "helm-tramp" :local-repo "emacs-helm-tramp")) "helm-ag" ("2021-12-11 13:20:23" ("emacs" "helm") (:type git :flavor melpa :host github :repo "emacsorphanage/helm-ag" :package "helm-ag" :local-repo "helm-ag")) "docker" ("2021-12-11 13:20:23" ("dash" "docker-tramp" "emacs" "json-mode" "s" "tablist" "transient") (:type git :flavor melpa :host github :repo "Silex/docker.el" :package "docker" :local-repo "docker.el")) "docker-tramp" ("2021-12-11 13:20:23" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "emacs-pe/docker-tramp.el" :package "docker-tramp" :local-repo "docker-tramp.el")) "json-mode" ("2021-12-11 13:20:23" ("json-snatcher" "emacs") (:type git :flavor melpa :host github :repo "joshwnj/json-mode" :package "json-mode" :local-repo "json-mode")) "json-snatcher" ("2021-12-11 13:20:23" ("emacs") (:type git :flavor melpa :host github :repo "Sterlingg/json-snatcher" :package "json-snatcher" :local-repo "json-snatcher")) "hydra" ("2021-12-11 13:20:23" ("cl-lib" "lv") (:type git :flavor melpa :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :host github :repo "abo-abo/hydra" :package "hydra" :local-repo "hydra")) "lv" ("2021-12-11 13:20:23" nil (:flavor melpa :files ("lv.el" "lv-pkg.el") :package "lv" :local-repo "hydra" :type git :repo "abo-abo/hydra" :host github)) "yapfify" ("2021-12-11 13:20:23" nil (:type git :flavor melpa :host github :repo "JorisE/yapfify" :package "yapfify" :local-repo "yapfify")) "py-isort" ("2021-12-11 13:20:23" nil (:type git :flavor melpa :host github :repo "paetzke/py-isort.el" :package "py-isort" :local-repo "py-isort.el")) "ace-window" ("2021-12-11 13:20:23" ("avy") (:type git :flavor melpa :host github :repo "abo-abo/ace-window" :package "ace-window" :local-repo "ace-window")) "which-key" ("2021-12-11 13:20:23" ("emacs") (:type git :flavor melpa :host github :repo "justbur/emacs-which-key" :package "which-key" :local-repo "emacs-which-key")) "package-lint" ("2021-12-11 13:20:23" ("cl-lib" "emacs" "let-alist") (:type git :flavor melpa :files (:defaults "data" (:exclude "*flymake.el") "package-lint-pkg.el") :host github :repo "purcell/package-lint" :package "package-lint" :local-repo "package-lint")) "lsp-mode" ("2021-12-11 13:20:28" ("emacs" "dash" "f" "ht" "spinner" "markdown-mode" "lv") (:type git :flavor melpa :files (:defaults "clients/*.el" "lsp-mode-pkg.el") :host github :repo "emacs-lsp/lsp-mode" :package "lsp-mode" :local-repo "lsp-mode")) "f" ("2021-12-11 13:20:24" ("s" "dash") (:type git :flavor melpa :files ("f.el" "f-pkg.el") :host github :repo "rejeep/f.el" :package "f" :local-repo "f.el")) "ht" ("2021-12-11 13:20:24" ("dash") (:type git :flavor melpa :files ("ht.el" "ht-pkg.el") :host github :repo "Wilfred/ht.el" :package "ht" :local-repo "ht.el")) "spinner" ("2021-12-11 13:20:24" ("emacs") (:type git :host github :repo "emacs-straight/spinner" :files ("*" (:exclude ".git")) :package "spinner" :local-repo "spinner")) "helm-lsp" ("2021-12-11 13:20:28" ("emacs" "dash" "lsp-mode" "helm") (:type git :flavor melpa :host github :repo "emacs-lsp/helm-lsp" :package "helm-lsp" :local-repo "helm-lsp")) "init-loader" ("2021-12-11 13:20:22" ("cl-lib") (:type git :flavor melpa :files ("init-loader.el" "init-loader-pkg.el") :host github :repo "emacs-jp/init-loader" :package "init-loader" :local-repo "init-loader")) "lsp-docker+" ("2021-12-11 13:20:28" ("emacs" "dash" "lsp-mode" "lsp-docker") (:type git :host github :repo "y-kitamu/emacs-lsp-docker-plus" :package "lsp-docker+" :local-repo "emacs-lsp-docker-plus")) "lsp-docker" ("2021-12-11 13:20:28" ("emacs" "dash" "lsp-mode") (:type git :flavor melpa :host github :repo "emacs-lsp/lsp-docker" :package "lsp-docker" :local-repo "lsp-docker")) "lsp-ui" ("2021-12-11 13:20:29" ("emacs" "dash" "lsp-mode" "markdown-mode") (:type git :flavor melpa :files (:defaults "lsp-ui-doc.html" "resources" "lsp-ui-pkg.el") :host github :repo "emacs-lsp/lsp-ui" :package "lsp-ui" :local-repo "lsp-ui")) "lsp-pyright" ("2021-12-11 13:20:29" ("emacs" "lsp-mode" "dash" "ht") (:type git :flavor melpa :host github :repo "emacs-lsp/lsp-pyright" :package "lsp-pyright" :local-repo "lsp-pyright")) "ccls" ("2021-12-11 13:20:29" ("emacs" "lsp-mode" "dash") (:type git :flavor melpa :host github :repo "emacs-lsp/emacs-ccls" :package "ccls" :local-repo "emacs-ccls")) "yasnippet" ("2021-12-11 13:20:29" ("cl-lib") (:type git :flavor melpa :files ("yasnippet.el" "snippets" "yasnippet-pkg.el") :host github :repo "joaotavora/yasnippet" :package "yasnippet" :local-repo "yasnippet")) "company" ("2021-12-11 13:20:29" ("emacs") (:type git :flavor melpa :files (:defaults "icons" "company-pkg.el") :host github :repo "company-mode/company-mode" :package "company" :local-repo "company-mode")) "company-tabnine" ("2021-12-11 13:20:29" ("emacs" "company" "cl-lib" "dash" "s" "unicode-escape") (:type git :flavor melpa :host github :repo "TommyX12/company-tabnine" :package "company-tabnine" :local-repo "company-tabnine")) "unicode-escape" ("2021-12-11 13:20:29" ("emacs" "names" "dash") (:type git :flavor melpa :host github :repo "kosh04/unicode-escape.el" :package "unicode-escape" :local-repo "unicode-escape.el")) "names" ("2021-12-11 13:20:29" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "Malabarba/names" :package "names" :local-repo "names")) "dap-mode" ("2021-12-11 13:20:29" ("emacs" "dash" "lsp-mode" "bui" "f" "s" "lsp-treemacs" "posframe" "ht") (:type git :flavor melpa :files (:defaults "icons" "dap-mode-pkg.el") :host github :repo "emacs-lsp/dap-mode" :package "dap-mode" :local-repo "dap-mode")) "bui" ("2021-12-11 13:20:29" ("emacs" "dash") (:type git :flavor melpa :host github :repo "alezost/bui.el" :package "bui" :local-repo "bui.el")) "lsp-treemacs" ("2021-12-11 13:20:29" ("emacs" "dash" "f" "ht" "treemacs" "lsp-mode") (:type git :flavor melpa :files (:defaults "icons" "lsp-treemacs-pkg.el") :host github :repo "emacs-lsp/lsp-treemacs" :package "lsp-treemacs" :local-repo "lsp-treemacs")) "treemacs" ("2021-12-11 13:20:29" ("emacs" "cl-lib" "dash" "s" "ace-window" "pfuture" "hydra" "ht" "cfrs") (:type git :flavor melpa :files (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el" "src/scripts/treemacs*.py" (:exclude "src/extra/*") "treemacs-pkg.el") :host github :repo "Alexander-Miller/treemacs" :package "treemacs" :local-repo "treemacs")) "pfuture" ("2021-12-11 13:20:29" ("emacs") (:type git :flavor melpa :host github :repo "Alexander-Miller/pfuture" :package "pfuture" :local-repo "pfuture")) "cfrs" ("2021-12-11 13:20:29" ("emacs" "dash" "s" "posframe") (:type git :flavor melpa :host github :repo "Alexander-Miller/cfrs" :package "cfrs" :local-repo "cfrs")) "posframe" ("2021-12-11 13:20:29" ("emacs") (:type git :flavor melpa :host github :repo "tumashu/posframe" :package "posframe" :local-repo "posframe")) "srefactor" ("2021-12-11 13:20:29" ("emacs") (:type git :flavor melpa :host github :repo "tuhdo/semantic-refactor" :package "srefactor" :local-repo "semantic-refactor")) "mozc" ("2021-12-11 13:20:23" nil (:type git :flavor melpa :files ("src/unix/emacs/mozc.el" "mozc-pkg.el") :host github :repo "google/mozc" :package "mozc" :local-repo "mozc"))))

#s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("straight" ((straight-x straight-autoloads straight) (autoload 'straight-remove-unused-repos "straight" "Remove unused repositories from the repos directory.
A repo is considered \"unused\" if it was not explicitly requested via
`straight-use-package' during the current Emacs session.
If FORCE is non-nil do not prompt before deleting repos.

(fn &optional FORCE)" t nil) (autoload 'straight-get-recipe "straight" "Interactively select a recipe from one of the recipe repositories.
All recipe repositories in `straight-recipe-repositories' will
first be cloned. After the recipe is selected, it will be copied
to the kill ring. With a prefix argument, first prompt for a
recipe repository to search. Only that repository will be
cloned.

From Lisp code, SOURCES should be a subset of the symbols in
`straight-recipe-repositories'. Only those recipe repositories
are cloned and searched. If it is nil or omitted, then the value
of `straight-recipe-repositories' is used. If SOURCES is the
symbol `interactive', then the user is prompted to select a
recipe repository, and a list containing that recipe repository
is used for the value of SOURCES. ACTION may be `copy' (copy
recipe to the kill ring), `insert' (insert at point), or nil (no
action, just return it).

(fn &optional SOURCES ACTION)" t nil) (autoload 'straight-visit-package-website "straight" "Interactively select a recipe, and visit the package's website." t nil) (autoload 'straight-use-package "straight" "Register, clone, build, and activate a package and its dependencies.
This is the main entry point to the functionality of straight.el.

MELPA-STYLE-RECIPE is either a symbol naming a package, or a list
whose car is a symbol naming a package and whose cdr is a
property list containing e.g. `:type', `:local-repo', `:files',
and VC backend specific keywords.

First, the package recipe is registered with straight.el. If
NO-CLONE is a function, then it is called with two arguments: the
package name as a string, and a boolean value indicating whether
the local repository for the package is available. In that case,
the return value of the function is used as the value of NO-CLONE
instead. In any case, if NO-CLONE is non-nil, then processing
stops here.

Otherwise, the repository is cloned, if it is missing. If
NO-BUILD is a function, then it is called with one argument: the
package name as a string. In that case, the return value of the
function is used as the value of NO-BUILD instead. In any case,
if NO-BUILD is non-nil, then processing halts here. Otherwise,
the package is built and activated. Note that if the package
recipe has a nil `:build' entry, then NO-BUILD is ignored
and processing always stops before building and activation
occurs.

CAUSE is a string explaining the reason why
`straight-use-package' has been called. It is for internal use
only, and is used to construct progress messages. INTERACTIVE is
non-nil if the function has been called interactively. It is for
internal use only, and is used to determine whether to show a
hint about how to install the package permanently.

Return non-nil if package was actually installed, and nil
otherwise (this can only happen if NO-CLONE is non-nil).

(fn MELPA-STYLE-RECIPE &optional NO-CLONE NO-BUILD CAUSE INTERACTIVE)" t nil) (autoload 'straight-register-package "straight" "Register a package without cloning, building, or activating it.
This function is equivalent to calling `straight-use-package'
with a non-nil argument for NO-CLONE. It is provided for
convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-package-no-build "straight" "Register and clone a package without building it.
This function is equivalent to calling `straight-use-package'
with nil for NO-CLONE but a non-nil argument for NO-BUILD. It is
provided for convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-package-lazy "straight" "Register, build, and activate a package if it is already cloned.
This function is equivalent to calling `straight-use-package'
with symbol `lazy' for NO-CLONE. It is provided for convenience.
MELPA-STYLE-RECIPE is as for `straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-recipes "straight" "Register a recipe repository using MELPA-STYLE-RECIPE.
This registers the recipe and builds it if it is already cloned.
Note that you probably want the recipe for a recipe repository to
include a nil `:build' property, to unconditionally
inhibit the build phase.

This function also adds the recipe repository to
`straight-recipe-repositories', at the end of the list.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-override-recipe "straight" "Register MELPA-STYLE-RECIPE as a recipe override.
This puts it in `straight-recipe-overrides', depending on the
value of `straight-current-profile'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-check-package "straight" "Rebuild a PACKAGE if it has been modified.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. See also `straight-rebuild-package' and
`straight-check-all'.

(fn PACKAGE)" t nil) (autoload 'straight-check-all "straight" "Rebuild any packages that have been modified.
See also `straight-rebuild-all' and `straight-check-package'.
This function should not be called during init." t nil) (autoload 'straight-rebuild-package "straight" "Rebuild a PACKAGE.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument RECURSIVE, rebuild
all dependencies as well. See also `straight-check-package' and
`straight-rebuild-all'.

(fn PACKAGE &optional RECURSIVE)" t nil) (autoload 'straight-rebuild-all "straight" "Rebuild all packages.
See also `straight-check-all' and `straight-rebuild-package'." t nil) (autoload 'straight-prune-build-cache "straight" "Prune the build cache.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information and any cached
autoloads discarded." nil nil) (autoload 'straight-prune-build-directory "straight" "Prune the build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build directories deleted." nil nil) (autoload 'straight-prune-build "straight" "Prune the build cache and build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information discarded and
their build directories deleted." t nil) (autoload 'straight-normalize-package "straight" "Normalize a PACKAGE's local repository to its recipe's configuration.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t nil) (autoload 'straight-normalize-all "straight" "Normalize all packages. See `straight-normalize-package'.
Return a list of recipes for packages that were not successfully
normalized. If multiple packages come from the same local
repository, only one is normalized.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t nil) (autoload 'straight-fetch-package "straight" "Try to fetch a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-fetch-package-and-deps "straight" "Try to fetch a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are fetched
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-fetch-all "straight" "Try to fetch all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, fetch not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
fetched. If multiple packages come from the same local
repository, only one is fetched.

PREDICATE, if provided, filters the packages that are fetched. It
is called with the package name as a string, and should return
non-nil if the package should actually be fetched.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-merge-package "straight" "Try to merge a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-merge-package-and-deps "straight" "Try to merge a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are merged
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-merge-all "straight" "Try to merge all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, merge not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
merged. If multiple packages come from the same local
repository, only one is merged.

PREDICATE, if provided, filters the packages that are merged. It
is called with the package name as a string, and should return
non-nil if the package should actually be merged.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-pull-package "straight" "Try to pull a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM, pull
not just from primary remote but also from upstream (for forked
packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-pull-package-and-deps "straight" "Try to pull a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are pulled
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
pull not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-pull-all "straight" "Try to pull all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, pull not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
pulled. If multiple packages come from the same local repository,
only one is pulled.

PREDICATE, if provided, filters the packages that are pulled. It
is called with the package name as a string, and should return
non-nil if the package should actually be pulled.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-push-package "straight" "Push a PACKAGE to its primary remote, if necessary.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t nil) (autoload 'straight-push-all "straight" "Try to push all packages to their primary remotes.

Return a list of recipes for packages that were not successfully
pushed. If multiple packages come from the same local repository,
only one is pushed.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t nil) (autoload 'straight-freeze-versions "straight" "Write version lockfiles for currently activated packages.
This implies first pushing all packages that have unpushed local
changes. If the package management system has been used since the
last time the init-file was reloaded, offer to fix the situation
by reloading the init-file again. If FORCE is
non-nil (interactively, if a prefix argument is provided), skip
all checks and write the lockfile anyway.

Currently, writing version lockfiles requires cloning all lazily
installed packages. Hopefully, this inconvenient requirement will
be removed in the future.

Multiple lockfiles may be written (one for each profile),
according to the value of `straight-profiles'.

(fn &optional FORCE)" t nil) (autoload 'straight-thaw-versions "straight" "Read version lockfiles and restore package versions to those listed." t nil) (autoload 'straight-bug-report "straight" "Test straight.el in a clean environment.
ARGS may be any of the following keywords and their respective values:
  - :pre-bootstrap (Form)...
      Forms evaluated before bootstrapping straight.el
      e.g. (setq straight-repository-branch \"develop\")
      Note this example is already in the default bootstrapping code.

  - :post-bootstrap (Form)...
      Forms evaluated in the testing environment after boostrapping.
      e.g. (straight-use-package '(example :type git :host github))

  - :interactive Boolean
      If nil, the subprocess will immediately exit after the test.
      Output will be printed to `straight-bug-report--process-buffer'
      Otherwise, the subprocess will be interactive.

  - :preserve Boolean
      If non-nil, the test directory is left in the directory stored in the
      variable `temporary-file-directory'. Otherwise, it is
      immediately removed after the test is run.

  - :executable String
      Indicate the Emacs executable to launch.
      Defaults to the path of the current Emacs executable.

  - :raw Boolean
      If non-nil, the raw process output is sent to
      `straight-bug-report--process-buffer'. Otherwise, it is
      formatted as markdown for submitting as an issue.

  - :user-dir String
      If non-nil, the test is run with `user-emacs-directory' set to STRING.
      Otherwise, a temporary directory is created and used.
      Unless absolute, paths are expanded relative to the variable
      `temporary-file-directory'.

ARGS are accessible within the :pre/:post-bootsrap phases via the
locally bound plist, straight-bug-report-args.

(fn &rest ARGS)" nil t) (function-put 'straight-bug-report 'lisp-indent-function '0) (autoload 'straight-dependencies "straight" "Return a list of PACKAGE's dependencies.

(fn &optional PACKAGE)" t nil) (autoload 'straight-dependents "straight" "Return a list PACKAGE's dependents.

(fn &optional PACKAGE)" t nil) (register-definition-prefixes "straight" '("straight-")) (defvar straight-x-pinned-packages nil "List of pinned packages.") (register-definition-prefixes "straight-x" '("straight-x-")) (provide 'straight-autoloads)) "bind-key" ((bind-key bind-key-autoloads) (autoload 'bind-key "bind-key" "Bind KEY-NAME to COMMAND in KEYMAP (`global-map' if not passed).

KEY-NAME may be a vector, in which case it is passed straight to
`define-key'. Or it may be a string to be interpreted as
spelled-out keystrokes, e.g., \"C-c C-z\". See documentation of
`edmacro-mode' for details.

COMMAND must be an interactive function or lambda form.

KEYMAP, if present, should be a keymap variable or symbol.
For example:

  (bind-key \"M-h\" #'some-interactive-function my-mode-map)

  (bind-key \"M-h\" #'some-interactive-function 'my-mode-map)

If PREDICATE is non-nil, it is a form evaluated to determine when
a key should be bound. It must return non-nil in such cases.
Emacs can evaluate this form at any time that it does redisplay
or operates on menu data structures, so you should write it so it
can safely be called at any time.

(fn KEY-NAME COMMAND &optional KEYMAP PREDICATE)" nil t) (autoload 'unbind-key "bind-key" "Unbind the given KEY-NAME, within the KEYMAP (if specified).
See `bind-key' for more details.

(fn KEY-NAME &optional KEYMAP)" nil t) (autoload 'bind-key* "bind-key" "Similar to `bind-key', but overrides any mode-specific bindings.

(fn KEY-NAME COMMAND &optional PREDICATE)" nil t) (autoload 'bind-keys "bind-key" "Bind multiple keys at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted).

(fn &rest ARGS)" nil t) (autoload 'bind-keys* "bind-key" "

(fn &rest ARGS)" nil t) (autoload 'describe-personal-keybindings "bind-key" "Display all the personal keybindings defined by `bind-key'." t nil) (register-definition-prefixes "bind-key" '("bind-key" "compare-keybindings" "get-binding-description" "override-global-m" "personal-keybindings")) (provide 'bind-key-autoloads)) "use-package" ((use-package-bind-key use-package use-package-delight use-package-diminish use-package-lint use-package-ensure use-package-jump use-package-autoloads use-package-core) (autoload 'use-package-autoload-keymap "use-package-bind-key" "Loads PACKAGE and then binds the key sequence used to invoke
this function to KEYMAP-SYMBOL. It then simulates pressing the
same key sequence a again, so that the next key pressed is routed
to the newly loaded keymap.

This function supports use-package's :bind-keymap keyword. It
works by binding the given key sequence to an invocation of this
function for a particular keymap. The keymap is expected to be
defined by the package. In this way, loading the package is
deferred until the prefix key sequence is pressed.

(fn KEYMAP-SYMBOL PACKAGE OVERRIDE)" nil nil) (autoload 'use-package-normalize-binder "use-package-bind-key" "

(fn NAME KEYWORD ARGS)" nil nil) (defalias 'use-package-normalize/:bind 'use-package-normalize-binder) (defalias 'use-package-normalize/:bind* 'use-package-normalize-binder) (defalias 'use-package-autoloads/:bind 'use-package-autoloads-mode) (defalias 'use-package-autoloads/:bind* 'use-package-autoloads-mode) (autoload 'use-package-handler/:bind "use-package-bind-key" "

(fn NAME KEYWORD ARGS REST STATE &optional BIND-MACRO)" nil nil) (defalias 'use-package-normalize/:bind-keymap 'use-package-normalize-binder) (defalias 'use-package-normalize/:bind-keymap* 'use-package-normalize-binder) (autoload 'use-package-handler/:bind-keymap "use-package-bind-key" "

(fn NAME KEYWORD ARGS REST STATE &optional OVERRIDE)" nil nil) (autoload 'use-package-handler/:bind-keymap* "use-package-bind-key" "

(fn NAME KEYWORD ARG REST STATE)" nil nil) (register-definition-prefixes "use-package-bind-key" '("use-package-handler/:bind*")) (autoload 'use-package "use-package-core" "Declare an Emacs package by specifying a group of configuration options.

For full documentation, please see the README file that came with
this file.  Usage:

  (use-package package-name
     [:keyword [option]]...)

:init            Code to run before PACKAGE-NAME has been loaded.
:config          Code to run after PACKAGE-NAME has been loaded.  Note that
                 if loading is deferred for any reason, this code does not
                 execute until the lazy load has occurred.
:preface         Code to be run before everything except `:disabled'; this
                 can be used to define functions for use in `:if', or that
                 should be seen by the byte-compiler.

:mode            Form to be added to `auto-mode-alist'.
:magic           Form to be added to `magic-mode-alist'.
:magic-fallback  Form to be added to `magic-fallback-mode-alist'.
:interpreter     Form to be added to `interpreter-mode-alist'.

:commands        Define autoloads for commands that will be defined by the
                 package.  This is useful if the package is being lazily
                 loaded, and you wish to conditionally call functions in your
                 `:init' block that are defined in the package.
:hook            Specify hook(s) to attach this package to.

:bind            Bind keys, and define autoloads for the bound commands.
:bind*           Bind keys, and define autoloads for the bound commands,
                 *overriding all minor mode bindings*.
:bind-keymap     Bind a key prefix to an auto-loaded keymap defined in the
                 package.  This is like `:bind', but for keymaps.
:bind-keymap*    Like `:bind-keymap', but overrides all minor mode bindings

:defer           Defer loading of a package -- this is implied when using
                 `:commands', `:bind', `:bind*', `:mode', `:magic', `:hook',
                 `:magic-fallback', or `:interpreter'.  This can be an integer,
                 to force loading after N seconds of idle time, if the package
                 has not already been loaded.
:after           Delay the use-package declaration until after the named modules
                 have loaded. Once load, it will be as though the use-package
                 declaration (without `:after') had been seen at that moment.
:demand          Prevent the automatic deferred loading introduced by constructs
                 such as `:bind' (see `:defer' for the complete list).

:if EXPR         Initialize and load only if EXPR evaluates to a non-nil value.
:disabled        The package is ignored completely if this keyword is present.
:defines         Declare certain variables to silence the byte-compiler.
:functions       Declare certain functions to silence the byte-compiler.
:load-path       Add to the `load-path' before attempting to load the package.
:diminish        Support for diminish.el (if installed).
:delight         Support for delight.el (if installed).
:custom          Call `custom-set' or `set-default' with each variable
                 definition without modifying the Emacs `custom-file'.
                 (compare with `custom-set-variables').
:custom-face     Call `customize-set-faces' with each face definition.
:ensure          Loads the package using package.el if necessary.
:pin             Pin the package to an archive.

(fn NAME &rest ARGS)" nil t) (function-put 'use-package 'lisp-indent-function '1) (register-definition-prefixes "use-package-core" '("use-package-")) (autoload 'use-package-normalize/:delight "use-package-delight" "Normalize arguments to delight.

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:delight "use-package-delight" "

(fn NAME KEYWORD ARGS REST STATE)" nil nil) (register-definition-prefixes "use-package-delight" '("use-package-normalize-delight")) (autoload 'use-package-normalize/:diminish "use-package-diminish" "

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:diminish "use-package-diminish" "

(fn NAME KEYWORD ARG REST STATE)" nil nil) (register-definition-prefixes "use-package-diminish" '("use-package-normalize-diminish")) (autoload 'use-package-normalize/:ensure "use-package-ensure" "

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:ensure "use-package-ensure" "

(fn NAME KEYWORD ENSURE REST STATE)" nil nil) (register-definition-prefixes "use-package-ensure" '("use-package-")) (autoload 'use-package-jump-to-package-form "use-package-jump" "Attempt to find and jump to the `use-package' form that loaded
PACKAGE. This will only find the form if that form actually
required PACKAGE. If PACKAGE was previously required then this
function will jump to the file that originally required PACKAGE
instead.

(fn PACKAGE)" t nil) (register-definition-prefixes "use-package-jump" '("use-package-find-require")) (autoload 'use-package-lint "use-package-lint" "Check for errors in use-package declarations.
For example, if the module's `:if' condition is met, but even
with the specified `:load-path' the module cannot be found." t nil) (register-definition-prefixes "use-package-lint" '("use-package-lint-declaration")) (provide 'use-package-autoloads)) "dash" ((dash dash-autoloads) (autoload 'dash-fontify-mode "dash" "Toggle fontification of Dash special variables.

This is a minor mode.  If called interactively, toggle the
`Dash-Fontify mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `dash-fontify-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Dash-Fontify mode is a buffer-local minor mode intended for Emacs
Lisp buffers.  Enabling it causes the special variables bound in
anaphoric Dash macros to be fontified.  These anaphoras include
`it', `it-index', `acc', and `other'.  In older Emacs versions
which do not dynamically detect macros, Dash-Fontify mode
additionally fontifies Dash macro calls.

See also `dash-fontify-mode-lighter' and
`global-dash-fontify-mode'.

(fn &optional ARG)" t nil) (put 'global-dash-fontify-mode 'globalized-minor-mode t) (defvar global-dash-fontify-mode nil "Non-nil if Global Dash-Fontify mode is enabled.
See the `global-dash-fontify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-dash-fontify-mode'.") (custom-autoload 'global-dash-fontify-mode "dash" nil) (autoload 'global-dash-fontify-mode "dash" "Toggle Dash-Fontify mode in all buffers.
With prefix ARG, enable Global Dash-Fontify mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Dash-Fontify mode is enabled in all buffers where
`dash--turn-on-fontify-mode' would do it.

See `dash-fontify-mode' for more information on Dash-Fontify mode.

(fn &optional ARG)" t nil) (autoload 'dash-register-info-lookup "dash" "Register the Dash Info manual with `info-lookup-symbol'.
This allows Dash symbols to be looked up with \\[info-lookup-symbol]." t nil) (register-definition-prefixes "dash" '("!cdr" "!cons" "--" "->" "-a" "-butlast" "-c" "-d" "-e" "-f" "-gr" "-i" "-juxt" "-keep" "-l" "-m" "-no" "-o" "-p" "-r" "-s" "-t" "-u" "-value-to-list" "-when-let" "-zip" "dash-")) (provide 'dash-autoloads)) "yk-util" ((yk-util)) "transient" ((transient-autoloads transient) (autoload 'transient-insert-suffix "transient" "Insert a SUFFIX into PREFIX before LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)" nil nil) (function-put 'transient-insert-suffix 'lisp-indent-function 'defun) (autoload 'transient-append-suffix "transient" "Insert a SUFFIX into PREFIX after LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)" nil nil) (function-put 'transient-append-suffix 'lisp-indent-function 'defun) (autoload 'transient-replace-suffix "transient" "Replace the suffix at LOC in PREFIX with SUFFIX.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)" nil nil) (function-put 'transient-replace-suffix 'lisp-indent-function 'defun) (autoload 'transient-remove-suffix "transient" "Remove the suffix or group at LOC in PREFIX.
PREFIX is a prefix command, a symbol.
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC)" nil nil) (function-put 'transient-remove-suffix 'lisp-indent-function 'defun) (register-definition-prefixes "transient" '("magit--fit-window-to-buffer" "transient-")) (provide 'transient-autoloads)) "with-editor" ((with-editor with-editor-autoloads) (autoload 'with-editor-export-editor "with-editor" "Teach subsequent commands to use current Emacs instance as editor.

Set and export the environment variable ENVVAR, by default
\"EDITOR\".  The value is automatically generated to teach
commands to use the current Emacs instance as \"the editor\".

This works in `shell-mode', `term-mode', `eshell-mode' and
`vterm'.

(fn &optional (ENVVAR \"EDITOR\"))" t nil) (autoload 'with-editor-export-git-editor "with-editor" "Like `with-editor-export-editor' but always set `$GIT_EDITOR'." t nil) (autoload 'with-editor-export-hg-editor "with-editor" "Like `with-editor-export-editor' but always set `$HG_EDITOR'." t nil) (defvar shell-command-with-editor-mode nil "Non-nil if Shell-Command-With-Editor mode is enabled.
See the `shell-command-with-editor-mode' command
for a description of this minor mode.") (custom-autoload 'shell-command-with-editor-mode "with-editor" nil) (autoload 'shell-command-with-editor-mode "with-editor" "Teach `shell-command' to use current Emacs instance as editor.

If called interactively, enable Shell-Command-With-Editor mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Teach `shell-command', and all commands that ultimately call that
command, to use the current Emacs instance as editor by executing
\"EDITOR=CLIENT COMMAND&\" instead of just \"COMMAND&\".

CLIENT is automatically generated; EDITOR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming no other variable overrides the effect of \"$EDITOR\".
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Alternatively you can use the `with-editor-async-shell-command',
which also allows the use of another variable instead of
\"EDITOR\".

(fn &optional ARG)" t nil) (autoload 'with-editor-async-shell-command "with-editor" "Like `async-shell-command' but with `$EDITOR' set.

Execute string \"ENVVAR=CLIENT COMMAND\" in an inferior shell;
display output, if any.  With a prefix argument prompt for an
environment variable, otherwise the default \"EDITOR\" variable
is used.  With a negative prefix argument additionally insert
the COMMAND's output at point.

CLIENT is automatically generated; ENVVAR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming it respects ENVVAR as an \"EDITOR\"-like variable.
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Also see `async-shell-command' and `shell-command'.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t nil) (autoload 'with-editor-shell-command "with-editor" "Like `shell-command' or `with-editor-async-shell-command'.
If COMMAND ends with \"&\" behave like the latter,
else like the former.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t nil) (register-definition-prefixes "with-editor" '("server-" "shell-command--shell-command-with-editor-mode" "start-file-process--with-editor-process-filter" "with-editor")) (provide 'with-editor-autoloads)) "git-commit" ((git-commit-autoloads git-commit git-commit-pkg) (put 'git-commit-major-mode 'safe-local-variable (lambda (val) (memq val '(text-mode markdown-mode org-mode fundamental-mode git-commit-elisp-text-mode)))) (register-definition-prefixes "git-commit" '("git-commit-" "global-git-commit-mode")) (provide 'git-commit-autoloads)) "magit-section" ((magit-section-autoloads magit-section-pkg magit-section) (register-definition-prefixes "magit-section" '("isearch-clean-overlays@magit-mode" "magit-")) (provide 'magit-section-autoloads)) "magit" ((magit-fetch magit-process magit-apply magit-bookmark magit-autorevert magit-core magit-log magit-status magit-bisect magit-refs magit-wip magit-branch magit-pkg magit-clone magit-sequence magit-push magit-obsolete magit-utils magit-imenu magit-pull magit-git magit-reflog magit-merge magit-transient magit-files magit-tag magit-reset magit-commit magit-diff magit-remote magit-stash magit-worktree git-rebase magit-notes magit magit-blame magit-mode magit-submodule magit-patch magit-bundle magit-repos magit-margin magit-gitignore magit-extras magit-autoloads magit-subtree magit-ediff) (autoload 'git-rebase-current-line "git-rebase" "Parse current line into a `git-rebase-action' instance.
If the current line isn't recognized as a rebase line, an
instance with all nil values is returned." nil nil) (autoload 'git-rebase-mode "git-rebase" "Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details.

(fn)" t nil) (defconst git-rebase-filename-regexp "/git-rebase-todo\\'") (add-to-list 'auto-mode-alist (cons git-rebase-filename-regexp 'git-rebase-mode)) (register-definition-prefixes "git-rebase" '("git-rebase-")) (define-obsolete-variable-alias 'global-magit-file-mode 'magit-define-global-key-bindings "Magit 3.0.0") (defvar magit-define-global-key-bindings t "Whether to bind some Magit commands in the global keymap.

If this variable is non-nil, then the following bindings may
be added to the global keymap.  The default is t.

key             binding
---             -------
C-x g           magit-status
C-x M-g         magit-dispatch
C-c M-g         magit-file-dispatch

These bindings may be added when `after-init-hook' is run.
Each binding is added if and only if at that time no other key
is bound to the same command and no other command is bound to
the same key.  In other words we try to avoid adding bindings
that are unnecessary, as well as bindings that conflict with
other bindings.

Adding the above bindings is delayed until `after-init-hook'
is called to allow users to set the variable anywhere in their
init file (without having to make sure to do so before `magit'
is loaded or autoloaded) and to increase the likelihood that
all the potentially conflicting user bindings have already
been added.

To set this variable use either `setq' or the Custom interface.
Do not use the function `customize-set-variable' because doing
that would cause Magit to be loaded immediately when that form
is evaluated (this differs from `custom-set-variables', which
doesn't load the libraries that define the customized variables).

Setting this variable to nil has no effect if that is done after
the key bindings have already been added.

We recommend that you bind \"C-c g\" instead of \"C-c M-g\" to
`magit-file-dispatch'.  The former is a much better binding
but the \"C-c <letter>\" namespace is strictly reserved for
users; preventing Magit from using it by default.

Also see info node `(magit)Commands for Buffers Visiting Files'.") (custom-autoload 'magit-define-global-key-bindings "magit" t) (defun magit-maybe-define-global-key-bindings nil (when magit-define-global-key-bindings (let ((map (current-global-map))) (dolist (elt '(("C-x g" . magit-status) ("C-x M-g" . magit-dispatch) ("C-c M-g" . magit-file-dispatch))) (let ((key (kbd (car elt))) (def (cdr elt))) (unless (or (lookup-key map key) (where-is-internal def (make-sparse-keymap) t)) (define-key map key def))))))) (if after-init-time (magit-maybe-define-global-key-bindings) (add-hook 'after-init-hook 'magit-maybe-define-global-key-bindings t)) (autoload 'magit-dispatch "magit" nil t) (autoload 'magit-run "magit" nil t) (autoload 'magit-git-command "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

With a prefix argument COMMAND is run in the top-level directory
of the current working tree, otherwise in `default-directory'.

(fn COMMAND)" t nil) (autoload 'magit-git-command-topdir "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

COMMAND is run in the top-level directory of the current
working tree.

(fn COMMAND)" t nil) (autoload 'magit-shell-command "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  With a
prefix argument COMMAND is run in the top-level directory of
the current working tree, otherwise in `default-directory'.

(fn COMMAND)" t nil) (autoload 'magit-shell-command-topdir "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  COMMAND
is run in the top-level directory of the current working tree.

(fn COMMAND)" t nil) (autoload 'magit-version "magit" "Return the version of Magit currently in use.
If optional argument PRINT-DEST is non-nil, output
stream (interactively, the echo area, or the current buffer with
a prefix argument), also print the used versions of Magit, Git,
and Emacs to it.

(fn &optional PRINT-DEST)" t nil) (register-definition-prefixes "magit" '("magit-")) (autoload 'magit-stage-file "magit-apply" "Stage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be staged.  Otherwise stage the file at point without
requiring confirmation.

(fn FILE)" t nil) (autoload 'magit-stage-modified "magit-apply" "Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files.

(fn &optional ALL)" t nil) (autoload 'magit-unstage-file "magit-apply" "Unstage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be unstaged.  Otherwise unstage the file at point
without requiring confirmation.

(fn FILE)" t nil) (autoload 'magit-unstage-all "magit-apply" "Remove all changes from the staging area." t nil) (register-definition-prefixes "magit-apply" '("magit-")) (put 'magit-auto-revert-mode 'globalized-minor-mode t) (defvar magit-auto-revert-mode (not (or global-auto-revert-mode noninteractive)) "Non-nil if Magit-Auto-Revert mode is enabled.
See the `magit-auto-revert-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-auto-revert-mode'.") (custom-autoload 'magit-auto-revert-mode "magit-autorevert" nil) (autoload 'magit-auto-revert-mode "magit-autorevert" "Toggle Auto-Revert mode in all buffers.
With prefix ARG, enable Magit-Auto-Revert mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Auto-Revert mode is enabled in all buffers where
`magit-turn-on-auto-revert-mode-if-desired' would do it.
See `auto-revert-mode' for more information on Auto-Revert mode.

(fn &optional ARG)" t nil) (register-definition-prefixes "magit-autorevert" '("auto-revert-buffer" "magit-")) (autoload 'magit-bisect "magit-bisect" nil t) (autoload 'magit-bisect-start "magit-bisect" "Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a known
good and a known bad commit.  To move the session forward use the
other actions from the bisect transient command (\\<magit-status-mode-map>\\[magit-bisect]).

(fn BAD GOOD ARGS)" t nil) (autoload 'magit-bisect-reset "magit-bisect" "After bisecting, cleanup bisection state and return to original `HEAD'." t nil) (autoload 'magit-bisect-good "magit-bisect" "While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question." t nil) (autoload 'magit-bisect-bad "magit-bisect" "While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question." t nil) (autoload 'magit-bisect-mark "magit-bisect" "While bisecting, mark the current commit with a bisect term.
During a bisect using alternate terms, commits can still be
marked with `magit-bisect-good' and `magit-bisect-bad', as those
commands map to the correct term (\"good\" to --term-old's value
and \"bad\" to --term-new's).  However, in some cases, it can be
difficult to keep that mapping straight in your head; this
command provides an interface that exposes the underlying terms." t nil) (autoload 'magit-bisect-skip "magit-bisect" "While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one." t nil) (autoload 'magit-bisect-run "magit-bisect" "Bisect automatically by running commands after each step.

Unlike `git bisect run' this can be used before bisecting has
begun.  In that case it behaves like `git bisect start; git
bisect run'.

(fn CMDLINE &optional BAD GOOD ARGS)" t nil) (register-definition-prefixes "magit-bisect" '("magit-")) (autoload 'magit-blame-echo "magit-blame" nil t) (autoload 'magit-blame-addition "magit-blame" nil t) (autoload 'magit-blame-removal "magit-blame" nil t) (autoload 'magit-blame-reverse "magit-blame" nil t) (autoload 'magit-blame "magit-blame" nil t) (register-definition-prefixes "magit-blame" '("magit-")) (autoload 'magit--handle-bookmark "magit-bookmark" "Open a bookmark created by `magit--make-bookmark'.
Call the `magit-*-setup-buffer' function of the the major-mode
with the variables' values as arguments, which were recorded by
`magit--make-bookmark'.  Ignore `magit-display-buffer-function'.

(fn BOOKMARK)" nil nil) (register-definition-prefixes "magit-bookmark" '("magit--make-bookmark")) (autoload 'magit-branch "magit" nil t) (autoload 'magit-checkout "magit-branch" "Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch, then that becomes the current
branch.  If it is something else, then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.

(git checkout REVISION).

(fn REVISION &optional ARGS)" t nil) (autoload 'magit-branch-create "magit-branch" "Create BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT)" t nil) (autoload 'magit-branch-and-checkout "magit-branch" "Create and checkout BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT &optional ARGS)" t nil) (autoload 'magit-branch-or-checkout "magit-branch" "Hybrid between `magit-checkout' and `magit-branch-and-checkout'.

Ask the user for an existing branch or revision.  If the user
input actually can be resolved as a branch or revision, then
check that out, just like `magit-checkout' would.

Otherwise create and checkout a new branch using the input as
its name.  Before doing so read the starting-point for the new
branch.  This is similar to what `magit-branch-and-checkout'
does.

(fn ARG &optional START-POINT)" t nil) (autoload 'magit-branch-checkout "magit-branch" "Checkout an existing or new local branch.

Read a branch name from the user offering all local branches and
a subset of remote branches as candidates.  Omit remote branches
for which a local branch by the same name exists from the list
of candidates.  The user can also enter a completely new branch
name.

- If the user selects an existing local branch, then check that
  out.

- If the user selects a remote branch, then create and checkout
  a new local branch with the same name.  Configure the selected
  remote branch as push target.

- If the user enters a new branch name, then create and check
  that out, after also reading the starting-point from the user.

In the latter two cases the upstream is also set.  Whether it is
set to the chosen START-POINT or something else depends on the
value of `magit-branch-adjust-remote-upstream-alist', just like
when using `magit-branch-and-checkout'.

(fn BRANCH &optional START-POINT)" t nil) (autoload 'magit-branch-orphan "magit-branch" "Create and checkout an orphan BRANCH with contents from revision START-POINT.

(fn BRANCH START-POINT)" t nil) (autoload 'magit-branch-spinout "magit-branch" "Create new branch from the unpushed commits.
Like `magit-branch-spinoff' but remain on the current branch.
If there are any uncommitted changes, then behave exactly like
`magit-branch-spinoff'.

(fn BRANCH &optional FROM)" t nil) (autoload 'magit-branch-spinoff "magit-branch" "Create new branch from the unpushed commits.

Create and checkout a new branch starting at and tracking the
current branch.  That branch in turn is reset to the last commit
it shares with its upstream.  If the current branch has no
upstream or no unpushed commits, then the new branch is created
anyway and the previously current branch is not touched.

This is useful to create a feature branch after work has already
began on the old branch (likely but not necessarily \"master\").

If the current branch is a member of the value of option
`magit-branch-prefer-remote-upstream' (which see), then the
current branch will be used as the starting point as usual, but
the upstream of the starting-point may be used as the upstream
of the new branch, instead of the starting-point itself.

If optional FROM is non-nil, then the source branch is reset
to `FROM~', instead of to the last commit it shares with its
upstream.  Interactively, FROM is only ever non-nil, if the
region selects some commits, and among those commits, FROM is
the commit that is the fewest commits ahead of the source
branch.

The commit at the other end of the selection actually does not
matter, all commits between FROM and `HEAD' are moved to the new
branch.  If FROM is not reachable from `HEAD' or is reachable
from the source branch's upstream, then an error is raised.

(fn BRANCH &optional FROM)" t nil) (autoload 'magit-branch-reset "magit-branch" "Reset a branch to the tip of another branch or any other commit.

When the branch being reset is the current branch, then do a
hard reset.  If there are any uncommitted changes, then the user
has to confirm the reset because those changes would be lost.

This is useful when you have started work on a feature branch but
realize it's all crap and want to start over.

When resetting to another branch and a prefix argument is used,
then also set the target branch as the upstream of the branch
that is being reset.

(fn BRANCH TO &optional SET-UPSTREAM)" t nil) (autoload 'magit-branch-delete "magit-branch" "Delete one or multiple branches.
If the region marks multiple branches, then offer to delete
those, otherwise prompt for a single branch to be deleted,
defaulting to the branch at point.

(fn BRANCHES &optional FORCE)" t nil) (autoload 'magit-branch-rename "magit-branch" "Rename the branch named OLD to NEW.

With a prefix argument FORCE, rename even if a branch named NEW
already exists.

If `branch.OLD.pushRemote' is set, then unset it.  Depending on
the value of `magit-branch-rename-push-target' (which see) maybe
set `branch.NEW.pushRemote' and maybe rename the push-target on
the remote.

(fn OLD NEW &optional FORCE)" t nil) (autoload 'magit-branch-shelve "magit-branch" "Shelve a BRANCH.
Rename \"refs/heads/BRANCH\" to \"refs/shelved/BRANCH\",
and also rename the respective reflog file.

(fn BRANCH)" t nil) (autoload 'magit-branch-unshelve "magit-branch" "Unshelve a BRANCH
Rename \"refs/shelved/BRANCH\" to \"refs/heads/BRANCH\",
and also rename the respective reflog file.

(fn BRANCH)" t nil) (autoload 'magit-branch-configure "magit-branch" nil t) (register-definition-prefixes "magit-branch" '("magit-")) (autoload 'magit-bundle "magit-bundle" nil t) (autoload 'magit-bundle-import "magit-bundle" nil t) (autoload 'magit-bundle-create-tracked "magit-bundle" "Create and track a new bundle.

(fn FILE TAG BRANCH REFS ARGS)" t nil) (autoload 'magit-bundle-update-tracked "magit-bundle" "Update a bundle that is being tracked using TAG.

(fn TAG)" t nil) (autoload 'magit-bundle-verify "magit-bundle" "Check whether FILE is valid and applies to the current repository.

(fn FILE)" t nil) (autoload 'magit-bundle-list-heads "magit-bundle" "List the refs in FILE.

(fn FILE)" t nil) (register-definition-prefixes "magit-bundle" '("magit-")) (autoload 'magit-clone "magit-clone" nil t) (autoload 'magit-clone-regular "magit-clone" "Create a clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t nil) (autoload 'magit-clone-shallow "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
With a prefix argument read the DEPTH of the clone;
otherwise use 1.

(fn REPOSITORY DIRECTORY ARGS DEPTH)" t nil) (autoload 'magit-clone-shallow-since "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits before DATE, which is read from the
user.

(fn REPOSITORY DIRECTORY ARGS DATE)" t nil) (autoload 'magit-clone-shallow-exclude "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits reachable from EXCLUDE, which is a
branch or tag read from the user.

(fn REPOSITORY DIRECTORY ARGS EXCLUDE)" t nil) (autoload 'magit-clone-bare "magit-clone" "Create a bare clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t nil) (autoload 'magit-clone-mirror "magit-clone" "Create a mirror of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t nil) (register-definition-prefixes "magit-clone" '("magit-clone-")) (autoload 'magit-commit "magit-commit" nil t) (autoload 'magit-commit-create "magit-commit" "Create a new commit on `HEAD'.
With a prefix argument, amend to the commit at `HEAD' instead.

(git commit [--amend] ARGS)

(fn &optional ARGS)" t nil) (autoload 'magit-commit-amend "magit-commit" "Amend the last commit.

(git commit --amend ARGS)

(fn &optional ARGS)" t nil) (autoload 'magit-commit-extend "magit-commit" "Amend the last commit, without editing the message.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-extend-override-date' can be used
to inverse the meaning of the prefix argument.  
(git commit
--amend --no-edit)

(fn &optional ARGS OVERRIDE-DATE)" t nil) (autoload 'magit-commit-reword "magit-commit" "Reword the last commit, ignoring staged changes.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-reword-override-date' can be used
to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.

(git commit --amend --only)

(fn &optional ARGS OVERRIDE-DATE)" t nil) (autoload 'magit-commit-fixup "magit-commit" "Create a fixup commit.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-squash "magit-commit" "Create a squash commit, without editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

If you want to immediately add a message to the squash commit,
then use `magit-commit-augment' instead of this command.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-augment "magit-commit" "Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-instant-fixup "magit-commit" "Create a fixup commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-instant-squash "magit-commit" "Create a squash commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-reshelve "magit-commit" "Change the committer date and possibly the author date of `HEAD'.

The current time is used as the initial minibuffer input and the
original author or committer date is available as the previous
history element.

Both the author and the committer dates are changes, unless one
of the following is true, in which case only the committer date
is updated:
- You are not the author of the commit that is being reshelved.
- The command was invoked with a prefix argument.
- Non-interactively if UPDATE-AUTHOR is nil.

(fn DATE UPDATE-AUTHOR &optional ARGS)" t nil) (autoload 'magit-commit-absorb-modules "magit-commit" "Spread modified modules across recent commits.

(fn PHASE COMMIT)" t nil) (autoload 'magit-commit-absorb "magit-commit" nil t) (autoload 'magit-commit-autofixup "magit-commit" nil t) (register-definition-prefixes "magit-commit" '("magit-")) (autoload 'magit-diff "magit-diff" nil t) (autoload 'magit-diff-refresh "magit-diff" nil t) (autoload 'magit-diff-dwim "magit-diff" "Show changes for the thing at point.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-diff-range "magit-diff" "Show differences between two commits.

REV-OR-RANGE should be a range or a single revision.  If it is a
revision, then show changes in the working tree relative to that
revision.  If it is a range, but one side is omitted, then show
changes relative to `HEAD'.

If the region is active, use the revisions on the first and last
line of the region as the two sides of the range.  With a prefix
argument, instead of diffing the revisions, choose a revision to
view changes along, starting at the common ancestor of both
revisions (i.e., use a \"...\" range).

(fn REV-OR-RANGE &optional ARGS FILES)" t nil) (autoload 'magit-diff-working-tree "magit-diff" "Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer.

(fn &optional REV ARGS FILES)" t nil) (autoload 'magit-diff-staged "magit-diff" "Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer.

(fn &optional REV ARGS FILES)" t nil) (autoload 'magit-diff-unstaged "magit-diff" "Show changes between the working tree and the index.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-diff-unmerged "magit-diff" "Show changes that are being merged.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-diff-while-committing "magit-diff" "While committing, show the changes that are about to be committed.
While amending, invoking the command again toggles between
showing just the new changes or all the changes that will
be committed.

(fn &optional ARGS)" t nil) (autoload 'magit-diff-buffer-file "magit-diff" "Show diff for the blob or file visited in the current buffer.

When the buffer visits a blob, then show the respective commit.
When the buffer visits a file, then show the differenced between
`HEAD' and the working tree.  In both cases limit the diff to
the file or blob." t nil) (autoload 'magit-diff-paths "magit-diff" "Show changes between any two files on disk.

(fn A B)" t nil) (autoload 'magit-show-commit "magit-diff" "Visit the revision at point in another buffer.
If there is no revision at point or with a prefix argument prompt
for a revision.

(fn REV &optional ARGS FILES MODULE)" t nil) (register-definition-prefixes "magit-diff" '("magit-")) (autoload 'magit-ediff "magit-ediff" nil) (autoload 'magit-ediff-resolve "magit-ediff" "Resolve outstanding conflicts in FILE using Ediff.
FILE has to be relative to the top directory of the repository.

In the rare event that you want to manually resolve all
conflicts, including those already resolved by Git, use
`ediff-merge-revisions-with-ancestor'.

(fn FILE)" t nil) (autoload 'magit-ediff-stage "magit-ediff" "Stage and unstage changes to FILE using Ediff.
FILE has to be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-compare "magit-ediff" "Compare REVA:FILEA with REVB:FILEB using Ediff.

FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil, then this stands for the
working tree state.

If the region is active, use the revisions on the first and last
line of the region.  With a prefix argument, instead of diffing
the revisions, choose a revision to view changes along, starting
at the common ancestor of both revisions (i.e., use a \"...\"
range).

(fn REVA REVB FILEA FILEB)" t nil) (autoload 'magit-ediff-dwim "magit-ediff" "Compare, stage, or resolve using Ediff.
This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using Ediff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `magit-ediff-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run." t nil) (autoload 'magit-ediff-show-staged "magit-ediff" "Show staged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-show-unstaged "magit-ediff" "Show unstaged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-show-working-tree "magit-ediff" "Show changes between `HEAD' and working tree using Ediff.
FILE must be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-show-commit "magit-ediff" "Show changes introduced by COMMIT using Ediff.

(fn COMMIT)" t nil) (autoload 'magit-ediff-show-stash "magit-ediff" "Show changes introduced by STASH using Ediff.
`magit-ediff-show-stash-with-index' controls whether a
three-buffer Ediff is used in order to distinguish changes in the
stash that were staged.

(fn STASH)" t nil) (register-definition-prefixes "magit-ediff" '("magit-ediff-")) (autoload 'magit-run-git-gui "magit-extras" "Run `git gui' for the current git repository." t nil) (autoload 'magit-run-git-gui-blame "magit-extras" "Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the `HEAD', with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on.

(fn COMMIT FILENAME &optional LINENUM)" t nil) (autoload 'magit-run-gitk "magit-extras" "Run `gitk' in the current repository." t nil) (autoload 'magit-run-gitk-branches "magit-extras" "Run `gitk --branches' in the current repository." t nil) (autoload 'magit-run-gitk-all "magit-extras" "Run `gitk --all' in the current repository." t nil) (autoload 'ido-enter-magit-status "magit-extras" "Drop into `magit-status' from file switching.

This command does not work in Emacs 26.1.
See https://github.com/magit/magit/issues/3634
and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31707.

To make this command available use something like:

  (add-hook \\='ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd \"C-x g\") \\='ido-enter-magit-status)))

Starting with Emacs 25.1 the Ido keymaps are defined just once
instead of every time Ido is invoked, so now you can modify it
like pretty much every other keymap:

  (define-key ido-common-completion-map
    (kbd \"C-x g\") \\='ido-enter-magit-status)" t nil) (autoload 'magit-project-status "magit-extras" "Run `magit-status' in the current project's root." t nil) (autoload 'magit-dired-jump "magit-extras" "Visit file at point using Dired.
With a prefix argument, visit in another window.  If there
is no file at point, then instead visit `default-directory'.

(fn &optional OTHER-WINDOW)" t nil) (autoload 'magit-dired-log "magit-extras" "Show log for all marked files, or the current file.

(fn &optional FOLLOW)" t nil) (autoload 'magit-dired-am-apply-patches "magit-extras" "In Dired, apply the marked (or next ARG) files as patches.
If inside a repository, then apply in that.  Otherwise prompt
for a repository.

(fn REPO &optional ARG)" t nil) (autoload 'magit-do-async-shell-command "magit-extras" "Open FILE with `dired-do-async-shell-command'.
Interactively, open the file at point.

(fn FILE)" t nil) (autoload 'magit-previous-line "magit-extras" "Like `previous-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects an
area that is larger than the region.  This causes `previous-line'
when invoked while holding the shift key to move up one line and
thereby select two lines.  When invoked inside a hunk body this
command does not move point on the first invocation and thereby
it only selects a single line.  Which inconsistency you prefer
is a matter of preference.

(fn &optional ARG TRY-VSCROLL)" t nil) (function-put 'magit-previous-line 'interactive-only '"use `forward-line' with negative argument instead.") (autoload 'magit-next-line "magit-extras" "Like `next-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects
an area that is larger than the region.  This causes `next-line'
when invoked while holding the shift key to move down one line
and thereby select two lines.  When invoked inside a hunk body
this command does not move point on the first invocation and
thereby it only selects a single line.  Which inconsistency you
prefer is a matter of preference.

(fn &optional ARG TRY-VSCROLL)" t nil) (function-put 'magit-next-line 'interactive-only 'forward-line) (autoload 'magit-clean "magit-extras" "Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.

(git clean -f -d [-x|-X])

(fn &optional ARG)" t nil) (autoload 'magit-add-change-log-entry "magit-extras" "Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Magit buffer instead of on
a position in a file-visiting buffer.

(fn &optional WHOAMI FILE-NAME OTHER-WINDOW)" t nil) (autoload 'magit-add-change-log-entry-other-window "magit-extras" "Find change log file in other window and add entry and item.
This differs from `add-change-log-entry-other-window' (which see)
in that it acts on the current hunk in a Magit buffer instead of
on a position in a file-visiting buffer.

(fn &optional WHOAMI FILE-NAME)" t nil) (autoload 'magit-edit-line-commit "magit-extras" "Edit the commit that added the current line.

With a prefix argument edit the commit that removes the line,
if any.  The commit is determined using `git blame' and made
editable using `git rebase --interactive' if it is reachable
from `HEAD', or by checking out the commit (or a branch that
points at it) otherwise.

(fn &optional TYPE)" t nil) (autoload 'magit-diff-edit-hunk-commit "magit-extras" "From a hunk, edit the respective commit and visit the file.

First visit the file being modified by the hunk at the correct
location using `magit-diff-visit-file'.  This actually visits a
blob.  When point is on a diff header, not within an individual
hunk, then this visits the blob the first hunk is about.

Then invoke `magit-edit-line-commit', which uses an interactive
rebase to make the commit editable, or if that is not possible
because the commit is not reachable from `HEAD' by checking out
that commit directly.  This also causes the actual worktree file
to be visited.

Neither the blob nor the file buffer are killed when finishing
the rebase.  If that is undesirable, then it might be better to
use `magit-rebase-edit-command' instead of this command.

(fn FILE)" t nil) (autoload 'magit-reshelve-since "magit-extras" "Change the author and committer dates of the commits since REV.

Ask the user for the first reachable commit whose dates should
be changed.  Then read the new date for that commit.  The initial
minibuffer input and the previous history element offer good
values.  The next commit will be created one minute later and so
on.

This command is only intended for interactive use and should only
be used on highly rearranged and unpublished history.

If KEYID is non-nil, then use that to sign all reshelved commits.
Interactively use the value of the \"--gpg-sign\" option in the
list returned by `magit-rebase-arguments'.

(fn REV KEYID)" t nil) (autoload 'magit-pop-revision-stack "magit-extras" "Insert a representation of a revision into the current buffer.

Pop a revision from the `magit-revision-stack' and insert it into
the current buffer according to `magit-pop-revision-stack-format'.
Revisions can be put on the stack using `magit-copy-section-value'
and `magit-copy-buffer-revision'.

If the stack is empty or with a prefix argument, instead read a
revision in the minibuffer.  By using the minibuffer history this
allows selecting an item which was popped earlier or to insert an
arbitrary reference or revision without first pushing it onto the
stack.

When reading the revision from the minibuffer, then it might not
be possible to guess the correct repository.  When this command
is called inside a repository (e.g. while composing a commit
message), then that repository is used.  Otherwise (e.g. while
composing an email) then the repository recorded for the top
element of the stack is used (even though we insert another
revision).  If not called inside a repository and with an empty
stack, or with two prefix arguments, then read the repository in
the minibuffer too.

(fn REV TOPLEVEL)" t nil) (autoload 'magit-copy-section-value "magit-extras" "Save the value of the current section for later use.

Save the section value to the `kill-ring', and, provided that
the current section is a commit, branch, or tag section, push
the (referenced) revision to the `magit-revision-stack' for use
with `magit-pop-revision-stack'.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'.

When the current section is a branch or a tag, and a prefix
argument is used, then save the revision at its tip to the
`kill-ring' instead of the reference name.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.  If a prefix argument is used and the region is within
a hunk, then strip the diff marker column and keep only either
the added or removed lines, depending on the sign of the prefix
argument.

(fn ARG)" t nil) (autoload 'magit-copy-buffer-revision "magit-extras" "Save the revision of the current buffer for later use.

Save the revision shown in the current buffer to the `kill-ring'
and push it to the `magit-revision-stack'.

This command is mainly intended for use in `magit-revision-mode'
buffers, the only buffers where it is always unambiguous exactly
which revision should be saved.

Most other Magit buffers usually show more than one revision, in
some way or another, so this command has to select one of them,
and that choice might not always be the one you think would have
been the best pick.

In such buffers it is often more useful to save the value of
the current section instead, using `magit-copy-section-value'.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'." t nil) (autoload 'magit-display-repository-buffer "magit-extras" "Display a Magit buffer belonging to the current Git repository.
The buffer is displayed using `magit-display-buffer', which see.

(fn BUFFER)" t nil) (autoload 'magit-switch-to-repository-buffer "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t nil) (autoload 'magit-switch-to-repository-buffer-other-window "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t nil) (autoload 'magit-switch-to-repository-buffer-other-frame "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t nil) (autoload 'magit-abort-dwim "magit-extras" "Abort current operation.
Depending on the context, this will abort a merge, a rebase, a
patch application, a cherry-pick, a revert, or a bisect." t nil) (register-definition-prefixes "magit-extras" '("magit-")) (autoload 'magit-fetch "magit-fetch" nil t) (autoload 'magit-fetch-from-pushremote "magit-fetch" nil t) (autoload 'magit-fetch-from-upstream "magit-fetch" nil t) (autoload 'magit-fetch-other "magit-fetch" "Fetch from another repository.

(fn REMOTE ARGS)" t nil) (autoload 'magit-fetch-branch "magit-fetch" "Fetch a BRANCH from a REMOTE.

(fn REMOTE BRANCH ARGS)" t nil) (autoload 'magit-fetch-refspec "magit-fetch" "Fetch a REFSPEC from a REMOTE.

(fn REMOTE REFSPEC ARGS)" t nil) (autoload 'magit-fetch-all "magit-fetch" "Fetch from all remotes.

(fn ARGS)" t nil) (autoload 'magit-fetch-all-prune "magit-fetch" "Fetch from all remotes, and prune.
Prune remote tracking branches for branches that have been
removed on the respective remote." t nil) (autoload 'magit-fetch-all-no-prune "magit-fetch" "Fetch from all remotes." t nil) (autoload 'magit-fetch-modules "magit-fetch" "Fetch all submodules.

Option `magit-fetch-modules-jobs' controls how many submodules
are being fetched in parallel.  Also fetch the super-repository,
because `git-fetch' does not support not doing that.  With a
prefix argument fetch all remotes.

(fn &optional ALL)" t nil) (register-definition-prefixes "magit-fetch" '("magit-")) (autoload 'magit-find-file "magit-files" "View FILE from REV.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go
to the line and column corresponding to that location.

(fn REV FILE)" t nil) (autoload 'magit-find-file-other-window "magit-files" "View FILE from REV, in another window.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

(fn REV FILE)" t nil) (autoload 'magit-find-file-other-frame "magit-files" "View FILE from REV, in another frame.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

(fn REV FILE)" t nil) (autoload 'magit-file-dispatch "magit" nil t) (autoload 'magit-blob-visit-file "magit-files" "View the file from the worktree corresponding to the current blob.
When visiting a blob or the version from the index, then go to
the same location in the respective file in the working tree." t nil) (autoload 'magit-file-checkout "magit-files" "Checkout FILE from REV.

(fn REV FILE)" t nil) (register-definition-prefixes "magit-files" '("magit-")) (register-definition-prefixes "magit-git" '("magit-")) (autoload 'magit-gitignore "magit-gitignore" nil t) (autoload 'magit-gitignore-in-topdir "magit-gitignore" "Add the Git ignore RULE to the top-level \".gitignore\" file.
Since this file is tracked, it is shared with other clones of the
repository.  Also stage the file.

(fn RULE)" t nil) (autoload 'magit-gitignore-in-subdir "magit-gitignore" "Add the Git ignore RULE to a \".gitignore\" file in DIRECTORY.
Prompt the user for a directory and add the rule to the
\".gitignore\" file in that directory.  Since such files are
tracked, they are shared with other clones of the repository.
Also stage the file.

(fn RULE DIRECTORY)" t nil) (autoload 'magit-gitignore-in-gitdir "magit-gitignore" "Add the Git ignore RULE to \"$GIT_DIR/info/exclude\".
Rules in that file only affects this clone of the repository.

(fn RULE)" t nil) (autoload 'magit-gitignore-on-system "magit-gitignore" "Add the Git ignore RULE to the file specified by `core.excludesFile'.
Rules that are defined in that file affect all local repositories.

(fn RULE)" t nil) (autoload 'magit-skip-worktree "magit-gitignore" "Call \"git update-index --skip-worktree -- FILE\".

(fn FILE)" t nil) (autoload 'magit-no-skip-worktree "magit-gitignore" "Call \"git update-index --no-skip-worktree -- FILE\".

(fn FILE)" t nil) (autoload 'magit-assume-unchanged "magit-gitignore" "Call \"git update-index --assume-unchanged -- FILE\".

(fn FILE)" t nil) (autoload 'magit-no-assume-unchanged "magit-gitignore" "Call \"git update-index --no-assume-unchanged -- FILE\".

(fn FILE)" t nil) (register-definition-prefixes "magit-gitignore" '("magit-")) (autoload 'magit-imenu--log-prev-index-position-function "magit-imenu" "Move point to previous line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--log-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--diff-prev-index-position-function "magit-imenu" "Move point to previous file line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--diff-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--status-create-index-function "magit-imenu" "Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'." nil nil) (autoload 'magit-imenu--refs-create-index-function "magit-imenu" "Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'." nil nil) (autoload 'magit-imenu--cherry-create-index-function "magit-imenu" "Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'." nil nil) (autoload 'magit-imenu--submodule-prev-index-position-function "magit-imenu" "Move point to previous line in magit-submodule-list buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--submodule-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--repolist-prev-index-position-function "magit-imenu" "Move point to previous line in magit-repolist buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--repolist-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--process-prev-index-position-function "magit-imenu" "Move point to previous process in magit-process buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--process-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--rebase-prev-index-position-function "magit-imenu" "Move point to previous commit in git-rebase buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--rebase-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (register-definition-prefixes "magit-imenu" '("magit-imenu--index-function")) (autoload 'magit-log "magit-log" nil t) (autoload 'magit-log-refresh "magit-log" nil t) (autoload 'magit-log-current "magit-log" "Show log for the current branch.
When `HEAD' is detached or with a prefix argument show log for
one or more revs read from the minibuffer.

(fn REVS &optional ARGS FILES)" t nil) (autoload 'magit-log-other "magit-log" "Show log for one or more revs read from the minibuffer.
The user can input any revision or revisions separated by a
space, or even ranges, but only branches and tags, and a
representation of the commit at point, are available as
completion candidates.

(fn REVS &optional ARGS FILES)" t nil) (autoload 'magit-log-head "magit-log" "Show log for `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-branches "magit-log" "Show log for all local branches and `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-matching-branches "magit-log" "Show log for all branches matching PATTERN and `HEAD'.

(fn PATTERN &optional ARGS FILES)" t nil) (autoload 'magit-log-matching-tags "magit-log" "Show log for all tags matching PATTERN and `HEAD'.

(fn PATTERN &optional ARGS FILES)" t nil) (autoload 'magit-log-all-branches "magit-log" "Show log for all local and remote branches and `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-all "magit-log" "Show log for all references and `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-buffer-file "magit-log" "Show log for the blob or file visited in the current buffer.
With a prefix argument or when `--follow' is an active log
argument, then follow renames.  When the region is active,
restrict the log to the lines that the region touches.

(fn &optional FOLLOW BEG END)" t nil) (autoload 'magit-log-trace-definition "magit-log" "Show log for the definition at point.

(fn FILE FN REV)" t nil) (autoload 'magit-log-merged "magit-log" "Show log for the merge of COMMIT into BRANCH.

More precisely, find merge commit M that brought COMMIT into
BRANCH, and show the log of the range \"M^1..M\".  If COMMIT is
directly on BRANCH, then show approximately twenty surrounding
commits instead.

This command requires git-when-merged, which is available from
https://github.com/mhagger/git-when-merged.

(fn COMMIT BRANCH &optional ARGS FILES)" t nil) (autoload 'magit-log-move-to-parent "magit-log" "Move to the Nth parent of the current commit.

(fn &optional N)" t nil) (autoload 'magit-shortlog "magit-log" nil t) (autoload 'magit-shortlog-since "magit-log" "Show a history summary for commits since REV.

(fn REV ARGS)" t nil) (autoload 'magit-shortlog-range "magit-log" "Show a history summary for commit or range REV-OR-RANGE.

(fn REV-OR-RANGE ARGS)" t nil) (autoload 'magit-cherry "magit-log" "Show commits in a branch that are not merged in the upstream branch.

(fn HEAD UPSTREAM)" t nil) (register-definition-prefixes "magit-log" '("magit-")) (register-definition-prefixes "magit-margin" '("magit-")) (autoload 'magit-merge "magit" nil t) (autoload 'magit-merge-plain "magit-merge" "Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

(git merge --no-edit|--no-commit [ARGS] REV)

(fn REV &optional ARGS NOCOMMIT)" t nil) (autoload 'magit-merge-editmsg "magit-merge" "Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.

(git merge --edit --no-ff [ARGS] REV)

(fn REV &optional ARGS)" t nil) (autoload 'magit-merge-nocommit "magit-merge" "Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.

(git merge --no-commit --no-ff [ARGS] REV)

(fn REV &optional ARGS)" t nil) (autoload 'magit-merge-into "magit-merge" "Merge the current branch into BRANCH and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
then also remove the respective remote branch.

(fn BRANCH &optional ARGS)" t nil) (autoload 'magit-merge-absorb "magit-merge" "Merge BRANCH into the current branch and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
then also remove the respective remote branch.

(fn BRANCH &optional ARGS)" t nil) (autoload 'magit-merge-squash "magit-merge" "Squash commit REV into the current branch; don't create a commit.

(git merge --squash REV)

(fn REV)" t nil) (autoload 'magit-merge-preview "magit-merge" "Preview result of merging REV into the current branch.

(fn REV)" t nil) (autoload 'magit-merge-abort "magit-merge" "Abort the current merge operation.

(git merge --abort)" t nil) (register-definition-prefixes "magit-merge" '("magit-")) (register-definition-prefixes "magit-mode" '("disable-magit-save-buffers" "magit-")) (autoload 'magit-notes "magit" nil t) (register-definition-prefixes "magit-notes" '("magit-notes-")) (register-definition-prefixes "magit-obsolete" '("magit--magit-popup-warning")) (autoload 'magit-patch "magit-patch" nil t) (autoload 'magit-patch-create "magit-patch" nil t) (autoload 'magit-patch-apply "magit-patch" nil t) (autoload 'magit-patch-save "magit-patch" "Write current diff into patch FILE.

What arguments are used to create the patch depends on the value
of `magit-patch-save-arguments' and whether a prefix argument is
used.

If the value is the symbol `buffer', then use the same arguments
as the buffer.  With a prefix argument use no arguments.

If the value is a list beginning with the symbol `exclude', then
use the same arguments as the buffer except for those matched by
entries in the cdr of the list.  The comparison is done using
`string-prefix-p'.  With a prefix argument use the same arguments
as the buffer.

If the value is a list of strings (including the empty list),
then use those arguments.  With a prefix argument use the same
arguments as the buffer.

Of course the arguments that are required to actually show the
same differences as those shown in the buffer are always used.

(fn FILE &optional ARG)" t nil) (autoload 'magit-request-pull "magit-patch" "Request upstream to pull from your public repository.

URL is the url of your publicly accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit.

(fn URL START END)" t nil) (register-definition-prefixes "magit-patch" '("magit-")) (register-definition-prefixes "magit-process" '("magit-" "tramp-sh-handle-")) (autoload 'magit-pull "magit-pull" nil t) (autoload 'magit-pull-from-pushremote "magit-pull" nil t) (autoload 'magit-pull-from-upstream "magit-pull" nil t) (autoload 'magit-pull-branch "magit-pull" "Pull from a branch read in the minibuffer.

(fn SOURCE ARGS)" t nil) (register-definition-prefixes "magit-pull" '("magit-pull-")) (autoload 'magit-push "magit-push" nil t) (autoload 'magit-push-current-to-pushremote "magit-push" nil t) (autoload 'magit-push-current-to-upstream "magit-push" nil t) (autoload 'magit-push-current "magit-push" "Push the current branch to a branch read in the minibuffer.

(fn TARGET ARGS)" t nil) (autoload 'magit-push-other "magit-push" "Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer.

(fn SOURCE TARGET ARGS)" t nil) (autoload 'magit-push-refspecs "magit-push" "Push one or multiple REFSPECS to a REMOTE.
Both the REMOTE and the REFSPECS are read in the minibuffer.  To
use multiple REFSPECS, separate them with commas.  Completion is
only available for the part before the colon, or when no colon
is used.

(fn REMOTE REFSPECS ARGS)" t nil) (autoload 'magit-push-matching "magit-push" "Push all matching branches to another repository.
If multiple remotes exist, then read one from the user.
If just one exists, use that without requiring confirmation.

(fn REMOTE &optional ARGS)" t nil) (autoload 'magit-push-tags "magit-push" "Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default.

(fn REMOTE &optional ARGS)" t nil) (autoload 'magit-push-tag "magit-push" "Push a tag to another repository.

(fn TAG REMOTE &optional ARGS)" t nil) (autoload 'magit-push-notes-ref "magit-push" "Push a notes ref to another repository.

(fn REF REMOTE &optional ARGS)" t nil) (autoload 'magit-push-implicitly "magit-push" nil t) (autoload 'magit-push-to-remote "magit-push" "Push to REMOTE without using an explicit refspec.
The REMOTE is read in the minibuffer.

This command simply runs \"git push -v [ARGS] REMOTE\".  ARGS
are the arguments specified in the popup buffer.  No refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

(fn REMOTE ARGS)" t nil) (register-definition-prefixes "magit-push" '("magit-")) (autoload 'magit-reflog-current "magit-reflog" "Display the reflog of the current branch.
If `HEAD' is detached, then show the reflog for that instead." t nil) (autoload 'magit-reflog-other "magit-reflog" "Display the reflog of a branch or another ref.

(fn REF)" t nil) (autoload 'magit-reflog-head "magit-reflog" "Display the `HEAD' reflog." t nil) (register-definition-prefixes "magit-reflog" '("magit-reflog-")) (autoload 'magit-show-refs "magit-refs" nil t) (autoload 'magit-show-refs-head "magit-refs" "List and compare references in a dedicated buffer.
Compared with `HEAD'.

(fn &optional ARGS)" t nil) (autoload 'magit-show-refs-current "magit-refs" "List and compare references in a dedicated buffer.
Compare with the current branch or `HEAD' if it is detached.

(fn &optional ARGS)" t nil) (autoload 'magit-show-refs-other "magit-refs" "List and compare references in a dedicated buffer.
Compared with a branch read from the user.

(fn &optional REF ARGS)" t nil) (register-definition-prefixes "magit-refs" '("magit-")) (autoload 'magit-remote "magit-remote" nil t) (autoload 'magit-remote-add "magit-remote" "Add a remote named REMOTE and fetch it.

(fn REMOTE URL &optional ARGS)" t nil) (autoload 'magit-remote-rename "magit-remote" "Rename the remote named OLD to NEW.

(fn OLD NEW)" t nil) (autoload 'magit-remote-remove "magit-remote" "Delete the remote named REMOTE.

(fn REMOTE)" t nil) (autoload 'magit-remote-prune "magit-remote" "Remove stale remote-tracking branches for REMOTE.

(fn REMOTE)" t nil) (autoload 'magit-remote-prune-refspecs "magit-remote" "Remove stale refspecs for REMOTE.

A refspec is stale if there no longer exists at least one branch
on the remote that would be fetched due to that refspec.  A stale
refspec is problematic because its existence causes Git to refuse
to fetch according to the remaining non-stale refspecs.

If only stale refspecs remain, then offer to either delete the
remote or to replace the stale refspecs with the default refspec.

Also remove the remote-tracking branches that were created due to
the now stale refspecs.  Other stale branches are not removed.

(fn REMOTE)" t nil) (autoload 'magit-remote-set-head "magit-remote" "Set the local representation of REMOTE's default branch.
Query REMOTE and set the symbolic-ref refs/remotes/<remote>/HEAD
accordingly.  With a prefix argument query for the branch to be
used, which allows you to select an incorrect value if you fancy
doing that.

(fn REMOTE &optional BRANCH)" t nil) (autoload 'magit-remote-unset-head "magit-remote" "Unset the local representation of REMOTE's default branch.
Delete the symbolic-ref \"refs/remotes/<remote>/HEAD\".

(fn REMOTE)" t nil) (autoload 'magit-remote-unshallow "magit-remote" "Convert a shallow remote into a full one.
If only a single refspec is set and it does not contain a
wildcard, then also offer to replace it with the standard
refspec.

(fn REMOTE)" t nil) (autoload 'magit-remote-configure "magit-remote" nil t) (register-definition-prefixes "magit-remote" '("magit-")) (autoload 'magit-list-repositories "magit-repos" "Display a list of repositories.

Use the options `magit-repository-directories' to control which
repositories are displayed." t nil) (register-definition-prefixes "magit-repos" '("magit-")) (autoload 'magit-reset "magit" nil t) (autoload 'magit-reset-mixed "magit-reset" "Reset the `HEAD' and index to COMMIT, but not the working tree.

(git reset --mixed COMMIT)

(fn COMMIT)" t nil) (autoload 'magit-reset-soft "magit-reset" "Reset the `HEAD' to COMMIT, but not the index and working tree.

(git reset --soft REVISION)

(fn COMMIT)" t nil) (autoload 'magit-reset-hard "magit-reset" "Reset the `HEAD', index, and working tree to COMMIT.

(git reset --hard REVISION)

(fn COMMIT)" t nil) (autoload 'magit-reset-keep "magit-reset" "Reset the `HEAD' and index to COMMIT, while keeping uncommitted changes.

(git reset --keep REVISION)

(fn COMMIT)" t nil) (autoload 'magit-reset-index "magit-reset" "Reset the index to COMMIT.
Keep the `HEAD' and working tree as-is, so if COMMIT refers to the
head this effectively unstages all changes.

(git reset COMMIT .)

(fn COMMIT)" t nil) (autoload 'magit-reset-worktree "magit-reset" "Reset the worktree to COMMIT.
Keep the `HEAD' and index as-is.

(fn COMMIT)" t nil) (autoload 'magit-reset-quickly "magit-reset" "Reset the `HEAD' and index to COMMIT, and possibly the working tree.
With a prefix argument reset the working tree otherwise don't.

(git reset --mixed|--hard COMMIT)

(fn COMMIT &optional HARD)" t nil) (register-definition-prefixes "magit-reset" '("magit-reset-")) (autoload 'magit-sequencer-continue "magit-sequence" "Resume the current cherry-pick or revert sequence." t nil) (autoload 'magit-sequencer-skip "magit-sequence" "Skip the stopped at commit during a cherry-pick or revert sequence." t nil) (autoload 'magit-sequencer-abort "magit-sequence" "Abort the current cherry-pick or revert sequence.
This discards all changes made since the sequence started." t nil) (autoload 'magit-cherry-pick "magit-sequence" nil t) (autoload 'magit-cherry-copy "magit-sequence" "Copy COMMITS from another branch onto the current branch.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then pick all of them,
without prompting.

(fn COMMITS &optional ARGS)" t nil) (autoload 'magit-cherry-apply "magit-sequence" "Apply the changes in COMMITS but do not commit them.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then apply all of them,
without prompting.

(fn COMMITS &optional ARGS)" t nil) (autoload 'magit-cherry-harvest "magit-sequence" "Move COMMITS from another BRANCH onto the current branch.
Remove the COMMITS from BRANCH and stay on the current branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH &optional ARGS)" t nil) (autoload 'magit-cherry-donate "magit-sequence" "Move COMMITS from the current branch onto another existing BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH &optional ARGS)" t nil) (autoload 'magit-cherry-spinout "magit-sequence" "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH START-POINT &optional ARGS)" t nil) (autoload 'magit-cherry-spinoff "magit-sequence" "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and checkout BRANCH.
If a conflict occurs, then you have to fix that and finish
the process manually.

(fn COMMITS BRANCH START-POINT &optional ARGS)" t nil) (autoload 'magit-revert "magit-sequence" nil t) (autoload 'magit-revert-and-commit "magit-sequence" "Revert COMMIT by creating a new commit.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

(fn COMMIT &optional ARGS)" t nil) (autoload 'magit-revert-no-commit "magit-sequence" "Revert COMMIT by applying it in reverse to the worktree.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

(fn COMMIT &optional ARGS)" t nil) (autoload 'magit-am "magit-sequence" nil t) (autoload 'magit-am-apply-patches "magit-sequence" "Apply the patches FILES.

(fn &optional FILES ARGS)" t nil) (autoload 'magit-am-apply-maildir "magit-sequence" "Apply the patches from MAILDIR.

(fn &optional MAILDIR ARGS)" t nil) (autoload 'magit-am-continue "magit-sequence" "Resume the current patch applying sequence." t nil) (autoload 'magit-am-skip "magit-sequence" "Skip the stopped at patch during a patch applying sequence." t nil) (autoload 'magit-am-abort "magit-sequence" "Abort the current patch applying sequence.
This discards all changes made since the sequence started." t nil) (autoload 'magit-rebase "magit-sequence" nil t) (autoload 'magit-rebase-onto-pushremote "magit-sequence" nil t) (autoload 'magit-rebase-onto-upstream "magit-sequence" nil t) (autoload 'magit-rebase-branch "magit-sequence" "Rebase the current branch onto a branch read in the minibuffer.
All commits that are reachable from `HEAD' but not from the
selected branch TARGET are being rebased.

(fn TARGET ARGS)" t nil) (autoload 'magit-rebase-subset "magit-sequence" "Rebase a subset of the current branch's history onto a new base.
Rebase commits from START to `HEAD' onto NEWBASE.
START has to be selected from a list of recent commits.

(fn NEWBASE START ARGS)" t nil) (autoload 'magit-rebase-interactive "magit-sequence" "Start an interactive rebase sequence.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-autosquash "magit-sequence" "Combine squash and fixup commits with their intended targets.

(fn ARGS)" t nil) (autoload 'magit-rebase-edit-commit "magit-sequence" "Edit a single older commit using rebase.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-reword-commit "magit-sequence" "Reword a single older commit using rebase.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-remove-commit "magit-sequence" "Remove a single older commit using rebase.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-continue "magit-sequence" "Restart the current rebasing operation.
In some cases this pops up a commit message buffer for you do
edit.  With a prefix argument the old message is reused as-is.

(fn &optional NOEDIT)" t nil) (autoload 'magit-rebase-skip "magit-sequence" "Skip the current commit and restart the current rebase operation." t nil) (autoload 'magit-rebase-edit "magit-sequence" "Edit the todo list of the current rebase operation." t nil) (autoload 'magit-rebase-abort "magit-sequence" "Abort the current rebase operation, restoring the original branch." t nil) (register-definition-prefixes "magit-sequence" '("magit-")) (autoload 'magit-stash "magit-stash" nil t) (autoload 'magit-stash-both "magit-stash" "Create a stash of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-stash-index "magit-stash" "Create a stash of the index only.
Unstaged and untracked changes are not stashed.  The stashed
changes are applied in reverse to both the index and the
worktree.  This command can fail when the worktree is not clean.
Applying the resulting stash has the inverse effect.

(fn MESSAGE)" t nil) (autoload 'magit-stash-worktree "magit-stash" "Create a stash of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-stash-keep-index "magit-stash" "Create a stash of the index and working tree, keeping index intact.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-snapshot-both "magit-stash" "Create a snapshot of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-snapshot-index "magit-stash" "Create a snapshot of the index only.
Unstaged and untracked changes are not stashed." t nil) (autoload 'magit-snapshot-worktree "magit-stash" "Create a snapshot of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-stash-apply "magit-stash" "Apply a stash to the working tree.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index.

(fn STASH)" t nil) (autoload 'magit-stash-pop "magit-stash" "Apply a stash to the working tree and remove it from stash list.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index
and forgo removing the stash.

(fn STASH)" t nil) (autoload 'magit-stash-drop "magit-stash" "Remove a stash from the stash list.
When the region is active offer to drop all contained stashes.

(fn STASH)" t nil) (autoload 'magit-stash-clear "magit-stash" "Remove all stashes saved in REF's reflog by deleting REF.

(fn REF)" t nil) (autoload 'magit-stash-branch "magit-stash" "Create and checkout a new BRANCH from STASH.

(fn STASH BRANCH)" t nil) (autoload 'magit-stash-branch-here "magit-stash" "Create and checkout a new BRANCH and apply STASH.
The branch is created using `magit-branch-and-checkout', using the
current branch or `HEAD' as the start-point.

(fn STASH BRANCH)" t nil) (autoload 'magit-stash-format-patch "magit-stash" "Create a patch from STASH

(fn STASH)" t nil) (autoload 'magit-stash-list "magit-stash" "List all stashes in a buffer." t nil) (autoload 'magit-stash-show "magit-stash" "Show all diffs of a stash in a buffer.

(fn STASH &optional ARGS FILES)" t nil) (register-definition-prefixes "magit-stash" '("magit-")) (autoload 'magit-init "magit-status" "Initialize a Git repository, then show its status.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally.

(fn DIRECTORY)" t nil) (autoload 'magit-status "magit-status" "Show the status of the current Git repository in a buffer.

If the current directory isn't located within a Git repository,
then prompt for an existing repository or an arbitrary directory,
depending on option `magit-repository-directories', and show the
status of the selected repository instead.

* If that option specifies any existing repositories, then offer
  those for completion and show the status buffer for the
  selected one.

* Otherwise read an arbitrary directory using regular file-name
  completion.  If the selected directory is the top-level of an
  existing working tree, then show the status buffer for that.

* Otherwise offer to initialize the selected directory as a new
  repository.  After creating the repository show its status
  buffer.

These fallback behaviors can also be forced using one or more
prefix arguments:

* With two prefix arguments (or more precisely a numeric prefix
  value of 16 or greater) read an arbitrary directory and act on
  it as described above.  The same could be accomplished using
  the command `magit-init'.

* With a single prefix argument read an existing repository, or
  if none can be found based on `magit-repository-directories',
  then fall back to the same behavior as with two prefix
  arguments.

(fn &optional DIRECTORY CACHE)" t nil) (defalias 'magit 'magit-status "An alias for `magit-status' for better discoverability.

Instead of invoking this alias for `magit-status' using
\"M-x magit RET\", you should bind a key to `magit-status'
and read the info node `(magit)Getting Started', which
also contains other useful hints.") (autoload 'magit-status-here "magit-status" "Like `magit-status' but with non-nil `magit-status-goto-file-position'." t nil) (autoload 'magit-status-quick "magit-status" "Show the status of the current Git repository, maybe without refreshing.

If the status buffer of the current Git repository exists but
isn't being displayed in the selected frame, then display it
without refreshing it.

If the status buffer is being displayed in the selected frame,
then also refresh it.

Prefix arguments have the same meaning as for `magit-status',
and additionally cause the buffer to be refresh.

To use this function instead of `magit-status', add this to your
init file: (global-set-key (kbd \"C-x g\") 'magit-status-quick)." t nil) (autoload 'magit-status-setup-buffer "magit-status" "

(fn &optional DIRECTORY)" nil nil) (register-definition-prefixes "magit-status" '("magit-")) (autoload 'magit-submodule "magit-submodule" nil t) (autoload 'magit-submodule-add "magit-submodule" nil t) (autoload 'magit-submodule-read-name-for-path "magit-submodule" "

(fn PATH &optional PREFER-SHORT)" nil nil) (autoload 'magit-submodule-register "magit-submodule" nil t) (autoload 'magit-submodule-populate "magit-submodule" nil t) (autoload 'magit-submodule-update "magit-submodule" nil t) (autoload 'magit-submodule-synchronize "magit-submodule" nil t) (autoload 'magit-submodule-unpopulate "magit-submodule" nil t) (autoload 'magit-submodule-remove "magit-submodule" "Unregister MODULES and remove their working directories.

For safety reasons, do not remove the gitdirs and if a module has
uncommitted changes, then do not remove it at all.  If a module's
gitdir is located inside the working directory, then move it into
the gitdir of the superproject first.

With the \"--force\" argument offer to remove dirty working
directories and with a prefix argument offer to delete gitdirs.
Both actions are very dangerous and have to be confirmed.  There
are additional safety precautions in place, so you might be able
to recover from making a mistake here, but don't count on it.

(fn MODULES ARGS TRASH-GITDIRS)" t nil) (autoload 'magit-insert-modules "magit-submodule" "Insert submodule sections.
Hook `magit-module-sections-hook' controls which module sections
are inserted, and option `magit-module-sections-nested' controls
whether they are wrapped in an additional section." nil nil) (autoload 'magit-insert-modules-overview "magit-submodule" "Insert sections for all modules.
For each section insert the path and the output of `git describe --tags',
or, failing that, the abbreviated HEAD commit hash." nil nil) (autoload 'magit-insert-modules-unpulled-from-upstream "magit-submodule" "Insert sections for modules that haven't been pulled from the upstream.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-insert-modules-unpulled-from-pushremote "magit-submodule" "Insert sections for modules that haven't been pulled from the push-remote.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-insert-modules-unpushed-to-upstream "magit-submodule" "Insert sections for modules that haven't been pushed to the upstream.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-insert-modules-unpushed-to-pushremote "magit-submodule" "Insert sections for modules that haven't been pushed to the push-remote.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-list-submodules "magit-submodule" "Display a list of the current repository's submodules." t nil) (register-definition-prefixes "magit-submodule" '("magit-")) (autoload 'magit-subtree "magit-subtree" nil t) (autoload 'magit-subtree-import "magit-subtree" nil t) (autoload 'magit-subtree-export "magit-subtree" nil t) (autoload 'magit-subtree-add "magit-subtree" "Add REF from REPOSITORY as a new subtree at PREFIX.

(fn PREFIX REPOSITORY REF ARGS)" t nil) (autoload 'magit-subtree-add-commit "magit-subtree" "Add COMMIT as a new subtree at PREFIX.

(fn PREFIX COMMIT ARGS)" t nil) (autoload 'magit-subtree-merge "magit-subtree" "Merge COMMIT into the PREFIX subtree.

(fn PREFIX COMMIT ARGS)" t nil) (autoload 'magit-subtree-pull "magit-subtree" "Pull REF from REPOSITORY into the PREFIX subtree.

(fn PREFIX REPOSITORY REF ARGS)" t nil) (autoload 'magit-subtree-push "magit-subtree" "Extract the history of the subtree PREFIX and push it to REF on REPOSITORY.

(fn PREFIX REPOSITORY REF ARGS)" t nil) (autoload 'magit-subtree-split "magit-subtree" "Extract the history of the subtree PREFIX.

(fn PREFIX COMMIT ARGS)" t nil) (register-definition-prefixes "magit-subtree" '("magit-")) (autoload 'magit-tag "magit" nil t) (autoload 'magit-tag-create "magit-tag" "Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.

(git tag [--annotate] NAME REV)

(fn NAME REV &optional ARGS)" t nil) (autoload 'magit-tag-delete "magit-tag" "Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.

(git tag -d TAGS)

(fn TAGS)" t nil) (autoload 'magit-tag-prune "magit-tag" "Offer to delete tags missing locally from REMOTE, and vice versa.

(fn TAGS REMOTE-TAGS REMOTE)" t nil) (autoload 'magit-tag-release "magit-tag" "Create a release tag.

Assume that release tags match `magit-release-tag-regexp'.

First prompt for the name of the new tag using the highest
existing tag as initial input and leaving it to the user to
increment the desired part of the version string.

If `--annotate' is enabled, then prompt for the message of the
new tag.  Base the proposed tag message on the message of the
highest tag, provided that that contains the corresponding
version string and substituting the new version string for that.
Otherwise propose something like \"Foo-Bar 1.2.3\", given, for
example, a TAG \"v1.2.3\" and a repository located at something
like \"/path/to/foo-bar\".

(fn TAG MSG &optional ARGS)" t nil) (register-definition-prefixes "magit-tag" '("magit-")) (register-definition-prefixes "magit-transient" '("magit-")) (autoload 'magit-emacs-Q-command "magit-utils" "Show a shell command that runs an uncustomized Emacs with only Magit loaded.
See info node `(magit)Debugging Tools' for more information." t nil) (autoload 'Info-follow-nearest-node--magit-gitman "magit-utils" "

(fn FN &optional FORK)" nil nil) (advice-add 'Info-follow-nearest-node :around 'Info-follow-nearest-node--magit-gitman) (autoload 'org-man-export--magit-gitman "magit-utils" "

(fn FN LINK DESCRIPTION FORMAT)" nil nil) (advice-add 'org-man-export :around 'org-man-export--magit-gitman) (register-definition-prefixes "magit-utils" '("magit-")) (defvar magit-wip-mode nil "Non-nil if Magit-Wip mode is enabled.
See the `magit-wip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-mode'.") (custom-autoload 'magit-wip-mode "magit-wip" nil) (autoload 'magit-wip-mode "magit-wip" "Save uncommitted changes to work-in-progress refs.

If called interactively, enable Magit-Wip mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Whenever appropriate (i.e. when dataloss would be a possibility
otherwise) this mode causes uncommitted changes to be committed
to dedicated work-in-progress refs.

For historic reasons this mode is implemented on top of four
other `magit-wip-*' modes, which can also be used individually,
if you want finer control over when the wip refs are updated;
but that is discouraged.

(fn &optional ARG)" t nil) (put 'magit-wip-after-save-mode 'globalized-minor-mode t) (defvar magit-wip-after-save-mode nil "Non-nil if Magit-Wip-After-Save mode is enabled.
See the `magit-wip-after-save-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-after-save-mode'.") (custom-autoload 'magit-wip-after-save-mode "magit-wip" nil) (autoload 'magit-wip-after-save-mode "magit-wip" "Toggle Magit-Wip-After-Save-Local mode in all buffers.
With prefix ARG, enable Magit-Wip-After-Save mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-Wip-After-Save-Local mode is enabled in all buffers where
`magit-wip-after-save-local-mode-turn-on' would do it.
See `magit-wip-after-save-local-mode' for more information on Magit-Wip-After-Save-Local mode.

(fn &optional ARG)" t nil) (defvar magit-wip-after-apply-mode nil "Non-nil if Magit-Wip-After-Apply mode is enabled.
See the `magit-wip-after-apply-mode' command
for a description of this minor mode.") (custom-autoload 'magit-wip-after-apply-mode "magit-wip" nil) (autoload 'magit-wip-after-apply-mode "magit-wip" "Commit to work-in-progress refs.

If called interactively, enable Magit-Wip-After-Apply mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

After applying a change using any \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected files to the current wip refs.  For each branch there
may be two wip refs; one contains snapshots of the files as found
in the worktree and the other contains snapshots of the entries
in the index.

(fn &optional ARG)" t nil) (defvar magit-wip-before-change-mode nil "Non-nil if Magit-Wip-Before-Change mode is enabled.
See the `magit-wip-before-change-mode' command
for a description of this minor mode.") (custom-autoload 'magit-wip-before-change-mode "magit-wip" nil) (autoload 'magit-wip-before-change-mode "magit-wip" "Commit to work-in-progress refs before certain destructive changes.

If called interactively, enable Magit-Wip-Before-Change mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Before invoking a revert command or an \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected tracked files to the current wip refs.  For each branch
there may be two wip refs; one contains snapshots of the files
as found in the worktree and the other contains snapshots of the
entries in the index.

Only changes to files which could potentially be affected by the
command which is about to be called are committed.

(fn &optional ARG)" t nil) (autoload 'magit-wip-commit-initial-backup "magit-wip" "Before saving, commit current file to a worktree wip ref.

The user has to add this function to `before-save-hook'.

Commit the current state of the visited file before saving the
current buffer to that file.  This backs up the same version of
the file as `backup-buffer' would, but stores the backup in the
worktree wip ref, which is also used by the various Magit Wip
modes, instead of in a backup file as `backup-buffer' would.

This function ignores the variables that affect `backup-buffer'
and can be used along-side that function, which is recommended
because this function only backs up files that are tracked in
a Git repository." nil nil) (register-definition-prefixes "magit-wip" '("magit-")) (autoload 'magit-worktree "magit-worktree" nil t) (autoload 'magit-worktree-checkout "magit-worktree" "Checkout BRANCH in a new worktree at PATH.

(fn PATH BRANCH)" t nil) (autoload 'magit-worktree-branch "magit-worktree" "Create a new BRANCH and check it out in a new worktree at PATH.

(fn PATH BRANCH START-POINT &optional FORCE)" t nil) (autoload 'magit-worktree-move "magit-worktree" "Move WORKTREE to PATH.

(fn WORKTREE PATH)" t nil) (register-definition-prefixes "magit-worktree" '("magit-")) (provide 'magit-autoloads)) "async" ((smtpmail-async dired-async async async-bytecomp async-autoloads) (autoload 'async-start-process "async" "Start the executable PROGRAM asynchronously named NAME.  See `async-start'.
PROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the
process object when done.  If FINISH-FUNC is nil, the future
object will return the process object when the program is
finished.  Set DEFAULT-DIRECTORY to change PROGRAM's current
working directory.

(fn NAME PROGRAM FINISH-FUNC &rest PROGRAM-ARGS)" nil nil) (autoload 'async-start "async" "Execute START-FUNC (often a lambda) in a subordinate Emacs process.
When done, the return value is passed to FINISH-FUNC.  Example:

    (async-start
       ;; What to do in the child process
       (lambda ()
         (message \"This is a test\")
         (sleep-for 3)
         222)

       ;; What to do when it finishes
       (lambda (result)
         (message \"Async process done, result should be 222: %s\"
                  result)))

If FINISH-FUNC is nil or missing, a future is returned that can
be inspected using `async-get', blocking until the value is
ready.  Example:

    (let ((proc (async-start
                   ;; What to do in the child process
                   (lambda ()
                     (message \"This is a test\")
                     (sleep-for 3)
                     222))))

        (message \"I'm going to do some work here\") ;; ....

        (message \"Waiting on async process, result should be 222: %s\"
                 (async-get proc)))

If you don't want to use a callback, and you don't care about any
return value from the child process, pass the `ignore' symbol as
the second argument (if you don't, and never call `async-get', it
will leave *emacs* process buffers hanging around):

    (async-start
     (lambda ()
       (delete-file \"a remote file on a slow link\" nil))
     \\='ignore)

Special case:
If the output of START-FUNC is a string with properties
e.g. (buffer-string) RESULT will be transformed in a list where the
car is the string itself (without props) and the cdr the rest of
properties, this allows using in FINISH-FUNC the string without
properties and then apply the properties in cdr to this string (if
needed).
Properties handling special objects like markers are returned as
list to allow restoring them later.
See <https://github.com/jwiegley/emacs-async/issues/145> for more infos.

Note: Even when FINISH-FUNC is present, a future is still
returned except that it yields no value (since the value is
passed to FINISH-FUNC).  Call `async-get' on such a future always
returns nil.  It can still be useful, however, as an argument to
`async-ready' or `async-wait'.

(fn START-FUNC &optional FINISH-FUNC)" nil nil) (register-definition-prefixes "async" '("async-")) (autoload 'async-byte-recompile-directory "async-bytecomp" "Compile all *.el files in DIRECTORY asynchronously.
All *.elc files are systematically deleted before proceeding.

(fn DIRECTORY &optional QUIET)" nil nil) (defvar async-bytecomp-package-mode nil "Non-nil if Async-Bytecomp-Package mode is enabled.
See the `async-bytecomp-package-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `async-bytecomp-package-mode'.") (custom-autoload 'async-bytecomp-package-mode "async-bytecomp" nil) (autoload 'async-bytecomp-package-mode "async-bytecomp" "Byte compile asynchronously packages installed with package.el.
Async compilation of packages can be controlled by
`async-bytecomp-allowed-packages'.

If called interactively, enable Async-Bytecomp-Package mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'async-byte-compile-file "async-bytecomp" "Byte compile Lisp code FILE asynchronously.

Same as `byte-compile-file' but asynchronous.

(fn FILE)" t nil) (register-definition-prefixes "async-bytecomp" '("async-")) (defvar dired-async-mode nil "Non-nil if Dired-Async mode is enabled.
See the `dired-async-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dired-async-mode'.") (custom-autoload 'dired-async-mode "dired-async" nil) (autoload 'dired-async-mode "dired-async" "Do dired actions asynchronously.

If called interactively, enable Dired-Async mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-copy "dired-async" "Run \342\200\230dired-do-copy\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-symlink "dired-async" "Run \342\200\230dired-do-symlink\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-hardlink "dired-async" "Run \342\200\230dired-do-hardlink\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (autoload 'dired-async-do-rename "dired-async" "Run \342\200\230dired-do-rename\342\200\231 asynchronously.

(fn &optional ARG)" t nil) (register-definition-prefixes "dired-async" '("dired-async-")) (register-definition-prefixes "smtpmail-async" '("async-smtpmail-")) (provide 'async-autoloads)) "popup" ((popup-autoloads popup) (register-definition-prefixes "popup" '("popup-")) (provide 'popup-autoloads)) "helm-core" ((helm-lib helm-source helm-core-autoloads helm-multi-match helm-core-pkg helm) (autoload 'helm-configuration "helm" "Customize Helm." t nil) (autoload 'helm-define-multi-key "helm" "In KEYMAP, define key sequence KEY for function list FUNCTIONS.
Each function runs sequentially for each KEY press.
If DELAY is specified, switch back to initial function of FUNCTIONS list
after DELAY seconds.
The functions in FUNCTIONS list take no args.
E.g.
    (defun foo ()
      (interactive)
      (message \"Run foo\"))
    (defun bar ()
      (interactive)
      (message \"Run bar\"))
    (defun baz ()
      (interactive)
      (message \"Run baz\"))

(helm-define-multi-key global-map (kbd \"<f5> q\") '(foo bar baz) 2)

Each time \"<f5> q\" is pressed, the next function is executed.
Waiting more than 2 seconds between key presses switches back to
executing the first function on the next hit.

(fn KEYMAP KEY FUNCTIONS &optional DELAY)" nil nil) (autoload 'helm-multi-key-defun "helm" "Define NAME as a multi-key command running FUNS.
After DELAY seconds, the FUNS list is reinitialized.
See `helm-define-multi-key'.

(fn NAME DOCSTRING FUNS &optional DELAY)" nil t) (function-put 'helm-multi-key-defun 'lisp-indent-function '2) (autoload 'helm-define-key-with-subkeys "helm" "Define in MAP a KEY and SUBKEY to COMMAND.

This allows typing KEY to call COMMAND the first time and
type only SUBKEY on subsequent calls.

Arg MAP is the keymap to use, SUBKEY is the initial short
key binding to call COMMAND.

Arg OTHER-SUBKEYS is an alist specifying other short key bindings
to use once started, e.g.:

    (helm-define-key-with-subkeys global-map
       (kbd \"C-x v n\") ?n 'git-gutter:next-hunk
       '((?p . git-gutter:previous-hunk)))

In this example, `C-x v n' will run `git-gutter:next-hunk'
subsequent \"n\" will run this command again and subsequent \"p\"
will run `git-gutter:previous-hunk'.

If specified PROMPT can be displayed in minibuffer to describe
SUBKEY and OTHER-SUBKEYS.  Arg EXIT-FN specifies a function to run
on exit.

For any other key pressed, run their assigned command as defined
in MAP and then exit the loop running EXIT-FN, if specified.

If DELAY an integer is specified exit after DELAY seconds.

NOTE: SUBKEY and OTHER-SUBKEYS bindings support only char syntax
and vectors, so don't use strings to define them.

(fn MAP KEY SUBKEY COMMAND &optional OTHER-SUBKEYS PROMPT EXIT-FN DELAY)" nil nil) (function-put 'helm-define-key-with-subkeys 'lisp-indent-function '1) (autoload 'helm-debug-open-last-log "helm" "Open Helm log file or buffer of last Helm session." t nil) (autoload 'helm "helm" "Main function to execute helm sources.

PLIST is a list like

(:key1 val1 :key2 val2 ...)

 or

(&optional sources input prompt resume preselect
            buffer keymap default history allow-nest).

** Keywords

Keywords supported:

- :sources
- :input
- :prompt
- :resume
- :preselect
- :buffer
- :keymap
- :default
- :history
- :allow-nest

Extra LOCAL-VARS keywords are supported, see the \"** Other
keywords\" section below.

Basic keywords are the following:

*** :sources

One of the following:

- List of sources
- Symbol whose value is a list of sources
- Alist representing a Helm source.
  - In this case the source has no name and is referenced in
    `helm-sources' as a whole alist.

*** :input

Initial input of minibuffer (temporary value of `helm-pattern')

*** :prompt

Minibuffer prompt. Default value is `helm--prompt'.

*** :resume

If t, allow resumption of the previous session of this Helm
command, skipping initialization.

If 'noresume, this instance of `helm' cannot be resumed.

*** :preselect

Initially selected candidate (string or regexp).

*** :buffer

Buffer name for this Helm session. `helm-buffer' will take this value.

*** :keymap

[Obsolete]

Keymap used at the start of this Helm session.

It is overridden by keymaps specified in sources, and is kept
only for backward compatibility.

Keymaps should be specified in sources using the :keymap slot
instead. See `helm-source'.

This keymap is not restored by `helm-resume'.

*** :default

Default value inserted into the minibuffer with
\\<minibuffer-local-map>\\[next-history-element].

It can be a string or a list of strings, in this case
\\<minibuffer-local-map>\\[next-history-element] cycles through
the list items, starting with the first.

If nil, `thing-at-point' is used.

If `helm-maybe-use-default-as-input' is non-nil, display is
updated using this value if this value matches, otherwise it is
ignored. If :input is specified, it takes precedence on :default.

*** :history

Minibuffer input, by default, is pushed to `minibuffer-history'.

When an argument HISTORY is provided, input is pushed to
HISTORY. HISTORY should be a valid symbol.

*** :allow-nest

Allow running this Helm command in a running Helm session.

** Other keywords

Other keywords are interpreted as local variables of this Helm
session. The `helm-' prefix can be omitted. For example,

(helm :sources 'helm-source-buffers-list
       :buffer \"*helm buffers*\"
       :candidate-number-limit 10)

Starts a Helm session with the variable
`helm-candidate-number-limit' set to 10.

** Backward compatibility

For backward compatibility, positional parameters are
supported:

(helm sources input prompt resume preselect
       buffer keymap default history allow-nest)

However, the use of non-keyword args is deprecated.

(fn &key SOURCES INPUT PROMPT RESUME PRESELECT BUFFER KEYMAP DEFAULT HISTORY ALLOW-NEST OTHER-LOCAL-VARS)" nil nil) (autoload 'helm-cycle-resume "helm" "Cycle in `helm-buffers' list and resume when waiting more than 1.2s." t nil) (autoload 'helm-other-buffer "helm" "Simplified Helm interface with other `helm-buffer'.
Call `helm' only with SOURCES and BUFFER as args.

(fn SOURCES BUFFER)" nil nil) (register-definition-prefixes "helm" '("helm-" "with-helm-")) (register-definition-prefixes "helm-lib" '("helm-" "with-helm-")) (register-definition-prefixes "helm-multi-match" '("helm-m")) (register-definition-prefixes "helm-source" '("helm-")) (provide 'helm-core-autoloads)) "helm" ((helm-x-files helm-command helm-man helm-semantic helm-eshell helm-utils helm-global-bindings helm-elisp-package helm-bookmark helm-locate helm-external \.dir-locals helm-easymenu helm-imenu helm-font helm-help helm-mode helm-comint helm-config helm-ring helm-adaptive helm-pkg helm-types helm-sys helm-find helm-tags helm-files helm-regexp helm-misc helm-shell helm-color helm-epa helm-elisp helm-autoloads helm-for-files helm-info helm-dabbrev helm-fd helm-buffers helm-grep helm-eval helm-net helm-id-utils helm-occur) (defvar helm-adaptive-mode nil "Non-nil if Helm-Adaptive mode is enabled.
See the `helm-adaptive-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-adaptive-mode'.") (custom-autoload 'helm-adaptive-mode "helm-adaptive" nil) (autoload 'helm-adaptive-mode "helm-adaptive" "Toggle adaptive sorting in all sources.

If called interactively, enable Helm-Adaptive mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'helm-reset-adaptive-history "helm-adaptive" "Delete all `helm-adaptive-history' and his file.
Useful when you have a old or corrupted
`helm-adaptive-history-file'." t nil) (register-definition-prefixes "helm-adaptive" '("helm-adapt")) (autoload 'helm-bookmarks "helm-bookmark" "Preconfigured `helm' for bookmarks." t nil) (autoload 'helm-filtered-bookmarks "helm-bookmark" "Preconfigured `helm' for bookmarks (filtered by category).
Optional source `helm-source-bookmark-addressbook' is loaded only
if external addressbook-bookmark package is installed." t nil) (register-definition-prefixes "helm-bookmark" '("bmkext-jump-" "bookmark" "helm-")) (autoload 'helm-buffers-list "helm-buffers" "Preconfigured `helm' to list buffers." t nil) (autoload 'helm-mini "helm-buffers" "Preconfigured `helm' displaying `helm-mini-default-sources'." t nil) (register-definition-prefixes "helm-buffers" '("helm-")) (autoload 'helm-colors "helm-color" "Preconfigured `helm' for color." t nil) (register-definition-prefixes "helm-color" '("helm-")) (autoload 'helm-comint-prompts "helm-comint" "Pre-configured `helm' to browse the prompts of the current comint buffer." t nil) (autoload 'helm-comint-prompts-all "helm-comint" "Pre-configured `helm' to browse the prompts of all comint sessions." t nil) (autoload 'helm-comint-input-ring "helm-comint" "Preconfigured `helm' that provide completion of `comint' history." t nil) (register-definition-prefixes "helm-comint" '("helm-")) (autoload 'helm-M-x "helm-command" "Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x'
`execute-extended-command'.

Unlike regular `M-x' Emacs vanilla `execute-extended-command'
command, the prefix args if needed, can be passed AFTER starting
`helm-M-x'.  When a prefix arg is passed BEFORE starting
`helm-M-x', the first `C-u' while in `helm-M-x' session will
disable it.

You can get help on each command by persistent action.

(fn ARG)" t nil) (register-definition-prefixes "helm-command" '("helm-")) (autoload 'helm-dabbrev "helm-dabbrev" "Preconfigured helm for dynamic abbreviations." t nil) (register-definition-prefixes "helm-dabbrev" '("helm-dabbrev-")) (autoload 'helm-lisp-completion-at-point "helm-elisp" "Preconfigured Helm for Lisp symbol completion at point." t nil) (autoload 'helm-complete-file-name-at-point "helm-elisp" "Preconfigured Helm to complete file name at point.

(fn &optional FORCE)" t nil) (autoload 'helm-lisp-indent "helm-elisp" nil t nil) (autoload 'helm-lisp-completion-or-file-name-at-point "helm-elisp" "Preconfigured Helm to complete Lisp symbol or filename at point.
Filename completion happens if string start after or between a
double quote." t nil) (autoload 'helm-apropos "helm-elisp" "Preconfigured Helm to describe commands, functions, variables and faces.
In non interactives calls DEFAULT argument should be provided as
a string, i.e. the `symbol-name' of any existing symbol.

(fn DEFAULT)" t nil) (autoload 'helm-manage-advice "helm-elisp" "Preconfigured `helm' to disable/enable function advices." t nil) (autoload 'helm-locate-library "helm-elisp" "Preconfigured helm to locate elisp libraries." t nil) (autoload 'helm-timers "helm-elisp" "Preconfigured `helm' for timers." t nil) (autoload 'helm-complex-command-history "helm-elisp" "Preconfigured `helm' for complex command history." t nil) (register-definition-prefixes "helm-elisp" '("helm-" "with-helm-show-completion")) (autoload 'helm-list-elisp-packages "helm-elisp-package" "Preconfigured `helm' for listing and handling Emacs packages.

(fn ARG)" t nil) (autoload 'helm-list-elisp-packages-no-fetch "helm-elisp-package" "Preconfigured Helm for Emacs packages.

Same as `helm-list-elisp-packages' but don't fetch packages on
remote.  Called with a prefix ARG always fetch packages on
remote.

(fn ARG)" t nil) (register-definition-prefixes "helm-elisp-package" '("helm-")) (defvar helm-epa-mode nil "Non-nil if Helm-Epa mode is enabled.
See the `helm-epa-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-epa-mode'.") (custom-autoload 'helm-epa-mode "helm-epa" nil) (autoload 'helm-epa-mode "helm-epa" "Enable helm completion on gpg keys in epa functions.

If called interactively, enable Helm-Epa mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'helm-epa-list-keys "helm-epa" "List all gpg keys.
This is the helm interface for `epa-list-keys'." t nil) (register-definition-prefixes "helm-epa" '("helm-epa")) (autoload 'helm-esh-pcomplete "helm-eshell" "Preconfigured `helm' to provide Helm completion in Eshell." t nil) (autoload 'helm-eshell-history "helm-eshell" "Preconfigured Helm for Eshell history." t nil) (autoload 'helm-eshell-prompts "helm-eshell" "Pre-configured `helm' to browse the prompts of the current Eshell." t nil) (autoload 'helm-eshell-prompts-all "helm-eshell" "Pre-configured `helm' to browse the prompts of all Eshell sessions." t nil) (register-definition-prefixes "helm-eshell" '("helm-e")) (autoload 'helm-eval-expression "helm-eval" "Preconfigured `helm' for `helm-source-evaluation-result'.

(fn ARG)" t nil) (autoload 'helm-eval-expression-with-eldoc "helm-eval" "Preconfigured `helm' for `helm-source-evaluation-result' with `eldoc' support." t nil) (autoload 'helm-calcul-expression "helm-eval" "Preconfigured `helm' for `helm-source-calculation-result'." t nil) (register-definition-prefixes "helm-eval" '("helm-")) (autoload 'helm-run-external-command "helm-external" "Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running try to run `helm-raise-command' if
defined otherwise exit with error. You can set your own list of
commands with `helm-external-commands-list'." t nil) (register-definition-prefixes "helm-external" '("helm-")) (register-definition-prefixes "helm-fd" '("helm-fd-")) (autoload 'helm-projects-history "helm-files" "

(fn ARG)" t nil) (autoload 'helm-browse-project "helm-files" "Preconfigured helm to browse projects.
Browse files and see status of project with its VCS.
Only HG and GIT are supported for now.
Fall back to `helm-browse-project-find-files' if current
directory is not under control of one of those VCS.
With a prefix ARG browse files recursively, with two prefix ARG
rebuild the cache.
If the current directory is found in the cache, start
`helm-browse-project-find-files' even with no prefix ARG.
NOTE: The prefix ARG have no effect on the VCS controlled
directories.

Needed dependencies for VCS:
<https://github.com/emacs-helm/helm-ls-git>
and
<https://github.com/emacs-helm/helm-ls-hg>.

(fn ARG)" t nil) (autoload 'helm-find-files "helm-files" "Preconfigured `helm' for helm implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `helm-find-files-1' instead.
This is the starting point for nearly all actions you can do on
files.

(fn ARG)" t nil) (autoload 'helm-delete-tramp-connection "helm-files" "Allow deleting tramp connection or marked tramp connections at once.

This replace `tramp-cleanup-connection' which is partially broken
in Emacs < to 25.1.50.1 (See Emacs bug http://debbugs.gnu.org/cgi/bugreport.cgi?bug=24432).

It allows additionally to delete more than one connection at
once." t nil) (register-definition-prefixes "helm-files" '("eshell-command-aliases-list" "helm-")) (autoload 'helm-find "helm-find" "Preconfigured `helm' for the find shell command.

Recursively find files whose names are matched by all specified
globbing PATTERNs under the current directory using the external
program specified in `find-program' (usually \"find\").  Every
input PATTERN is silently wrapped into two stars: *PATTERN*.

With prefix argument, prompt for a directory to search.

When user option `helm-findutils-search-full-path' is non-nil,
match against complete paths, otherwise, against file names
without directory part.

The (possibly empty) list of globbing PATTERNs can be followed by
the separator \"*\" plus any number of additional arguments that
are passed to \"find\" literally.

(fn ARG)" t nil) (register-definition-prefixes "helm-find" '("helm-")) (autoload 'helm-select-xfont "helm-font" "Preconfigured `helm' to select Xfont." t nil) (autoload 'helm-ucs "helm-font" "Preconfigured `helm' for `ucs-names'.

Called with a prefix arg force reloading cache.

(fn ARG)" t nil) (register-definition-prefixes "helm-font" '("helm-")) (autoload 'helm-for-files "helm-for-files" "Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-preferred-list'." t nil) (autoload 'helm-multi-files "helm-for-files" "Preconfigured helm like `helm-for-files' but running locate only on demand.

Allow toggling back and forth from locate to others sources with
`helm-multi-files-toggle-locate-binding' key.
This avoids launching locate needlessly when what you are
searching for is already found." t nil) (autoload 'helm-recentf "helm-for-files" "Preconfigured `helm' for `recentf'." t nil) (register-definition-prefixes "helm-for-files" '("helm-")) (register-definition-prefixes "helm-global-bindings" '("helm-command-")) (autoload 'helm-goto-precedent-file "helm-grep" "Go to previous file in Helm grep/etags buffers." t nil) (autoload 'helm-goto-next-file "helm-grep" "Go to previous file in Helm grep/etags buffers." t nil) (autoload 'helm-do-grep-ag "helm-grep" "Preconfigured `helm' for grepping with AG in `default-directory'.
With prefix arg prompt for type if available with your AG
version.

(fn ARG)" t nil) (autoload 'helm-grep-do-git-grep "helm-grep" "Preconfigured `helm' for git-grepping `default-directory'.
With a prefix arg ARG git-grep the whole repository.

(fn ARG)" t nil) (register-definition-prefixes "helm-grep" '("helm-")) (autoload 'helm-documentation "helm-help" "Preconfigured `helm' for Helm documentation.
With a prefix arg refresh the documentation.

Find here the documentation of all documented sources." t nil) (defvar helm-comp-read-mode-line "\\<helm-comp-read-map>C/\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-help]:Help \\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend \\[helm-customize-group]:Conf") (defvar helm-read-file-name-mode-line-string "\\<helm-read-file-map>\\[helm-help]:Help C/\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend \\[helm-customize-group]:Conf" "String displayed in mode-line in `helm-source-find-files'.") (defvar helm-top-mode-line "\\<helm-top-map>\\[helm-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend \\[helm-customize-group]:Conf") (register-definition-prefixes "helm-help" '("helm-")) (autoload 'helm-gid "helm-id-utils" "Preconfigured `helm' for `gid' command line of `ID-Utils'.
Need A database created with the command `mkid' above
`default-directory'.
Need id-utils as dependency which provide `mkid', `gid' etc..
See <https://www.gnu.org/software/idutils/>." t nil) (register-definition-prefixes "helm-id-utils" '("helm-gid-")) (autoload 'helm-imenu "helm-imenu" "Preconfigured `helm' for `imenu'." t nil) (autoload 'helm-imenu-in-all-buffers "helm-imenu" "Preconfigured `helm' for fetching imenu entries in all buffers with similar mode as current.
A mode is similar as current if it is the same, it is derived
i.e. `derived-mode-p' or it have an association in
`helm-imenu-all-buffer-assoc'." t nil) (register-definition-prefixes "helm-imenu" '("helm-")) (autoload 'helm-info "helm-info" "Preconfigured `helm' for searching Info files' indices.

With a prefix argument \\[universal-argument], set REFRESH to
non-nil.

Optional parameter REFRESH, when non-nil, re-evaluates
`helm-default-info-index-list'.  If the variable has been
customized, set it to its saved value.  If not, set it to its
standard value. See `custom-reevaluate-setting' for more.

REFRESH is useful when new Info files are installed.  If
`helm-default-info-index-list' has not been customized, the new
Info files are made available.

(fn &optional REFRESH)" t nil) (autoload 'helm-info-at-point "helm-info" "Preconfigured `helm' for searching info at point." t nil) (register-definition-prefixes "helm-info" '("helm-")) (autoload 'helm-projects-find-files "helm-locate" "Find files with locate in `helm-locate-project-list'.
With a prefix arg refresh the database in each project.

(fn UPDATE)" t nil) (autoload 'helm-locate "helm-locate" "Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options and also `helm-locate-command'.

You can specify a local database with prefix argument ARG.
With two prefix arg, refresh the current local db or create it if
it doesn't exists.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'.

(fn ARG)" t nil) (register-definition-prefixes "helm-locate" '("helm-")) (autoload 'helm-man-woman "helm-man" "Preconfigured `helm' for Man and Woman pages.
With a prefix arg reinitialize the cache.

(fn ARG)" t nil) (register-definition-prefixes "helm-man" '("helm-")) (defvar helm-minibuffer-history-mode nil "Non-nil if Helm-Minibuffer-History mode is enabled.
See the `helm-minibuffer-history-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-minibuffer-history-mode'.") (custom-autoload 'helm-minibuffer-history-mode "helm-misc" nil) (autoload 'helm-minibuffer-history-mode "helm-misc" "Bind `helm-minibuffer-history-key' in al minibuffer maps.
This mode is enabled by `helm-mode', so there is no need to enable it directly.

If called interactively, enable Helm-Minibuffer-History mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'helm-world-time "helm-misc" "Preconfigured `helm' to show world time.
Default action change TZ environment variable locally to emacs." t nil) (autoload 'helm-insert-latex-math "helm-misc" "Preconfigured helm for latex math symbols completion." t nil) (autoload 'helm-ratpoison-commands "helm-misc" "Preconfigured `helm' to execute ratpoison commands." t nil) (autoload 'helm-stumpwm-commands "helm-misc" "Preconfigured helm for stumpwm commands." t nil) (autoload 'helm-minibuffer-history "helm-misc" "Preconfigured `helm' for `minibuffer-history'." t nil) (register-definition-prefixes "helm-misc" '("helm-")) (autoload 'helm-comp-read "helm-mode" "Read a string in the minibuffer, with helm completion.

It is helm `completing-read' equivalent.

- PROMPT is the prompt name to use.

- COLLECTION can be a list, alist, vector, obarray or hash-table.
  For alists and hash-tables their car are use as real value of
  candidate unless ALISTP is non-nil.
  It can be also a function that receives three arguments:
  the values string, predicate and t. See `all-completions' for more details.

Keys description:

- TEST: A predicate called with one arg i.e candidate.

- INITIAL-INPUT: Same as input arg in `helm'.

- PRESELECT: See preselect arg of `helm'.

- DEFAULT: This option is used only for compatibility with regular
  Emacs `completing-read' (Same as DEFAULT arg of `completing-read').

- BUFFER: Name of helm-buffer.

- MUST-MATCH: Candidate selected must be one of COLLECTION.

- FUZZY: Enable fuzzy matching.

- REVERSE-HISTORY: When non--nil display history source after current
  source completion.

- REQUIRES-PATTERN: Same as helm attribute, default is 0.

- HISTORY: A symbol where each result will be saved.
  If not specified as a symbol an error will popup.
  When specified, all elements of HISTORY are displayed in
  a special source before or after COLLECTION according to REVERSE-HISTORY.
  The main difference with INPUT-HISTORY is that the result of the
  completion is saved whereas in INPUT-HISTORY it is the minibuffer
  contents which is saved when you exit.
  Don't use the same symbol for INPUT-HISTORY and HISTORY.
  NOTE: As mentionned above this has nothing to do with
  `minibuffer-history-variable', therefore if you want to save this
  history persistently, you will have to add this variable to the
  relevant variable of your favorite tool for persistent emacs session
  i.e. psession, desktop etc...

- RAW-HISTORY: When non-nil do not remove backslashs if some in
  HISTORY candidates.

- INPUT-HISTORY: A symbol. The minibuffer input history will be
  stored there, if nil or not provided, `minibuffer-history'
  will be used instead.  You can navigate in this history with
  `M-p' and `M-n'.
  Don't use the same symbol for INPUT-HISTORY and HISTORY.

- CASE-FOLD: Same as `helm-case-fold-search'.

- PERSISTENT-ACTION: A function called with one arg i.e candidate.

- PERSISTENT-HELP: A string to document PERSISTENT-ACTION.

- MODE-LINE: A string or list to display in mode line.
  Default is `helm-comp-read-mode-line'.

- KEYMAP: A keymap to use in this `helm-comp-read'.
  (the keymap will be shared with history source)

- NAME: The name related to this local source.

- HEADER-NAME: A function to alter NAME, see `helm'.

- EXEC-WHEN-ONLY-ONE: Bound `helm-execute-action-at-once-if-one'
  to non--nil. (possibles values are t or nil).

- VOLATILE: Use volatile attribute.

- SORT: A predicate to give to `sort' e.g `string-lessp'
  Use this only on small data as it is inefficient.
  If you want to sort faster add a sort function to
  FC-TRANSFORMER.
  Note that FUZZY when enabled is already providing a sort function.

- FC-TRANSFORMER: A `filtered-candidate-transformer' function
  or a list of functions.

- HIST-FC-TRANSFORMER: A `filtered-candidate-transformer'
  function for the history source.

- MARKED-CANDIDATES: If non--nil return candidate or marked candidates as a list.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP:
  When non-nil (default) pass the value of (DISPLAY . REAL)
  candidate in COLLECTION to action when COLLECTION is an alist or a
  hash-table, otherwise DISPLAY is always returned as result on exit,
  which is the default when using `completing-read'.
  See `helm-comp-read-get-candidates'.

- CANDIDATES-IN-BUFFER: when non--nil use a source build with
  `helm-source-in-buffer' which is much faster.
  Argument VOLATILE have no effect when CANDIDATES-IN-BUFFER is non--nil.

- MATCH-PART: Allow matching only one part of candidate.
  See match-part documentation in `helm-source'.

- MATCH-DYNAMIC: See match-dynamic in `helm-source-sync'
  It has no effect when used with CANDIDATES-IN-BUFFER.

- ALLOW-NEST: Allow nesting this `helm-comp-read' in a helm session.
  See `helm'.

- MULTILINE: See multiline in `helm-source'.

- COERCE: See coerce in `helm-source'.

- GROUP: See group in `helm-source'.

Any prefix args passed during `helm-comp-read' invocation will be recorded
in `helm-current-prefix-arg', otherwise if prefix args were given before
`helm-comp-read' invocation, the value of `current-prefix-arg' will be used.
That means you can pass prefix args before or after calling a command
that use `helm-comp-read'.  See `helm-M-x' for example.

(fn PROMPT COLLECTION &key TEST INITIAL-INPUT DEFAULT PRESELECT (BUFFER \"*Helm Completions*\") MUST-MATCH FUZZY REVERSE-HISTORY (REQUIRES-PATTERN 0) (HISTORY nil SHISTORY) RAW-HISTORY INPUT-HISTORY (CASE-FOLD helm-comp-read-case-fold-search) (PERSISTENT-ACTION nil) (PERSISTENT-HELP \"DoNothing\") (MODE-LINE helm-comp-read-mode-line) HELP-MESSAGE (KEYMAP helm-comp-read-map) (NAME \"Helm Completions\") HEADER-NAME CANDIDATES-IN-BUFFER MATCH-PART MATCH-DYNAMIC EXEC-WHEN-ONLY-ONE QUIT-WHEN-NO-CAND (VOLATILE t) SORT FC-TRANSFORMER HIST-FC-TRANSFORMER (MARKED-CANDIDATES helm-comp-read-use-marked) NOMARK (ALISTP t) (CANDIDATE-NUMBER-LIMIT helm-candidate-number-limit) MULTILINE ALLOW-NEST COERCE (GROUP \\='helm))" nil nil) (autoload 'helm-read-file-name "helm-mode" "Read a file name with helm completion.
It is helm `read-file-name' emulation.

Argument PROMPT is the default prompt to use.

Keys description:

- NAME: Source name, default to \"Read File Name\".

- INITIAL-INPUT: Where to start reading file name, default to `default-directory' or $HOME.

- BUFFER: `helm-buffer' name, defaults to \"*Helm Completions*\".

- TEST: A predicate called with one arg 'candidate'.

- NORET: Allow disabling helm-ff-RET (have no effect if helm-ff-RET
                                      isn't bound to RET).

- CASE-FOLD: Same as `helm-case-fold-search'.

- PRESELECT: helm preselection.

- HISTORY: Display HISTORY in a special source.

- MUST-MATCH: Can be 'confirm, nil, or t.

- FUZZY: Enable fuzzy matching when non-nil (Enabled by default).

- MARKED-CANDIDATES: When non--nil return a list of marked candidates.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP: Don't use `all-completions' in history (take effect only on history).

- PERSISTENT-ACTION-IF: a persistent if action function.

- PERSISTENT-HELP: persistent help message.

- MODE-LINE: A mode line message, default is `helm-read-file-name-mode-line-string'.

(fn PROMPT &key (NAME \"Read File Name\") INITIAL-INPUT (BUFFER \"*Helm file completions*\") TEST NORET (CASE-FOLD helm-file-name-case-fold-search) PRESELECT HISTORY MUST-MATCH (FUZZY t) DEFAULT MARKED-CANDIDATES (CANDIDATE-NUMBER-LIMIT helm-ff-candidate-number-limit) NOMARK (ALISTP t) (PERSISTENT-ACTION-IF \\='helm-find-files-persistent-action-if) (PERSISTENT-HELP \"Hit1 Expand Candidate, Hit2 or (C-u) Find file\") (MODE-LINE helm-read-file-name-mode-line-string))" nil nil) (defvar helm-mode nil "Non-nil if Helm mode is enabled.
See the `helm-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-mode'.") (custom-autoload 'helm-mode "helm-mode" nil) (autoload 'helm-mode "helm-mode" "Toggle generic helm completion.

All functions in Emacs that use `completing-read',
`read-file-name', `completion-in-region' and friends will use helm
interface when this mode is turned on.

However you can modify this behavior for functions of your choice
with `helm-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can toggle it with M-x `helm-mode'.

About `ido-mode':
DO NOT enable `ido-everywhere' when using `helm-mode'.  Instead of
using `ido-mode', add the commands where you want to use ido to
`helm-completing-read-handlers-alist' with `ido' as value.

Note: This mode is incompatible with Emacs23.

(fn &optional ARG)" t nil) (register-definition-prefixes "helm-mode" '("helm-")) (autoload 'helm-browse-url-firefox "helm-net" "Same as `browse-url-firefox' but detach from Emacs.

So when you quit Emacs you can keep your Firefox session open and
not be prompted to kill the Firefox process.

NOTE: Probably not supported on some systems (e.g., Windows).

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-opera "helm-net" "Browse URL with Opera browser and detach from Emacs.

So when you quit Emacs you can keep your Opera session open and
not be prompted to kill the Opera process.

NOTE: Probably not supported on some systems (e.g., Windows).

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-chromium "helm-net" "Browse URL with Google Chrome browser.

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-uzbl "helm-net" "Browse URL with uzbl browser.

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-conkeror "helm-net" "Browse URL with conkeror browser.

(fn URL &optional IGNORE)" t nil) (autoload 'helm-browse-url-nyxt "helm-net" "Browse URL with nyxt browser.

(fn URL &optional IGNORE)" t nil) (autoload 'helm-surfraw "helm-net" "Preconfigured `helm' to search PATTERN with search ENGINE.

(fn PATTERN ENGINE)" t nil) (autoload 'helm-google-suggest "helm-net" "Preconfigured `helm' for Google search with Google suggest." t nil) (register-definition-prefixes "helm-net" '("helm-")) (autoload 'helm-occur "helm-occur" "Preconfigured helm for searching lines matching pattern in `current-buffer'.

When `helm-source-occur' is member of
`helm-sources-using-default-as-input' which is the default,
symbol at point is searched at startup.

When a region is marked search only in this region by narrowing.

To search in multiples buffers start from one of the commands listing
buffers (i.e. a helm command using `helm-source-buffers-list' like
`helm-mini') and use the multi occur buffers action.

This is the helm implementation that collect lines matching pattern
like vanilla Emacs `occur' but have nothing to do with it, the search
engine beeing completely different and also much faster." t nil) (autoload 'helm-occur-visible-buffers "helm-occur" "Run helm-occur on all visible buffers in frame." t nil) (autoload 'helm-occur-from-isearch "helm-occur" "Invoke `helm-occur' from isearch.

To use this bind it to a key in `isearch-mode-map'." t nil) (autoload 'helm-multi-occur-from-isearch "helm-occur" "Invoke `helm-multi-occur' from isearch.

With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling
`helm-multi-occur-from-isearch' or during the buffer selection.

To use this bind it to a key in `isearch-mode-map'." t nil) (register-definition-prefixes "helm-occur" '("helm-")) (autoload 'helm-regexp "helm-regexp" "Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp." t nil) (register-definition-prefixes "helm-regexp" '("helm-")) (autoload 'helm-mark-ring "helm-ring" "Preconfigured `helm' for `helm-source-mark-ring'." t nil) (autoload 'helm-global-mark-ring "helm-ring" "Preconfigured `helm' for `helm-source-global-mark-ring'." t nil) (autoload 'helm-all-mark-rings "helm-ring" "Preconfigured `helm' for `helm-source-global-mark-ring' and `helm-source-mark-ring'." t nil) (autoload 'helm-register "helm-ring" "Preconfigured `helm' for Emacs registers." t nil) (autoload 'helm-show-kill-ring "helm-ring" "Preconfigured `helm' for `kill-ring'.
It is drop-in replacement of `yank-pop'.

First call open the kill-ring browser, next calls move to next line." t nil) (autoload 'helm-execute-kmacro "helm-ring" "Preconfigured helm for keyboard macros.
Define your macros with `f3' and `f4'.
See (info \"(emacs) Keyboard Macros\") for detailed infos.
This command is useful when used with persistent action." t nil) (register-definition-prefixes "helm-ring" '("helm-")) (autoload 'helm-semantic "helm-semantic" "Preconfigured `helm' for `semantic'.
If ARG is supplied, pre-select symbol at point instead of current.

(fn ARG)" t nil) (autoload 'helm-semantic-or-imenu "helm-semantic" "Preconfigured helm for `semantic' or `imenu'.
If ARG is supplied, pre-select symbol at point instead of current
semantic tag in scope.

If `semantic-mode' is active in the current buffer, then use
semantic for generating tags, otherwise fall back to `imenu'.
Fill in the symbol at point by default.

(fn ARG)" t nil) (register-definition-prefixes "helm-semantic" '("helm-s")) (defalias 'helm-shell-prompts 'helm-comint-prompts) (defalias 'helm-shell-prompts-all 'helm-comint-prompts-all) (defvar helm-top-poll-mode nil "Non-nil if Helm-Top-Poll mode is enabled.
See the `helm-top-poll-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-top-poll-mode'.") (custom-autoload 'helm-top-poll-mode "helm-sys" nil) (autoload 'helm-top-poll-mode "helm-sys" "Refresh automatically helm top buffer once enabled.

If called interactively, enable Helm-Top-Poll mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'helm-top "helm-sys" "Preconfigured `helm' for top command." t nil) (autoload 'helm-list-emacs-process "helm-sys" "Preconfigured `helm' for Emacs process." t nil) (autoload 'helm-xrandr-set "helm-sys" "Preconfigured helm for xrandr." t nil) (register-definition-prefixes "helm-sys" '("helm-")) (autoload 'helm-etags-select "helm-tags" "Preconfigured helm for etags.
If called with a prefix argument REINIT
or if any of the tag files have been modified, reinitialize cache.

This function aggregates three sources of tag files:

  1) An automatically located file in the parent directories,
     by `helm-etags-get-tag-file'.
  2) `tags-file-name', which is commonly set by `find-tag' command.
  3) `tags-table-list' which is commonly set by `visit-tags-table' command.

(fn REINIT)" t nil) (register-definition-prefixes "helm-tags" '("helm-")) (register-definition-prefixes "helm-types" '("helm-")) (defvar helm-popup-tip-mode nil "Non-nil if Helm-Popup-Tip mode is enabled.
See the `helm-popup-tip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-popup-tip-mode'.") (custom-autoload 'helm-popup-tip-mode "helm-utils" nil) (autoload 'helm-popup-tip-mode "helm-utils" "Show help-echo informations in a popup tip at end of line.

If called interactively, enable Helm-Popup-Tip mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "helm-utils" '("helm-" "with-helm-display-marked-candidates")) (register-definition-prefixes "helm-x-files" '("helm-")) (provide 'helm-autoloads)) "helm-ls-git" ((helm-ls-git helm-ls-git-autoloads) (add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG$" . helm-ls-git-commit-mode)) (autoload 'helm-ls-git-commit-mode "helm-ls-git" "Mode to edit COMMIT_EDITMSG files.

Commands:
\\{helm-ls-git-commit-mode-map}

(fn)" t nil) (add-to-list 'auto-mode-alist '("/git-rebase-todo$" . helm-ls-git-rebase-todo-mode)) (autoload 'helm-ls-git-rebase-todo-mode "helm-ls-git" "Major Mode to edit git-rebase-todo files when using git rebase -i.

Commands:
\\{helm-ls-git-rebase-todo-mode-map}

(fn)" t nil) (autoload 'helm-ls-git "helm-ls-git" "

(fn &optional ARG)" t nil) (register-definition-prefixes "helm-ls-git" '("helm-")) (provide 'helm-ls-git-autoloads)) "git-gutter" ((git-gutter-autoloads git-gutter) (autoload 'git-gutter:linum-setup "git-gutter" "Setup for linum-mode." nil nil) (autoload 'git-gutter-mode "git-gutter" "Git-Gutter mode

If called interactively, enable Git-Gutter mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t nil) (put 'global-git-gutter-mode 'globalized-minor-mode t) (defvar global-git-gutter-mode nil "Non-nil if Global Git-Gutter mode is enabled.
See the `global-git-gutter-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-git-gutter-mode'.") (custom-autoload 'global-git-gutter-mode "git-gutter" nil) (autoload 'global-git-gutter-mode "git-gutter" "Toggle Git-Gutter mode in all buffers.
With prefix ARG, enable Global Git-Gutter mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Git-Gutter mode is enabled in all buffers where
`git-gutter--turn-on' would do it.
See `git-gutter-mode' for more information on Git-Gutter mode.

(fn &optional ARG)" t nil) (autoload 'git-gutter "git-gutter" "Show diff information in gutter" t nil) (autoload 'git-gutter:toggle "git-gutter" "Toggle to show diff information." t nil) (register-definition-prefixes "git-gutter" '("git-gutter")) (provide 'git-gutter-autoloads)) "projectile" ((projectile-autoloads projectile) (autoload 'projectile-version "projectile" "Get the Projectile version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

(fn &optional SHOW-VERSION)" t nil) (autoload 'projectile-invalidate-cache "projectile" "Remove the current project's files from `projectile-projects-cache'.

With a prefix argument PROMPT prompts for the name of the project whose cache
to invalidate.

(fn PROMPT)" t nil) (autoload 'projectile-purge-file-from-cache "projectile" "Purge FILE from the cache of the current project.

(fn FILE)" t nil) (autoload 'projectile-purge-dir-from-cache "projectile" "Purge DIR from the cache of the current project.

(fn DIR)" t nil) (autoload 'projectile-cache-current-file "projectile" "Add the currently visited file to the cache." t nil) (autoload 'projectile-discover-projects-in-directory "projectile" "Discover any projects in DIRECTORY and add them to the projectile cache.

If DEPTH is non-nil recursively descend exactly DEPTH levels below DIRECTORY and
discover projects there.

(fn DIRECTORY &optional DEPTH)" nil nil) (autoload 'projectile-discover-projects-in-search-path "projectile" "Discover projects in `projectile-project-search-path'.
Invoked automatically when `projectile-mode' is enabled." t nil) (autoload 'projectile-switch-to-buffer "projectile" "Switch to a project buffer." t nil) (autoload 'projectile-switch-to-buffer-other-window "projectile" "Switch to a project buffer and show it in another window." t nil) (autoload 'projectile-switch-to-buffer-other-frame "projectile" "Switch to a project buffer and show it in another frame." t nil) (autoload 'projectile-display-buffer "projectile" "Display a project buffer in another window without selecting it." t nil) (autoload 'projectile-project-buffers-other-buffer "projectile" "Switch to the most recently selected buffer project buffer.
Only buffers not visible in windows are returned." t nil) (autoload 'projectile-multi-occur "projectile" "Do a `multi-occur' in the project's buffers.
With a prefix argument, show NLINES of context.

(fn &optional NLINES)" t nil) (autoload 'projectile-find-other-file "projectile" "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t nil) (autoload 'projectile-find-other-file-other-window "projectile" "Switch between files with the same name but different extensions in other window.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t nil) (autoload 'projectile-find-other-file-other-frame "projectile" "Switch between files with the same name but different extensions in other frame.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t nil) (autoload 'projectile-find-file-dwim "projectile" "Jump to a project's files using completion based on context.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim' still switches to \"projectile/projectile.el\" immediately
 because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename like
 \"projectile/a\", a list of files with character 'a' in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-dwim-other-window "projectile" "Jump to a project's files using completion based on context in other window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-window' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim-other-window' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-dwim-other-frame "projectile" "Jump to a project's files using completion based on context in other frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-frame' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim-other-frame' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file "projectile" "Jump to a project's file using completion.
With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-other-window "projectile" "Jump to a project's file using completion and show it in another window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-other-frame "projectile" "Jump to a project's file using completion and show it in another frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-toggle-project-read-only "projectile" "Toggle project read only." t nil) (autoload 'projectile-find-dir "projectile" "Jump to a project's directory using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-dir-other-window "projectile" "Jump to a project's directory in other window using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-dir-other-frame "projectile" "Jump to a project's directory in other frame using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-test-file "projectile" "Jump to a project's test file using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-related-file-other-window "projectile" "Open related file in other window." t nil) (autoload 'projectile-find-related-file-other-frame "projectile" "Open related file in other frame." t nil) (autoload 'projectile-find-related-file "projectile" "Open related file." t nil) (autoload 'projectile-related-files-fn-groups "projectile" "Generate a related-files-fn which relates as KIND for files in each of GROUPS.

(fn KIND GROUPS)" nil nil) (autoload 'projectile-related-files-fn-extensions "projectile" "Generate a related-files-fn which relates as KIND for files having EXTENSIONS.

(fn KIND EXTENSIONS)" nil nil) (autoload 'projectile-related-files-fn-test-with-prefix "projectile" "Generate a related-files-fn which relates tests and impl for files with EXTENSION based on TEST-PREFIX.

(fn EXTENSION TEST-PREFIX)" nil nil) (autoload 'projectile-related-files-fn-test-with-suffix "projectile" "Generate a related-files-fn which relates tests and impl for files with EXTENSION based on TEST-SUFFIX.

(fn EXTENSION TEST-SUFFIX)" nil nil) (autoload 'projectile-project-info "projectile" "Display info for current project." t nil) (autoload 'projectile-find-implementation-or-test-other-window "projectile" "Open matching implementation or test file in other window." t nil) (autoload 'projectile-find-implementation-or-test-other-frame "projectile" "Open matching implementation or test file in other frame." t nil) (autoload 'projectile-toggle-between-implementation-and-test "projectile" "Toggle between an implementation file and its test file." t nil) (autoload 'projectile-grep "projectile" "Perform rgrep in the project.

With a prefix ARG asks for files (globbing-aware) which to grep in.
With prefix ARG of `-' (such as `M--'), default the files (without prompt),
to `projectile-grep-default-files'.

With REGEXP given, don't query the user for a regexp.

(fn &optional REGEXP ARG)" t nil) (autoload 'projectile-ag "projectile" "Run an ag search with SEARCH-TERM in the project.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

(fn SEARCH-TERM &optional ARG)" t nil) (autoload 'projectile-ripgrep "projectile" "Run a ripgrep (rg) search with `SEARCH-TERM' at current project root.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

This command depends on of the Emacs packages ripgrep or rg being
installed to work.

(fn SEARCH-TERM &optional ARG)" t nil) (autoload 'projectile-regenerate-tags "projectile" "Regenerate the project's [e|g]tags." t nil) (autoload 'projectile-find-tag "projectile" "Find tag in project." t nil) (autoload 'projectile-run-command-in-root "projectile" "Invoke `execute-extended-command' in the project's root." t nil) (autoload 'projectile-run-shell-command-in-root "projectile" "Invoke `shell-command' in the project's root.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER)" t nil) (autoload 'projectile-run-async-shell-command-in-root "projectile" "Invoke `async-shell-command' in the project's root.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER)" t nil) (autoload 'projectile-run-gdb "projectile" "Invoke `gdb' in the project's root." t nil) (autoload 'projectile-run-shell "projectile" "Invoke `shell' in the project's root.

Switch to the project specific shell buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-run-eshell "projectile" "Invoke `eshell' in the project's root.

Switch to the project specific eshell buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-run-ielm "projectile" "Invoke `ielm' in the project's root.

Switch to the project specific ielm buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-run-term "projectile" "Invoke `term' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-run-vterm "projectile" "Invoke `vterm' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-replace "projectile" "Replace literal string in project using non-regexp `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement.

(fn &optional ARG)" t nil) (autoload 'projectile-replace-regexp "projectile" "Replace a regexp in the project using `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement.

(fn &optional ARG)" t nil) (autoload 'projectile-kill-buffers "projectile" "Kill project buffers.

The buffer are killed according to the value of
`projectile-kill-buffers-filter'." t nil) (autoload 'projectile-save-project-buffers "projectile" "Save all project buffers." t nil) (autoload 'projectile-dired "projectile" "Open `dired' at the root of the project." t nil) (autoload 'projectile-dired-other-window "projectile" "Open `dired'  at the root of the project in another window." t nil) (autoload 'projectile-dired-other-frame "projectile" "Open `dired' at the root of the project in another frame." t nil) (autoload 'projectile-vc "projectile" "Open `vc-dir' at the root of the project.

For git projects `magit-status-internal' is used if available.
For hg projects `monky-status' is used if available.

If PROJECT-ROOT is given, it is opened instead of the project
root directory of the current buffer file.  If interactively
called with a prefix argument, the user is prompted for a project
directory to open.

(fn &optional PROJECT-ROOT)" t nil) (autoload 'projectile-recentf "projectile" "Show a list of recently visited files in a project." t nil) (autoload 'projectile-configure-project "projectile" "Run project configure command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-compile-project "projectile" "Run project compilation command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-test-project "projectile" "Run project test command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-install-project "projectile" "Run project install command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-package-project "projectile" "Run project package command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-run-project "projectile" "Run project run command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-repeat-last-command "projectile" "Run last projectile external command.

External commands are: `projectile-configure-project',
`projectile-compile-project', `projectile-test-project',
`projectile-install-project', `projectile-package-project',
and `projectile-run-project'.

If the prefix argument SHOW_PROMPT is non nil, the command can be edited.

(fn SHOW-PROMPT)" t nil) (autoload 'projectile-switch-project "projectile" "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'

(fn &optional ARG)" t nil) (autoload 'projectile-switch-open-project "projectile" "Switch to a project we have currently opened.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'

(fn &optional ARG)" t nil) (autoload 'projectile-find-file-in-directory "projectile" "Jump to a file in a (maybe regular) DIRECTORY.

This command will first prompt for the directory the file is in.

(fn &optional DIRECTORY)" t nil) (autoload 'projectile-find-file-in-known-projects "projectile" "Jump to a file in any of the known projects." t nil) (autoload 'projectile-cleanup-known-projects "projectile" "Remove known projects that don't exist anymore." t nil) (autoload 'projectile-clear-known-projects "projectile" "Clear both `projectile-known-projects' and `projectile-known-projects-file'." t nil) (autoload 'projectile-reset-known-projects "projectile" "Clear known projects and rediscover." t nil) (autoload 'projectile-remove-known-project "projectile" "Remove PROJECT from the list of known projects.

(fn &optional PROJECT)" t nil) (autoload 'projectile-remove-current-project-from-known-projects "projectile" "Remove the current project from the list of known projects." t nil) (autoload 'projectile-add-known-project "projectile" "Add PROJECT-ROOT to the list of known projects.

(fn PROJECT-ROOT)" t nil) (autoload 'projectile-ibuffer "projectile" "Open an IBuffer window showing all buffers in the current project.

Let user choose another project when PROMPT-FOR-PROJECT is supplied.

(fn PROMPT-FOR-PROJECT)" t nil) (autoload 'projectile-commander "projectile" "Execute a Projectile command with a single letter.
The user is prompted for a single character indicating the action to invoke.
The `?' character describes then
available actions.

See `def-projectile-commander-method' for defining new methods." t nil) (autoload 'projectile-browse-dirty-projects "projectile" "Browse dirty version controlled projects.

With a prefix argument, or if CACHED is non-nil, try to use the cached
dirty project list.

(fn &optional CACHED)" t nil) (autoload 'projectile-edit-dir-locals "projectile" "Edit or create a .dir-locals.el file of the project." t nil) (defvar projectile-mode nil "Non-nil if Projectile mode is enabled.
See the `projectile-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `projectile-mode'.") (custom-autoload 'projectile-mode "projectile" nil) (autoload 'projectile-mode "projectile" "Minor mode to assist project management and navigation.

When called interactively, toggle `projectile-mode'.  With prefix
ARG, enable `projectile-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `projectile-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `projectile-mode'.
Otherwise behave as if called interactively.

\\{projectile-mode-map}

(fn &optional ARG)" t nil) (define-obsolete-function-alias 'projectile-global-mode 'projectile-mode "1.0") (register-definition-prefixes "projectile" '("??" "compilation-find-file-projectile-find-compilation-buffer" "def-projectile-commander-method" "delete-file-projectile-remove-from-cache" "projectile-")) (provide 'projectile-autoloads)) "helm-projectile" ((helm-projectile-autoloads helm-projectile) (defvar helm-projectile-fuzzy-match t "Enable fuzzy matching for Helm Projectile commands.
This needs to be set before loading helm-projectile.el.") (custom-autoload 'helm-projectile-fuzzy-match "helm-projectile" t) (autoload 'helm-projectile-find-file-dwim "helm-projectile" "Find file at point based on context." t nil) (autoload 'helm-projectile-find-other-file "helm-projectile" "Switch between files with the same name but different extensions using Helm.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t nil) (autoload 'helm-projectile-on "helm-projectile" "Turn on `helm-projectile' key bindings." t nil) (autoload 'helm-projectile-off "helm-projectile" "Turn off `helm-projectile' key bindings." t nil) (autoload 'helm-projectile-grep "helm-projectile" "Helm version of `projectile-grep'.
DIR is the project root, if not set then current directory is used

(fn &optional DIR)" t nil) (autoload 'helm-projectile-ack "helm-projectile" "Helm version of projectile-ack.

(fn &optional DIR)" t nil) (autoload 'helm-projectile-ag "helm-projectile" "Helm version of `projectile-ag'.

(fn &optional OPTIONS)" t nil) (autoload 'helm-projectile-rg "helm-projectile" "Projectile version of `helm-rg'." t nil) (autoload 'helm-projectile-toggle "helm-projectile" "Toggle Helm version of Projectile commands.

(fn TOGGLE)" nil nil) (autoload 'helm-projectile "helm-projectile" "Use projectile with Helm instead of ido.

With a prefix ARG invalidates the cache first.
If invoked outside of a project, displays a list of known projects to jump.

(fn &optional ARG)" t nil) (eval-after-load 'projectile '(progn (define-key projectile-command-map (kbd "h") #'helm-projectile))) (register-definition-prefixes "helm-projectile" '("helm-")) (provide 'helm-projectile-autoloads)) "org" ((ob-lua ol-bbdb org-refile ol-w3m ob-matlab ob-sed ol-doi org-crypt org-lint org-timer ob-lisp ob-ditaa ob-perl ob-calc org-faces ob-sass ox-ascii ob-groovy org-macs ob-screen org-footnote ox-beamer org-colview ob-css ox ob-lob ob-plantuml ox-icalendar ob-R org-id ol-eww ob-julia ob-exp ol-bibtex org-keys org-entities org-tempo org org-attach ob-octave ob-haskell org-attach-git org-mobile oc-csl org-clock org-macro org-element ob-awk ob-js org-indent org-table ol-docview oc org-loaddefs org-agenda org-feed ob-processing org-compat ob-eval ob-gnuplot ox-man ob-ruby org-goto org-pcomplete ob-org ob-java ob-C ob-sql ob-eshell ob-latex ol-gnus ob-table ob-core oc-bibtex ob-ref ox-html org-num ob ol-irc org-persist org-plot org-version ox-latex ox-org ob-sqlite ol-rmail ob-maxima ob-python ob-dot ox-texinfo ob-tangle ol-info ox-publish oc-natbib org-habit ox-odt oc-biblatex ob-makefile ol ox-koma-letter org-list ob-scheme oc-basic org-src ob-clojure ob-lilypond org-capture ol-eshell org-ctags ob-shell ob-comint org-mouse org-duration ox-md org-datetree ob-forth ob-ocaml ol-mhe org-inlinetask ob-fortran ol-man ob-emacs-lisp org-archive org-protocol org-install)) "open-junk-file" ((open-junk-file open-junk-file-autoloads) (autoload 'find-file-hook--open-junk-file "open-junk-file" "Run `open-junk-file-hook' when the file is a JUNK file." nil nil) (add-hook 'find-file-hook 'find-file-hook--open-junk-file) (autoload 'open-junk-file "open-junk-file" "Open a new file whose filename is derived from current time.
You can write short program in it.  It helps to try-and-error programs.

For example, in Emacs Lisp programming, use M-x `open-junk-file'
instead of *scratch* buffer.  The junk code is SEARCHABLE.

FORMAT and FIND-FILE-FN are optional.
Default value of them are `open-junk-file-format' and
`open-junk-file-find-file-function'.

(fn &optional FORMAT FIND-FILE-FN)" t nil) (register-definition-prefixes "open-junk-file" '("open-junk-file-")) (provide 'open-junk-file-autoloads)) "lispxmp" ((lispxmp lispxmp-autoloads) (autoload 'lispxmp "lispxmp" "Annotate value of lines containing `; =>' ." t nil) (register-definition-prefixes "lispxmp" '("%lispxmp-" "lispxmp-")) (provide 'lispxmp-autoloads)) "paredit" ((paredit paredit-autoloads) (autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code.
With a prefix argument, enable Paredit Mode even if there are
  unbalanced parentheses in the buffer.
Paredit behaves badly if parentheses are unbalanced, so exercise
  caution when forcing Paredit Mode to be enabled, and consider
  fixing unbalanced parentheses instead.
\\<paredit-mode-map>

If called interactively, enable Paredit mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t nil) (register-definition-prefixes "paredit" '("?\\" "disable-paredit-mode" "paredit-")) (provide 'paredit-autoloads)) "auto-async-byte-compile" ((auto-async-byte-compile-autoloads auto-async-byte-compile) (register-definition-prefixes "auto-async-byte-compile" '("aabc/" "auto-async-byte-compile" "enable-auto-async-byte-compile-mode")) (provide 'auto-async-byte-compile-autoloads)) "powershell" ((powershell powershell-autoloads) (add-to-list 'auto-mode-alist '("\\.ps[dm]?1\\'" . powershell-mode)) (autoload 'powershell-mode "powershell" "Major mode for editing PowerShell scripts.

\\{powershell-mode-map}
Entry to this mode calls the value of `powershell-mode-hook' if
that value is non-nil.

(fn)" t nil) (autoload 'powershell "powershell" "Run an inferior PowerShell.
If BUFFER is non-nil, use it to hold the powershell
process.  Defaults to *PowerShell*.

Interactively, a prefix arg means to prompt for BUFFER.

If BUFFER exists but the shell process is not running, it makes a
new shell.

If BUFFER exists and the shell process is running, just switch to
BUFFER.

If PROMPT-STRING is non-nil, sets the prompt to the given value.

See the help for `shell' for more details.  (Type
\\[describe-mode] in the shell buffer for a list of commands.)

(fn &optional BUFFER PROMPT-STRING)" t nil) (register-definition-prefixes "powershell" '("explicit-powershell.exe-args" "powershell-")) (provide 'powershell-autoloads)) "dockerfile-mode" ((dockerfile-mode dockerfile-mode-autoloads) (autoload 'dockerfile-build-buffer "dockerfile-mode" "Build an image called IMAGE-NAME based upon the buffer.

If prefix arg NO-CACHE is set, don't cache the image.
The build string will be of the format:
`sudo docker build --no-cache --tag IMAGE-NAME --build-args arg1.. -f filename directory`

(fn IMAGE-NAME &optional NO-CACHE)" t nil) (autoload 'dockerfile-build-no-cache-buffer "dockerfile-mode" "Build an image called IMAGE-NAME based upon the buffer without cache.

(fn IMAGE-NAME)" t nil) (autoload 'dockerfile-mode "dockerfile-mode" "A major mode to edit Dockerfiles.
\\{dockerfile-mode-map}

(fn)" t nil) (add-to-list 'auto-mode-alist '("/Dockerfile\\(?:\\.[^/\\]*\\)?\\'" . dockerfile-mode)) (add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-mode)) (register-definition-prefixes "dockerfile-mode" '("dockerfile-")) (provide 'dockerfile-mode-autoloads)) "yaml-mode" ((yaml-mode-autoloads yaml-mode) (let ((loads (get 'yaml 'custom-loads))) (if (member '"yaml-mode" loads) nil (put 'yaml 'custom-loads (cons '"yaml-mode" loads)))) (autoload 'yaml-mode "yaml-mode" "Simple mode to edit YAML.

\\{yaml-mode-map}

(fn)" t nil) (add-to-list 'auto-mode-alist '("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode)) (add-to-list 'magic-mode-alist '("^%YAML\\s-+[0-9]+\\.[0-9]+\\(\\s-+#\\|\\s-*$\\)" . yaml-mode)) (register-definition-prefixes "yaml-mode" '("yaml-")) (provide 'yaml-mode-autoloads)) "docker-compose-mode" ((docker-compose-mode docker-compose-mode-autoloads) (autoload 'docker-compose-mode "docker-compose-mode" "Major mode to edit docker-compose files.

(fn)" t nil) (add-to-list 'auto-mode-alist '("docker-compose[^/]*\\.ya?ml\\'" . docker-compose-mode)) (register-definition-prefixes "docker-compose-mode" '("docker-compose-")) (provide 'docker-compose-mode-autoloads)) "cmake-mode" ((cmake-mode-autoloads cmake-mode) (autoload 'cmake-mode "cmake-mode" "Major mode for editing CMake source files.

(fn)" t nil) (autoload 'cmake-command-run "cmake-mode" "Runs the command cmake with the arguments specified.  The
optional argument topic will be appended to the argument list.

(fn TYPE &optional TOPIC BUFFER)" t nil) (autoload 'cmake-command-run-help "cmake-mode" "`cmake-command-run' but rendered in `rst-mode'.

(fn TYPE &optional TOPIC BUFFER)" t nil) (autoload 'cmake-help-list-commands "cmake-mode" "Prints out a list of the cmake commands." t nil) (autoload 'cmake-help-command "cmake-mode" "Prints out the help message for the command the cursor is on." t nil) (autoload 'cmake-help-module "cmake-mode" "Prints out the help message for the module the cursor is on." t nil) (autoload 'cmake-help-variable "cmake-mode" "Prints out the help message for the variable the cursor is on." t nil) (autoload 'cmake-help-property "cmake-mode" "Prints out the help message for the property the cursor is on." t nil) (autoload 'cmake-help "cmake-mode" "Queries for any of the four available help topics and prints out the appropriate page." t nil) (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode)) (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode)) (register-definition-prefixes "cmake-mode" '("cmake-")) (provide 'cmake-mode-autoloads)) "protobuf-mode" ((protobuf-mode protobuf-mode-autoloads) (add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode)) (autoload 'protobuf-mode "protobuf-mode" "Major mode for editing Protocol Buffers description language.

The hook `c-mode-common-hook' is run with no argument at mode
initialization, then `protobuf-mode-hook'.

Key bindings:
\\{protobuf-mode-map}

(fn)" t nil) (register-definition-prefixes "protobuf-mode" '("protobuf-")) (provide 'protobuf-mode-autoloads)) "csv-mode" ((csv-mode-autoloads csv-mode-tests csv-mode) (autoload 'csv-mode "csv-mode" "Major mode for editing files of comma-separated value type.

CSV mode is derived from `text-mode', and runs `text-mode-hook' before
running `csv-mode-hook'.  It turns `auto-fill-mode' off by default.
CSV mode can be customized by user options in the CSV customization
group.  The separators are specified by the value of `csv-separators'.

CSV mode commands ignore blank lines and comment lines beginning with
the value of `csv-comment-start', which delimit \"paragraphs\".
\"Sexp\" is re-interpreted to mean \"field\", so that `forward-sexp'
(\\[forward-sexp]), `kill-sexp' (\\[kill-sexp]), etc. all apply to fields.
Standard comment commands apply, such as `comment-dwim' (\\[comment-dwim]).

If `font-lock-mode' is enabled then separators, quoted values and
comment lines are highlighted using respectively `csv-separator-face',
`font-lock-string-face' and `font-lock-comment-face'.

The user interface (UI) for CSV mode commands is similar to that of
the standard commands `sort-fields' and `sort-numeric-fields', except
that if there is no prefix argument then the UI prompts for the field
index or indices.  In `transient-mark-mode' only: if the region is not
set then the UI attempts to set it to include all consecutive CSV
records around point, and prompts for confirmation; if there is no
prefix argument then the UI prompts for it, offering as a default the
index of the field containing point if the region was not set
explicitly.  The region set automatically is delimited by blank lines
and comment lines, and the number of header lines at the beginning of
the region given by the value of `csv-header-lines' are skipped.

Sort order is controlled by `csv-descending'.

CSV mode provides the following specific keyboard key bindings:

\\{csv-mode-map}

(fn)" t nil) (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode)) (add-to-list 'auto-mode-alist '("\\.tsv\\'" . tsv-mode)) (autoload 'tsv-mode "csv-mode" "Major mode for editing files of tab-separated value type.

(fn)" t nil) (register-definition-prefixes "csv-mode" '("csv-" "tsv-")) (register-definition-prefixes "csv-mode-tests" '("csv-mode-tests--align-fields")) (provide 'csv-mode-autoloads)) "go-mode" ((go-mode go-mode-autoloads) (autoload 'go-mode "go-mode" "Major mode for editing Go source text.

This mode provides (not just) basic editing capabilities for
working with Go code. It offers almost complete syntax
highlighting, indentation that is almost identical to gofmt and
proper parsing of the buffer content to allow features such as
navigation by function, manipulation of comments or detection of
strings.

In addition to these core features, it offers various features to
help with writing Go code. You can directly run buffer content
through gofmt, read godoc documentation from within Emacs, modify
and clean up the list of package imports or interact with the
Playground (uploading and downloading pastes).

The following extra functions are defined:

- `gofmt'
- `godoc' and `godoc-at-point'
- `go-import-add'
- `go-remove-unused-imports'
- `go-goto-arguments'
- `go-goto-docstring'
- `go-goto-function'
- `go-goto-function-name'
- `go-goto-imports'
- `go-goto-return-values'
- `go-goto-method-receiver'
- `go-play-buffer' and `go-play-region'
- `go-download-play'
- `godef-describe' and `godef-jump'
- `go-coverage'
- `go-set-project'
- `go-reset-gopath'

If you want to automatically run `gofmt' before saving a file,
add the following hook to your emacs configuration:

(add-hook 'before-save-hook #'gofmt-before-save)

If you want to use `godef-jump' instead of etags (or similar),
consider binding godef-jump to `M-.', which is the default key
for `find-tag':

(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd \"M-.\") #'godef-jump)))

Please note that godef is an external dependency. You can install
it with

go get github.com/rogpeppe/godef


If you're looking for even more integration with Go, namely
on-the-fly syntax checking, auto-completion and snippets, it is
recommended that you look at flycheck
(see URL `https://github.com/flycheck/flycheck') or flymake in combination
with goflymake (see URL `https://github.com/dougm/goflymake'), gocode
(see URL `https://github.com/nsf/gocode'), go-eldoc
(see URL `github.com/syohex/emacs-go-eldoc') and yasnippet-go
(see URL `https://github.com/dominikh/yasnippet-go')

(fn)" t nil) (add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-mode)) (autoload 'gofmt-before-save "go-mode" "Add this to .emacs to run gofmt on the current buffer when saving:
(add-hook 'before-save-hook 'gofmt-before-save).

Note that this will cause \342\200\230go-mode\342\200\231 to get loaded the first time
you save any file, kind of defeating the point of autoloading." t nil) (autoload 'godoc "go-mode" "Show Go documentation for QUERY, much like \\<go-mode-map>\\[man].

(fn QUERY)" t nil) (autoload 'go-download-play "go-mode" "Download a paste from the playground and insert it in a Go buffer.
Tries to look for a URL at point.

(fn URL)" t nil) (autoload 'go-dot-mod-mode "go-mode" "A major mode for editing go.mod files.

(fn)" t nil) (add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-dot-mod-mode)) (register-definition-prefixes "go-mode" '("go-" "god" "gofmt")) (provide 'go-mode-autoloads)) "typescript-mode" ((typescript-mode-test-utilities typescript-mode typescript-mode-autoloads) (put 'typescript-indent-level 'safe-local-variable #'integerp) (autoload 'typescript-mode "typescript-mode" "Major mode for editing typescript.

Key bindings:

\\{typescript-mode-map}

(fn)" t nil) (eval-after-load 'folding '(when (fboundp 'folding-add-to-marks-list) (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}"))) (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)) (register-definition-prefixes "typescript-mode" '("typescript-")) (register-definition-prefixes "typescript-mode-test-utilities" '("font-lock-test" "get-face-at" "test-with-")) (provide 'typescript-mode-autoloads)) "cuda-mode" ((cuda-mode cuda-mode-autoloads) (add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode)) (add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-mode)) (autoload 'cuda-mode "cuda-mode" "Major mode for editing CUDA.
Cuda is a C like language extension for mixed native/GPU coding
created by NVIDIA

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `cuda-mode-hook'.

Key bindings:
\\{cuda-mode-map}" t nil) (register-definition-prefixes "cuda-mode" '("cuda-")) (provide 'cuda-mode-autoloads)) "rust-mode" ((rust-playpen rust-mode rust-utils rust-compile rust-cargo rust-mode-autoloads rust-rustfmt) (register-definition-prefixes "rust-cargo" '("rust-")) (register-definition-prefixes "rust-compile" '("cargo-compilation-regexps" "rustc-")) (autoload 'rust-mode "rust-mode" "Major mode for Rust code.

\\{rust-mode-map}

(fn)" t nil) (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)) (register-definition-prefixes "rust-mode" '("rust-")) (register-definition-prefixes "rust-playpen" '("rust-")) (register-definition-prefixes "rust-rustfmt" '("rust-")) (autoload 'rust-dbg-wrap-or-unwrap "rust-utils" "Either remove or add the dbg! macro." t nil) (register-definition-prefixes "rust-utils" '("rust-")) (provide 'rust-mode-autoloads)) "markdown-mode" ((markdown-mode markdown-mode-autoloads) (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files.

(fn)" t nil) (add-to-list 'auto-mode-alist '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)) (autoload 'gfm-mode "markdown-mode" "Major mode for editing GitHub Flavored Markdown files.

(fn)" t nil) (autoload 'markdown-view-mode "markdown-mode" "Major mode for viewing Markdown content.

(fn)" t nil) (autoload 'gfm-view-mode "markdown-mode" "Major mode for viewing GitHub Flavored Markdown content.

(fn)" t nil) (autoload 'markdown-live-preview-mode "markdown-mode" "Toggle native previewing on save for a specific markdown file.

If called interactively, enable Markdown-Live-Preview mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "markdown-mode" '("defun-markdown-" "gfm-" "markdown")) (provide 'markdown-mode-autoloads)) "cargo" ((cargo-autoloads cargo cargo-process) (autoload 'cargo-minor-mode "cargo" "Cargo minor mode. Used to hold keybindings for cargo-mode.

If called interactively, enable Cargo minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{cargo-minor-mode-command-map}

(fn &optional ARG)" t nil) (register-definition-prefixes "cargo" '("cargo-m")) (autoload 'cargo-process-bench "cargo-process" "Run the Cargo bench command.
With the prefix argument, modify the command's invocation.
Cargo: Run the benchmarks." t nil) (autoload 'cargo-process-build "cargo-process" "Run the Cargo build command.
With the prefix argument, modify the command's invocation.
Cargo: Compile the current project." t nil) (autoload 'cargo-process-clean "cargo-process" "Run the Cargo clean command.
With the prefix argument, modify the command's invocation.
Cargo: Remove the target directory." t nil) (autoload 'cargo-process-doc "cargo-process" "Run the Cargo doc command.
With the prefix argument, modify the command's invocation.
Cargo: Build this project's and its dependencies' documentation." t nil) (autoload 'cargo-process-doc-open "cargo-process" "Run the Cargo doc command with the --open switch.
With the prefix argument, modify the command's invocation.
Cargo: Open this project's documentation." t nil) (autoload 'cargo-process-new "cargo-process" "Run the Cargo new command.
With the prefix argument, modify the command's invocation.
NAME is the name of your application.
If BIN is t then create a binary application, otherwise a library.
Cargo: Create a new cargo project.

(fn NAME &optional BIN)" t nil) (autoload 'cargo-process-init "cargo-process" "Run the Cargo init command.
With the prefix argument, modify the command's invocation.
DIRECTORY is the directory you want to create a cargo project in.
If BIN is t then create a binary application, otherwise a library.
Cargo: Create a new cargo project in current directory.

DIRECTORY is created if necessary.

(fn DIRECTORY &optional BIN)" t nil) (autoload 'cargo-process-run "cargo-process" "Run the Cargo run command.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute src/main.rs." t nil) (autoload 'cargo-process-run-bin "cargo-process" "Run the Cargo run command --bin <name>.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute a specific binary

(fn COMMAND)" t nil) (autoload 'cargo-process-run-example "cargo-process" "Run the Cargo run command --example <name>.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute with --example <name>.

(fn COMMAND)" t nil) (autoload 'cargo-process-search "cargo-process" "Run the Cargo search command.
With the prefix argument, modify the command's invocation.
SEARCH-TERM is used as the search term for the Cargo registry.
Cargo: Search registry for crates.

(fn SEARCH-TERM)" t nil) (autoload 'cargo-process-test "cargo-process" "Run the Cargo test command.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests." t nil) (autoload 'cargo-process-current-test "cargo-process" "Run the Cargo test command for the current test.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests." t nil) (autoload 'cargo-process-current-file-tests "cargo-process" "Run the Cargo test command for the current file.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests." t nil) (autoload 'cargo-process-update "cargo-process" "Run the Cargo update command.
With the prefix argument, modify the command's invocation.
Cargo: Update dependencies listed in Cargo.lock." t nil) (autoload 'cargo-process-fmt "cargo-process" "Run the Cargo fmt command.
With the prefix argument, modify the command's invocation.
Requires Cargo Fmt to be installed." t nil) (autoload 'cargo-process-outdated "cargo-process" "Run the Cargo outdated command.
With the prefix argument, modify the command's invocation.
Requires Cargo Outdated to be installed." t nil) (autoload 'cargo-process-check "cargo-process" "Run the Cargo check command.
With the prefix argument, modify the command's invocation.
Cargo: Check compile the current project.
Requires cargo-check to be installed." t nil) (autoload 'cargo-process-clippy "cargo-process" "Run the Cargo clippy command.
With the prefix argument, modify the command's invocation.
Cargo: Clippy compile the current project.
Requires Cargo clippy to be installed." t nil) (autoload 'cargo-process-add "cargo-process" "Run the Cargo add command.
With the prefix argument, modify the command's invocation.
CRATES is the name of the crate to add.
Cargo: This command allows you to add a dependency to a Cargo.toml manifest file.

(fn CRATE)" t nil) (autoload 'cargo-process-audit "cargo-process" "Run the Cargo audit command.
With the prefix argument, modify the command's invocation.
Cargo: Audit checks the current project's Cargo.lock for security vulnerabilities.
Requires Cargo Audit to be installed." t nil) (autoload 'cargo-process-script "cargo-process" "Run the Cargo script command.
With the prefix argument, modify the command's invocation.
Cargo: Script compiles and runs 'Cargoified Rust scripts'.
Requires Cargo Script to be installed." t nil) (autoload 'cargo-process-rm "cargo-process" "Run the Cargo rm command.
With the prefix argument, modify the command's invocation.
CRATE is the name of the crate to remove.
Cargo: Remove a dependency from a Cargo.toml manifest file.

(fn CRATE)" t nil) (autoload 'cargo-process-upgrade "cargo-process" "Run the Cargo update command.
With the prefix argument, modify the command's invocation.
If ALL is t then update all crates, otherwise specify CRATES.
Cargo: Upgrade dependencies as specified in the local manifest file

(fn &optional ALL CRATES)" t nil) (autoload 'cargo-process-repeat "cargo-process" "Run the last cargo-process command." t nil) (register-definition-prefixes "cargo-process" '("cargo-" "manifest-path-argument" "rustc-errno" "set-rust-backtrace")) (provide 'cargo-autoloads)) "epl" ((epl-autoloads epl) (register-definition-prefixes "epl" '("epl-")) (provide 'epl-autoloads)) "pkg-info" ((pkg-info-autoloads pkg-info) (autoload 'pkg-info-library-original-version "pkg-info" "Get the original version in the header of LIBRARY.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no X-Original-Version
header.

See Info node `(elisp)Library Headers' for more information
about library headers.

(fn LIBRARY &optional SHOW)" t nil) (autoload 'pkg-info-library-version "pkg-info" "Get the version in the header of LIBRARY.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no proper header.

See Info node `(elisp)Library Headers' for more information
about library headers.

(fn LIBRARY &optional SHOW)" t nil) (autoload 'pkg-info-defining-library-original-version "pkg-info" "Get the original version of the library defining FUNCTION.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

(fn FUNCTION &optional SHOW)" t nil) (autoload 'pkg-info-defining-library-version "pkg-info" "Get the version of the library defining FUNCTION.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

(fn FUNCTION &optional SHOW)" t nil) (autoload 'pkg-info-package-version "pkg-info" "Get the version of an installed PACKAGE.

If SHOW is non-nil, show the version in the minibuffer.

Return the version as list, or nil if PACKAGE is not installed.

(fn PACKAGE &optional SHOW)" t nil) (autoload 'pkg-info-version-info "pkg-info" "Obtain complete version info for LIBRARY and PACKAGE.

LIBRARY is a symbol denoting a named feature, or a library name
as string.  PACKAGE is a symbol denoting an ELPA package.  If
omitted or nil, default to LIBRARY.

If SHOW is non-nil, show the version in the minibuffer.

When called interactively, prompt for LIBRARY.  When called
interactively with prefix argument, prompt for PACKAGE as well.

Return a string with complete version information for LIBRARY.
This version information contains the version from the headers of
LIBRARY, and the version of the installed PACKAGE, the LIBRARY is
part of.  If PACKAGE is not installed, or if the PACKAGE version
is the same as the LIBRARY version, do not include a package
version.

(fn LIBRARY &optional PACKAGE SHOW)" t nil) (register-definition-prefixes "pkg-info" '("pkg-info-")) (provide 'pkg-info-autoloads)) "let-alist" ((let-alist let-alist-autoloads) (autoload 'let-alist "let-alist" "Let-bind dotted symbols to their cdrs in ALIST and execute BODY.
Dotted symbol is any symbol starting with a `.'.  Only those present
in BODY are let-bound and this search is done at compile time.

For instance, the following code

  (let-alist alist
    (if (and .title .body)
        .body
      .site
      .site.contents))

essentially expands to

  (let ((.title (cdr (assq \\='title alist)))
        (.body  (cdr (assq \\='body alist)))
        (.site  (cdr (assq \\='site alist)))
        (.site.contents (cdr (assq \\='contents (cdr (assq \\='site alist))))))
    (if (and .title .body)
        .body
      .site
      .site.contents))

If you nest `let-alist' invocations, the inner one can't access
the variables of the outer one.  You can, however, access alists
inside the original alist by using dots inside the symbol, as
displayed in the example above.

(fn ALIST &rest BODY)" nil t) (function-put 'let-alist 'lisp-indent-function '1) (register-definition-prefixes "let-alist" '("let-alist--")) (provide 'let-alist-autoloads)) "flycheck" ((flycheck-ert flycheck-autoloads flycheck-buttercup flycheck) (autoload 'flycheck-manual "flycheck" "Open the Flycheck manual." t nil) (autoload 'flycheck-mode "flycheck" "Flycheck is a minor mode for on-the-fly syntax checking.

In `flycheck-mode' the buffer is automatically syntax-checked
using the first suitable syntax checker from `flycheck-checkers'.
Use `flycheck-select-checker' to select a checker for the current
buffer manually.

If you run into issues, use `\\[flycheck-verify-setup]' to get help.

Flycheck supports many languages out of the box, and many
additional ones are available on MELPA.  Adding new ones is very
easy.  Complete documentation is available online at URL
`https://www.flycheck.org/en/latest/'.  Please report issues and
request features at URL `https://github.com/flycheck/flycheck'.

Flycheck displays its status in the mode line.  In the default
configuration, it looks like this:

`FlyC'     This buffer has not been checked yet.
`FlyC-'    Flycheck doesn't have a checker for this buffer.
`FlyC*'    Flycheck is running.  Expect results soon!
`FlyC:3|2' This buffer contains three warnings and two errors.
           Use `\\[flycheck-list-errors]' to see the list.

You may also see the following icons:
`FlyC!'    The checker crashed.
`FlyC.'    The last syntax check was manually interrupted.
`FlyC?'    The checker did something unexpected, like exiting with 1
           but returning no errors.

The following keybindings are available in `flycheck-mode':

\\{flycheck-mode-map}
(you can change the prefix by customizing
`flycheck-keymap-prefix')

If called interactively, enable Flycheck mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is \342\200\230toggle\342\200\231; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'global-flycheck-mode 'globalized-minor-mode t) (defvar global-flycheck-mode nil "Non-nil if Global Flycheck mode is enabled.
See the `global-flycheck-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-flycheck-mode'.") (custom-autoload 'global-flycheck-mode "flycheck" nil) (autoload 'global-flycheck-mode "flycheck" "Toggle Flycheck mode in all buffers.
With prefix ARG, enable Global Flycheck mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Flycheck mode is enabled in all buffers where
`flycheck-mode-on-safe' would do it.
See `flycheck-mode' for more information on Flycheck mode.

(fn &optional ARG)" t nil) (autoload 'flycheck-define-error-level "flycheck" "Define a new error LEVEL with PROPERTIES.

The following PROPERTIES constitute an error level:

`:severity SEVERITY'
     A number denoting the severity of this level.  The higher
     the number, the more severe is this level compared to other
     levels.  Defaults to 0; info is -10, warning is 10, and
     error is 100.

     The severity is used by `flycheck-error-level-<' to
     determine the ordering of errors according to their levels.

`:compilation-level LEVEL'

     A number indicating the broad class of messages that errors
     at this level belong to: one of 0 (info), 1 (warning), or
     2 or nil (error).  Defaults to nil.

     This is used by `flycheck-checker-pattern-to-error-regexp'
     to map error levels into `compilation-mode''s hierarchy and
     to get proper highlighting of errors in `compilation-mode'.

`:overlay-category CATEGORY'
     A symbol denoting the overlay category to use for error
     highlight overlays for this level.  See Info
     node `(elisp)Overlay Properties' for more information about
     overlay categories.

     A category for an error level overlay should at least define
     the `face' property, for error highlighting.  Another useful
     property for error level categories is `priority', to
     influence the stacking of multiple error level overlays.

`:fringe-bitmap BITMAPS'
     A fringe bitmap symbol denoting the bitmap to use for fringe
     indicators for this level, or a cons of two bitmaps (one for
     narrow fringes and one for wide fringes).  See Info node
     `(elisp)Fringe Bitmaps' for more information about fringe
     bitmaps, including a list of built-in fringe bitmaps.

`:fringe-face FACE'
     A face symbol denoting the face to use for fringe indicators
     for this level.

`:margin-spec SPEC'
     A display specification indicating what to display in the
     margin when `flycheck-indication-mode' is `left-margin' or
     `right-margin'.  See Info node `(elisp)Displaying in the
     Margins'.  If omitted, Flycheck generates an image spec from
     the fringe bitmap.

`:error-list-face FACE'
     A face symbol denoting the face to use for messages of this
     level in the error list.  See `flycheck-list-errors'.

(fn LEVEL &rest PROPERTIES)" nil nil) (function-put 'flycheck-define-error-level 'lisp-indent-function '1) (autoload 'flycheck-define-command-checker "flycheck" "Define SYMBOL as syntax checker to run a command.

Define SYMBOL as generic syntax checker via
`flycheck-define-generic-checker', which uses an external command
to check the buffer.  SYMBOL and DOCSTRING are the same as for
`flycheck-define-generic-checker'.

In addition to the properties understood by
`flycheck-define-generic-checker', the following PROPERTIES
constitute a command syntax checker.  Unless otherwise noted, all
properties are mandatory.  Note that the default `:error-filter'
of command checkers is `flycheck-sanitize-errors'.

`:command COMMAND'
     The command to run for syntax checking.

     COMMAND is a list of the form `(EXECUTABLE [ARG ...])'.

     EXECUTABLE is a string with the executable of this syntax
     checker.  It can be overridden with the variable
     `flycheck-SYMBOL-executable'.  Note that this variable is
     NOT implicitly defined by this function.  Use
     `flycheck-def-executable-var' to define this variable.

     Each ARG is an argument to the executable, either as string,
     or as special symbol or form for
     `flycheck-substitute-argument', which see.

`:error-patterns PATTERNS'
     A list of patterns to parse the output of the `:command'.

     Each ITEM in PATTERNS is a list `(LEVEL SEXP ...)', where
     LEVEL is a Flycheck error level (see
     `flycheck-define-error-level'), followed by one or more RX
     `SEXP's which parse an error of that level and extract line,
     column, file name and the message.

     See `rx' for general information about RX, and
     `flycheck-rx-to-string' for some special RX forms provided
     by Flycheck.

     All patterns are applied in the order of declaration to the
     whole output of the syntax checker.  Output already matched
     by a pattern will not be matched by subsequent patterns.  In
     other words, the first pattern wins.

     This property is optional.  If omitted, however, an
     `:error-parser' is mandatory.

`:error-parser FUNCTION'
     A function to parse errors with.

     The function shall accept three arguments OUTPUT CHECKER
     BUFFER.  OUTPUT is the syntax checker output as string,
     CHECKER the syntax checker that was used, and BUFFER a
     buffer object representing the checked buffer.  The function
     must return a list of `flycheck-error' objects parsed from
     OUTPUT.

     This property is optional.  If omitted, it defaults to
     `flycheck-parse-with-patterns'.  In this case,
     `:error-patterns' is mandatory.

`:standard-input t'
     Whether to send the buffer contents on standard input.

     If this property is given and has a non-nil value, send the
     contents of the buffer on standard input.

     Defaults to nil.

Note that you may not give `:start', `:interrupt', and
`:print-doc' for a command checker.  You can give a custom
`:verify' function, though, whose results will be appended to the
default `:verify' function of command checkers.

(fn SYMBOL DOCSTRING &rest PROPERTIES)" nil nil) (function-put 'flycheck-define-command-checker 'lisp-indent-function '1) (function-put 'flycheck-define-command-checker 'doc-string-elt '2) (autoload 'flycheck-def-config-file-var "flycheck" "Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable variable using `defcustom', to
provide configuration files for the given syntax CHECKER.
CUSTOM-ARGS are forwarded to `defcustom'.

FILE-NAME is the initial value of the new variable.  If omitted,
the default value is nil.  It can be either a string or a list of
strings.

Use this together with the `config-file' form in the `:command'
argument to `flycheck-define-checker'.

(fn SYMBOL CHECKER &optional FILE-NAME &rest CUSTOM-ARGS)" nil t) (function-put 'flycheck-def-config-file-var 'lisp-indent-function '3) (autoload 'flycheck-def-option-var "flycheck" "Define SYMBOL as option variable with INIT-VALUE for CHECKER.

SYMBOL is declared as customizable variable using `defcustom', to
provide an option for the given syntax CHECKERS (a checker or a
list of checkers).  INIT-VALUE is the initial value of the
variable, and DOCSTRING is its docstring.  CUSTOM-ARGS are
forwarded to `defcustom'.

Use this together with the `option', `option-list' and
`option-flag' forms in the `:command' argument to
`flycheck-define-checker'.

(fn SYMBOL INIT-VALUE CHECKERS DOCSTRING &rest CUSTOM-ARGS)" nil t) (function-put 'flycheck-def-option-var 'lisp-indent-function '3) (function-put 'flycheck-def-option-var 'doc-string-elt '4) (autoload 'flycheck-define-checker "flycheck" "Define SYMBOL as command syntax checker with DOCSTRING and PROPERTIES.

Like `flycheck-define-command-checker', but PROPERTIES must not
be quoted.  Also, implicitly define the executable variable for
SYMBOL with `flycheck-def-executable-var'.

(fn SYMBOL DOCSTRING &rest PROPERTIES)" nil t) (function-put 'flycheck-define-checker 'lisp-indent-function '1) (function-put 'flycheck-define-checker 'doc-string-elt '2) (register-definition-prefixes "flycheck" '("flycheck-" "help-flycheck-checker-d" "list-flycheck-errors")) (register-definition-prefixes "flycheck-buttercup" '("flycheck-buttercup-format-error-list")) (register-definition-prefixes "flycheck-ert" '("flycheck-er")) (provide 'flycheck-autoloads)) "flycheck-rust" ((flycheck-rust flycheck-rust-autoloads) (autoload 'flycheck-rust-setup "flycheck-rust" "Setup Rust in Flycheck.

If the current file is part of a Cargo project, configure
Flycheck according to the Cargo project layout." t nil) (register-definition-prefixes "flycheck-rust" '("flycheck-rust-")) (provide 'flycheck-rust-autoloads)) "web-mode" ((web-mode-autoloads web-mode) (autoload 'web-mode "web-mode" "Major mode for editing web templates.

(fn)" t nil) (register-definition-prefixes "web-mode" '("web-mode-")) (provide 'web-mode-autoloads)) "prettier-js" ((prettier-js-autoloads prettier-js) (autoload 'prettier-js-mode "prettier-js" "Runs prettier on file save when this mode is turned on

If called interactively, enable Prettier-Js mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "prettier-js" '("prettier-js")) (provide 'prettier-js-autoloads)) "glsl-mode" ((glsl-mode glsl-mode-autoloads) (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode)) (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode)) (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode)) (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode)) (autoload 'glsl-mode "glsl-mode" "Major mode for editing GLSL shader files.

(fn)" t nil) (register-definition-prefixes "glsl-mode" '("gl-version" "glsl-")) (provide 'glsl-mode-autoloads)) "clang-format" ((clang-format clang-format-autoloads) (autoload 'clang-format-region "clang-format" "Use clang-format to format the code between START and END according to STYLE.
If called interactively uses the region or the current statement if there is no
no active region. If no STYLE is given uses `clang-format-style'. Use
ASSUME-FILE-NAME to locate a style config file, if no ASSUME-FILE-NAME is given
uses the function `buffer-file-name'.

(fn START END &optional STYLE ASSUME-FILE-NAME)" t nil) (autoload 'clang-format-buffer "clang-format" "Use clang-format to format the current buffer according to STYLE.
If no STYLE is given uses `clang-format-style'. Use ASSUME-FILE-NAME
to locate a style config file. If no ASSUME-FILE-NAME is given uses
the function `buffer-file-name'.

(fn &optional STYLE ASSUME-FILE-NAME)" t nil) (defalias 'clang-format 'clang-format-region) (register-definition-prefixes "clang-format" '("clang-format-")) (provide 'clang-format-autoloads)) "clang-format+" ((clang-format+ clang-format+-autoloads) (autoload 'clang-format+-mode "clang-format+" "Run clang-format on save.

If called interactively, enable Clang-Format+ mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "clang-format+" '("clang-format+-")) (provide 'clang-format+-autoloads)) "rainbow-delimiters" ((rainbow-delimiters rainbow-delimiters-autoloads) (autoload 'rainbow-delimiters-mode "rainbow-delimiters" "Highlight nested parentheses, brackets, and braces according to their depth.

If called interactively, enable Rainbow-Delimiters mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'rainbow-delimiters-mode-enable "rainbow-delimiters" "Enable `rainbow-delimiters-mode'." nil nil) (autoload 'rainbow-delimiters-mode-disable "rainbow-delimiters" "Disable `rainbow-delimiters-mode'." nil nil) (register-definition-prefixes "rainbow-delimiters" '("rainbow-delimiters-")) (provide 'rainbow-delimiters-autoloads)) "zenburn-theme" ((zenburn-theme zenburn-theme-autoloads) (defvar zenburn-override-colors-alist 'nil "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist.") (custom-autoload 'zenburn-override-colors-alist "zenburn-theme" t) (and load-file-name (boundp 'custom-theme-load-path) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (register-definition-prefixes "zenburn-theme" '("zenburn")) (provide 'zenburn-theme-autoloads)) "vline" ((vline vline-autoloads) (autoload 'vline-mode "vline" "Display vertical line mode.

If called interactively, enable Vline mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (put 'vline-global-mode 'globalized-minor-mode t) (defvar vline-global-mode nil "Non-nil if Vline-Global mode is enabled.
See the `vline-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vline-global-mode'.") (custom-autoload 'vline-global-mode "vline" nil) (autoload 'vline-global-mode "vline" "Toggle Vline mode in all buffers.
With prefix ARG, enable Vline-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Vline mode is enabled in all buffers where
`(lambda nil (unless (minibufferp) (vline-mode 1)))' would do it.
See `vline-mode' for more information on Vline mode.

(fn &optional ARG)" t nil) (register-definition-prefixes "vline" '("vline-")) (provide 'vline-autoloads)) "col-highlight" ((col-highlight-autoloads col-highlight) (let ((loads (get 'column-highlight 'custom-loads))) (if (member '"col-highlight" loads) nil (put 'column-highlight 'custom-loads (cons '"col-highlight" loads)))) (defvar col-highlight-show-only nil "Non-nil means `column-highlight-mode' affects only a section of text.
This affects `vline-mode' also.

The non-nil value determines the type of text section: paragraph,
sentence, defun, page...

The actual non-nil value is a forward movement command for the given
section type, e.g., `forward-paragraph', `end-of-defun'.") (custom-autoload 'col-highlight-show-only "col-highlight" t) (defvar col-highlight-vline-face-flag t "*Non-nil means `column-highlight-mode' uses `col-highlight-face'.
nil means that it uses `vline-face'.") (custom-autoload 'col-highlight-vline-face-flag "col-highlight" t) (defvar col-highlight-period 1 "*Number of seconds to highlight the current column.") (custom-autoload 'col-highlight-period "col-highlight" t) (defvar col-highlight-overlay-priority 300 "*Priority to use for overlays in `vline-overlay-table'.
A higher priority can make the vline highlighting appear on top of
other overlays that might exist.") (custom-autoload 'col-highlight-overlay-priority "col-highlight" t) (defface col-highlight '((t (:background "SlateGray3"))) "*Face for current-column highlighting by `column-highlight-mode'.
Not used if `col-highlight-vline-face-flag' is nil." :group 'column-highlight :group 'faces) (defvar column-highlight-mode nil "Non-nil if Column-Highlight mode is enabled.
See the `column-highlight-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `column-highlight-mode'.") (custom-autoload 'column-highlight-mode "col-highlight" nil) (autoload 'column-highlight-mode "col-highlight" "Toggle highlighting the current column.
With ARG, turn column highlighting on if and only if ARG is positive.

Column-Highlight mode uses the functions
`col-highlight-unhighlight' and `col-highlight-highlight'
on `pre-command-hook' and `post-command-hook'.

(fn &optional ARG)" t nil) (defalias 'toggle-highlight-column-when-idle 'col-highlight-toggle-when-idle) (autoload 'col-highlight-toggle-when-idle "col-highlight" "Turn on or off highlighting the current column when Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off.

(fn &optional ARG)" t nil) (autoload 'col-highlight-set-interval "col-highlight" "Set the delay before highlighting current column when Emacs is idle.
Whenever Emacs has been idle for N seconds, the current column is
highlighted using the face that is the value of variable
`col-highlight-face'.

To turn on or off automatically highlighting the current column
when Emacs is idle, use `\\[toggle-highlight-column-when-idle].

(fn N)" t nil) (defalias 'flash-column-highlight 'col-highlight-flash) (autoload 'col-highlight-flash "col-highlight" "Highlight the current column for `col-highlight-period' seconds.
With a prefix ARG, highlight for that many seconds.

(fn &optional ARG)" t nil) (register-definition-prefixes "col-highlight" '("col-highlight-" "vline-show")) (provide 'col-highlight-autoloads)) "powerline" ((powerline powerline-autoloads powerline-themes powerline-separators) (autoload 'powerline-hud "powerline" "Return an XPM of relative buffer location using FACE1 and FACE2 of optional WIDTH.

(fn FACE1 FACE2 &optional WIDTH)" nil nil) (autoload 'powerline-mouse "powerline" "Return mouse handler for CLICK-GROUP given CLICK-TYPE and STRING.

(fn CLICK-GROUP CLICK-TYPE STRING)" nil nil) (autoload 'powerline-concat "powerline" "Concatonate STRINGS and pad sides by spaces.

(fn &rest STRINGS)" nil nil) (autoload 'defpowerline "powerline" "Create function NAME by wrapping BODY with powerline padding an propetization.

(fn NAME BODY)" nil t) (autoload 'powerline-raw "powerline" "Render STR as mode-line data using FACE and optionally PAD import on left (l) or right (r).

(fn STR &optional FACE PAD)" nil nil) (autoload 'powerline-fill "powerline" "Return empty space using FACE and leaving RESERVE space on the right.

(fn FACE RESERVE)" nil nil) (autoload 'powerline-major-mode "powerline") (autoload 'powerline-minor-modes "powerline") (autoload 'powerline-narrow "powerline") (autoload 'powerline-vc "powerline") (autoload 'powerline-encoding "powerline") (autoload 'powerline-buffer-size "powerline") (autoload 'powerline-buffer-id "powerline") (autoload 'powerline-process "powerline") (autoload 'powerline-selected-window-active "powerline") (register-definition-prefixes "powerline" '("pl/" "powerline-")) (register-definition-prefixes "powerline-separators" '("pl/" "powerline-image-apple-rgb")) (autoload 'powerline-default-theme "powerline-themes" "Setup the default mode-line." t nil) (autoload 'powerline-center-theme "powerline-themes" "Setup a mode-line with major and minor modes centered." t nil) (autoload 'powerline-vim-theme "powerline-themes" "Setup a Vim-like mode-line." t nil) (autoload 'powerline-nano-theme "powerline-themes" "Setup a nano-like mode-line." t nil) (register-definition-prefixes "powerline-themes" '("powerline-")) (provide 'powerline-autoloads)) "total-lines" ((total-lines-autoloads total-lines) (autoload 'total-lines-mode "total-lines" "A minor mode that keeps track of the total number of lines in a buffer.

If called interactively, enable Total-Lines mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (put 'global-total-lines-mode 'globalized-minor-mode t) (defvar global-total-lines-mode nil "Non-nil if Global Total-Lines mode is enabled.
See the `global-total-lines-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-total-lines-mode'.") (custom-autoload 'global-total-lines-mode "total-lines" nil) (autoload 'global-total-lines-mode "total-lines" "Toggle Total-Lines mode in all buffers.
With prefix ARG, enable Global Total-Lines mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Total-Lines mode is enabled in all buffers where
`total-lines-on' would do it.
See `total-lines-mode' for more information on Total-Lines mode.

(fn &optional ARG)" t nil) (register-definition-prefixes "total-lines" '("total-lines")) (provide 'total-lines-autoloads)) "cursor-chg" ((cursor-chg cursor-chg-autoloads) (defvar curchg-change-cursor-on-input-method-flag t "*Non-nil means to use a different cursor when using an input method.") (custom-autoload 'curchg-change-cursor-on-input-method-flag "cursor-chg" t) (defvar curchg-change-cursor-on-overwrite/read-only-flag t "*Non-nil means use a different cursor for overwrite mode or read-only.") (custom-autoload 'curchg-change-cursor-on-overwrite/read-only-flag "cursor-chg" t) (defvar curchg-default-cursor-color (or (cdr (assq 'cursor-color default-frame-alist)) "Red") "*Default text cursor color for non-special frames.") (custom-autoload 'curchg-default-cursor-color "cursor-chg" t) (defvar curchg-default-cursor-type 'bar "*Default text cursor type.") (custom-autoload 'curchg-default-cursor-type "cursor-chg" t) (defvar curchg-idle-cursor-type 'box "*Text cursor type when Emacs is idle.") (custom-autoload 'curchg-idle-cursor-type "cursor-chg" t) (defvar curchg-input-method-cursor-color "Orange" "*Default cursor color if using an input method.
This has no effect if `curchg-change-cursor-on-input-method-flag' is nil.") (custom-autoload 'curchg-input-method-cursor-color "cursor-chg" t) (defvar curchg-overwrite/read-only-cursor-type 'box "*Default text cursor type for overwrite mode or read-only buffer.
This applies only to non-special frames.  This has no effect if
`curchg-change-cursor-on-overwrite/read-only-flag' is nil.") (custom-autoload 'curchg-overwrite/read-only-cursor-type "cursor-chg" t) (autoload 'curchg-set-cursor-type "cursor-chg" "Set the cursor type of the selected frame to CURSOR-TYPE.
When called interactively, prompt for the type to use.
To get the frame's current cursor type, use `frame-parameters'.

(fn CURSOR-TYPE)" t nil) (defalias 'toggle-cursor-type-when-idle 'curchg-toggle-cursor-type-when-idle) (autoload 'curchg-toggle-cursor-type-when-idle "cursor-chg" "Turn on or off automatically changing cursor type when Emacs is idle.
When on, use `curchg-idle-cursor-type' whenever Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off.

(fn &optional ARG)" t nil) (autoload 'curchg-change-cursor-when-idle-interval "cursor-chg" "Set wait until automatically change cursor type when Emacs is idle.
Whenever Emacs is idle for this many seconds, the cursor type will
change to `curchg-idle-cursor-type'.

To turn on or off automatically changing the cursor type when idle,
use `\\[toggle-cursor-type-when-idle].

(fn SECS)" t nil) (register-definition-prefixes "cursor-chg" '("curchg-")) (provide 'cursor-chg-autoloads)) "s" ((s-autoloads s) (register-definition-prefixes "s" '("s-")) (provide 's-autoloads)) "ag" ((ag-autoloads ag) (autoload 'ag "ag" "Search using ag in a given DIRECTORY for a given literal search STRING,
with STRING defaulting to the symbol under point.

If called with a prefix, prompts for flags to pass to ag.

(fn STRING DIRECTORY)" t nil) (autoload 'ag-files "ag" "Search using ag in a given DIRECTORY for a given literal search STRING,
limited to files that match FILE-TYPE. STRING defaults to the
symbol under point.

If called with a prefix, prompts for flags to pass to ag.

(fn STRING FILE-TYPE DIRECTORY)" t nil) (autoload 'ag-regexp "ag" "Search using ag in a given directory for a given regexp.
The regexp should be in PCRE syntax, not Emacs regexp syntax.

If called with a prefix, prompts for flags to pass to ag.

(fn STRING DIRECTORY)" t nil) (autoload 'ag-project "ag" "Guess the root of the current project and search it with ag
for the given literal search STRING.

If called with a prefix, prompts for flags to pass to ag.

(fn STRING)" t nil) (autoload 'ag-project-files "ag" "Search using ag for a given literal search STRING,
limited to files that match FILE-TYPE. STRING defaults to the
symbol under point.

If called with a prefix, prompts for flags to pass to ag.

(fn STRING FILE-TYPE)" t nil) (autoload 'ag-project-regexp "ag" "Guess the root of the current project and search it with ag
for the given regexp. The regexp should be in PCRE syntax, not
Emacs regexp syntax.

If called with a prefix, prompts for flags to pass to ag.

(fn REGEXP)" t nil) (defalias 'ag-project-at-point 'ag-project) (defalias 'ag-regexp-project-at-point 'ag-project-regexp) (autoload 'ag-dired "ag" "Recursively find files in DIR matching literal search STRING.

The PATTERN is matched against the full path to the file, not
only against the file name.

The results are presented as a `dired-mode' buffer with
`default-directory' being DIR.

See also `ag-dired-regexp'.

(fn DIR STRING)" t nil) (autoload 'ag-dired-regexp "ag" "Recursively find files in DIR matching REGEXP.
REGEXP should be in PCRE syntax, not Emacs regexp syntax.

The REGEXP is matched against the full path to the file, not
only against the file name.

Results are presented as a `dired-mode' buffer with
`default-directory' being DIR.

See also `find-dired'.

(fn DIR REGEXP)" t nil) (autoload 'ag-project-dired "ag" "Recursively find files in current project matching PATTERN.

See also `ag-dired'.

(fn PATTERN)" t nil) (autoload 'ag-project-dired-regexp "ag" "Recursively find files in current project matching REGEXP.

See also `ag-dired-regexp'.

(fn REGEXP)" t nil) (autoload 'ag-kill-buffers "ag" "Kill all `ag-mode' buffers." t nil) (autoload 'ag-kill-other-buffers "ag" "Kill all `ag-mode' buffers other than the current buffer." t nil) (register-definition-prefixes "ag" '("ag-" "ag/")) (provide 'ag-autoloads)) "wgrep" ((wgrep wgrep-autoloads) (autoload 'wgrep-setup "wgrep" "Setup wgrep preparation." nil nil) (add-hook 'grep-setup-hook 'wgrep-setup) (register-definition-prefixes "wgrep" '("wgrep-")) (provide 'wgrep-autoloads)) "wgrep-ag" ((wgrep-ag-autoloads wgrep-ag) (autoload 'wgrep-ag-setup "wgrep-ag" nil nil nil) (add-hook 'ag-mode-hook 'wgrep-ag-setup) (register-definition-prefixes "wgrep-ag" '("wgrep-ag-")) (provide 'wgrep-ag-autoloads)) "undo-tree" ((undo-tree-autoloads undo-tree) (autoload 'undo-tree-mode "undo-tree" "Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

If called interactively, enable Undo-Tree mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-mode-map}

(fn &optional ARG)" t nil) (put 'global-undo-tree-mode 'globalized-minor-mode t) (defvar global-undo-tree-mode nil "Non-nil if Global Undo-Tree mode is enabled.
See the `global-undo-tree-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-tree-mode'.") (custom-autoload 'global-undo-tree-mode "undo-tree" nil) (autoload 'global-undo-tree-mode "undo-tree" "Toggle Undo-Tree mode in all buffers.
With prefix ARG, enable Global Undo-Tree mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Undo-Tree mode is enabled in all buffers where
`turn-on-undo-tree-mode' would do it.
See `undo-tree-mode' for more information on Undo-Tree mode.

(fn &optional ARG)" t nil) (register-definition-prefixes "undo-tree" '("*undo-tree-id-counter*" "buffer-undo-tree" "turn-on-undo-tree-mode" "undo-")) (provide 'undo-tree-autoloads)) "buffer-move" ((buffer-move buffer-move-autoloads) (autoload 'buf-move-up "buffer-move" "Swap the current buffer and the buffer above the split.
   If there is no split, ie now window above the current one, an
   error is signaled." t nil) (autoload 'buf-move-down "buffer-move" "Swap the current buffer and the buffer under the split.
   If there is no split, ie now window under the current one, an
   error is signaled." t nil) (autoload 'buf-move-left "buffer-move" "Swap the current buffer and the buffer on the left of the split.
   If there is no split, ie now window on the left of the current
   one, an error is signaled." t nil) (autoload 'buf-move-right "buffer-move" "Swap the current buffer and the buffer on the right of the split.
   If there is no split, ie now window on the right of the current
   one, an error is signaled." t nil) (autoload 'buf-move "buffer-move" "Begin moving the current buffer to different windows.

Use the arrow keys to move in the desired direction.  Pressing
any other key exits this function." t nil) (register-definition-prefixes "buffer-move" '("buf")) (provide 'buffer-move-autoloads)) "fast-scroll" ((fast-scroll fast-scroll-autoloads) (autoload 'fast-scroll-config "fast-scroll" "Load some config defaults / binds." t nil) (autoload 'fast-scroll-advice-scroll-functions "fast-scroll" "Wrap as many scrolling functions that we know of in this advice." t nil) (defvar fast-scroll-mode nil "Non-nil if Fast-Scroll mode is enabled.
See the `fast-scroll-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `fast-scroll-mode'.") (custom-autoload 'fast-scroll-mode "fast-scroll" nil) (autoload 'fast-scroll-mode "fast-scroll" "Minor mode to speed up scrolling.

If called interactively, enable Fast-Scroll mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When fast-scroll-mode is on, certain features/modes of Emacs will be
shut off or minimized during the scrolling activity, to ensure
the user experience the least amount of scroll-lag as possible.

By default, enabling this global minor mode will advice the following
scrolling built-ins (or commonly installed scroll functions): `scroll-up-command',
`scroll-down-command', `evil-scroll-up', `evil-scroll-down'.

Disabling this mode will unload the advice that was added when enabling.

The mode-line format will also be set to a minimal mode-line
during scrolling activity.

(fn &optional ARG)" t nil) (register-definition-prefixes "fast-scroll" '("fast-scroll-")) (provide 'fast-scroll-autoloads)) "edit-server" ((edit-server-autoloads edit-server) (autoload 'edit-server-start "edit-server" "Start the edit server.

If argument VERBOSE is non-nil, logs all server activity to buffer
`*edit-server-log*'.  When called interactivity, a prefix argument
will cause it to be verbose.

(fn &optional VERBOSE)" t nil) (register-definition-prefixes "edit-server" '("edit-server-" "global-edit-server-edit-mode" "turn-on-edit-server-edit-mode-if-server")) (provide 'edit-server-autoloads)) "multi-term" ((multi-term multi-term-autoloads) (autoload 'multi-term "multi-term" "Create new term buffer.
Will prompt you shell name when you type `C-u' before this command." t nil) (autoload 'multi-term-next "multi-term" "Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET.

(fn &optional OFFSET)" t nil) (autoload 'multi-term-prev "multi-term" "Go to the previous term buffer.
If OFFSET is `non-nil', will goto previous term buffer with OFFSET.

(fn &optional OFFSET)" t nil) (autoload 'multi-term-dedicated-open "multi-term" "Open dedicated `multi-term' window.
Will prompt you shell name when you type `C-u' before this command." t nil) (autoload 'multi-term-dedicated-toggle "multi-term" "Toggle dedicated `multi-term' window." t nil) (autoload 'multi-term-dedicated-select "multi-term" "Select the `multi-term' dedicated window." t nil) (register-definition-prefixes "multi-term" '("multi-term-" "term-")) (provide 'multi-term-autoloads)) "gxref" ((gxref-autoloads gxref) (let ((loads (get 'gxref 'custom-loads))) (if (member '"gxref" loads) nil (put 'gxref 'custom-loads (cons '"gxref" loads)))) (defvar gxref-global-exe "global" "Path to GNU Global executable.") (custom-autoload 'gxref-global-exe "gxref" t) (defvar gxref-create-db-cmd "gtags" "Command to create the gtags database.") (custom-autoload 'gxref-create-db-cmd "gxref" t) (defvar gxref-update-db-on-save t "A flag indicating whether to update the GTAGS database when a file is saved.") (custom-autoload 'gxref-update-db-on-save "gxref" t) (defvar gxref-project-root-dir nil "The root directory of the project.
If not defined, 'global -p' will be used to find it.") (custom-autoload 'gxref-project-root-dir "gxref" t) (defvar gxref-gtags-conf nil "Explicit GTAGS/GLOBAL configuration file.") (custom-autoload 'gxref-gtags-conf "gxref" t) (defvar gxref-gtags-label nil "Explicit GTAGS/GLOBAL label.") (custom-autoload 'gxref-gtags-label "gxref" t) (defvar gxref-gtags-lib-path nil "Explicit GLOBAL libpath.") (custom-autoload 'gxref-gtags-lib-path "gxref" t) (autoload 'gxref-update-db "gxref" "Update GTAGS project database for current project." t nil) (autoload 'gxref-single-update-db "gxref" "Update GTAGS project database for the current file." t nil) (autoload 'gxref-create-db "gxref" "Create a GTAGS database in the directory specified as PROJECT-ROOT-DIR.

(fn PROJECT-ROOT-DIR)" t nil) (autoload 'gxref-set-project-dir "gxref" "Explicitly Set the directory of the current project to PROJECT-DIR.
The given project dir will be used for locating the GTAGS file,
until a different project is selected, or `gxref-clear-project-dir'
is called to clear the project.

This function is provided as a convenience, but there are other
ways to determine the current project, which could sometimes be
more comfortable.  One option is to not set the project at all,
in which case a search is performed upwards from the current
directory, until a GTAGS file is found.  Alternatively, you could
explicitly set the variable `gxref-project-root-dir'.  This has
the same effect as using this function, but can be by setting a
file-local or dir-local variable.

(fn PROJECT-DIR)" t nil) (autoload 'gxref-clear-project-dir "gxref" "Explicitly clear the project directory.
When no project directory is set, a project directory is
determined by searching upwards from the current directory, until
a GTAGS file is found.  See `gxref-set-project-dir' for more details." t nil) (autoload 'gxref-xref-backend "gxref" "Gxref backend for Xref." nil nil) (register-definition-prefixes "gxref" '("gxref--")) (provide 'gxref-autoloads)) "eldoc" ((eldoc eldoc-autoloads) (defvar eldoc-minor-mode-string (purecopy " ElDoc") "String to display in mode line when ElDoc Mode is enabled; nil for none.") (custom-autoload 'eldoc-minor-mode-string "eldoc" t) (autoload 'eldoc-mode "eldoc" "Toggle echo area display of Lisp objects at point (ElDoc mode).

If called interactively, enable Eldoc mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

ElDoc mode is a buffer-local minor mode.  When enabled, the echo
area displays information about a function or variable in the
text where point is.  If point is on a documented variable, it
displays the first line of that variable's doc string.  Otherwise
it displays the argument list of the function called in the
expression point is on.

(fn &optional ARG)" t nil) (put 'global-eldoc-mode 'globalized-minor-mode t) (defvar global-eldoc-mode t "Non-nil if Global Eldoc mode is enabled.
See the `global-eldoc-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-eldoc-mode'.") (custom-autoload 'global-eldoc-mode "eldoc" nil) (autoload 'global-eldoc-mode "eldoc" "Toggle Eldoc mode in all buffers.
With prefix ARG, enable Global Eldoc mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Eldoc mode is enabled in all buffers where
`turn-on-eldoc-mode' would do it.
See `eldoc-mode' for more information on Eldoc mode.

(fn &optional ARG)" t nil) (autoload 'turn-on-eldoc-mode "eldoc" "Turn on `eldoc-mode' if the buffer has ElDoc support enabled.
See `eldoc-documentation-strategy' for more detail." nil nil) (register-definition-prefixes "eldoc" '("eldoc")) (provide 'eldoc-autoloads)) "websocket" ((websocket websocket-autoloads) (register-definition-prefixes "websocket" '("websocket-")) (provide 'websocket-autoloads)) "anaphora" ((anaphora-autoloads anaphora) (let ((loads (get 'anaphora 'custom-loads))) (if (member '"anaphora" loads) nil (put 'anaphora 'custom-loads (cons '"anaphora" loads)))) (defvar anaphora-use-long-names-only nil "Use only long names such as `anaphoric-if' instead of traditional `aif'.") (custom-autoload 'anaphora-use-long-names-only "anaphora" t) (defun anaphora--install-traditional-aliases (&optional arg) "Install traditional short aliases for anaphoric macros.

With negative numeric ARG, remove traditional aliases." (let ((syms '((if . t) (prog1 . t) (prog2 . t) (when . when) (while . t) (and . t) (cond . cond) (lambda . lambda) (block . block) (case . case) (ecase . ecase) (typecase . typecase) (etypecase . etypecase) (let . let) (+ . t) (- . t) (* . t) (/ . t)))) (cond ((and (numberp arg) (< arg 0)) (dolist (cell syms) (when (ignore-errors (eq (symbol-function (intern-soft (format "a%s" (car cell)))) (intern-soft (format "anaphoric-%s" (car cell))))) (fmakunbound (intern (format "a%s" (car cell))))))) (t (dolist (cell syms) (let* ((builtin (car cell)) (traditional (intern (format "a%s" builtin))) (long (intern (format "anaphoric-%s" builtin)))) (defalias traditional long) (put traditional 'lisp-indent-function (get builtin 'lisp-indent-function)) (put traditional 'edebug-form-spec (cdr cell)))))))) (unless anaphora-use-long-names-only (anaphora--install-traditional-aliases)) (autoload 'anaphoric-if "anaphora" "Like `if', but the result of evaluating COND is bound to `it'.

The variable `it' is available within THEN and ELSE.

COND, THEN, and ELSE are otherwise as documented for `if'.

(fn COND THEN &rest ELSE)" nil t) (function-put 'anaphoric-if 'lisp-indent-function '2) (autoload 'anaphoric-prog1 "anaphora" "Like `prog1', but the result of evaluating FIRST is bound to `it'.

The variable `it' is available within BODY.

FIRST and BODY are otherwise as documented for `prog1'.

(fn FIRST &rest BODY)" nil t) (function-put 'anaphoric-prog1 'lisp-indent-function '1) (autoload 'anaphoric-prog2 "anaphora" "Like `prog2', but the result of evaluating FORM2 is bound to `it'.

The variable `it' is available within BODY.

FORM1, FORM2, and BODY are otherwise as documented for `prog2'.

(fn FORM1 FORM2 &rest BODY)" nil t) (function-put 'anaphoric-prog2 'lisp-indent-function '2) (autoload 'anaphoric-when "anaphora" "Like `when', but the result of evaluating COND is bound to `it'.

The variable `it' is available within BODY.

COND and BODY are otherwise as documented for `when'.

(fn COND &rest BODY)" nil t) (function-put 'anaphoric-when 'lisp-indent-function '1) (autoload 'anaphoric-while "anaphora" "Like `while', but the result of evaluating TEST is bound to `it'.

The variable `it' is available within BODY.

TEST and BODY are otherwise as documented for `while'.

(fn TEST &rest BODY)" nil t) (function-put 'anaphoric-while 'lisp-indent-function '1) (autoload 'anaphoric-and "anaphora" "Like `and', but the result of the previous condition is bound to `it'.

The variable `it' is available within all CONDITIONS after the
initial one.

CONDITIONS are otherwise as documented for `and'.

Note that some implementations of this macro bind only the first
condition to `it', rather than each successive condition.

(fn &rest CONDITIONS)" nil t) (autoload 'anaphoric-cond "anaphora" "Like `cond', but the result of each condition is bound to `it'.

The variable `it' is available within the remainder of each of CLAUSES.

CLAUSES are otherwise as documented for `cond'.

(fn &rest CLAUSES)" nil t) (autoload 'anaphoric-lambda "anaphora" "Like `lambda', but the function may refer to itself as `self'.

ARGS and BODY are otherwise as documented for `lambda'.

(fn ARGS &rest BODY)" nil t) (function-put 'anaphoric-lambda 'lisp-indent-function 'defun) (autoload 'anaphoric-block "anaphora" "Like `block', but the result of the previous expression is bound to `it'.

The variable `it' is available within all expressions of BODY
except the initial one.

NAME and BODY are otherwise as documented for `block'.

(fn NAME &rest BODY)" nil t) (function-put 'anaphoric-block 'lisp-indent-function '1) (autoload 'anaphoric-case "anaphora" "Like `case', but the result of evaluating EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `case'.

(fn EXPR &rest CLAUSES)" nil t) (function-put 'anaphoric-case 'lisp-indent-function '1) (autoload 'anaphoric-ecase "anaphora" "Like `ecase', but the result of evaluating EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `ecase'.

(fn EXPR &rest CLAUSES)" nil t) (function-put 'anaphoric-ecase 'lisp-indent-function '1) (autoload 'anaphoric-typecase "anaphora" "Like `typecase', but the result of evaluating EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `typecase'.

(fn EXPR &rest CLAUSES)" nil t) (function-put 'anaphoric-typecase 'lisp-indent-function '1) (autoload 'anaphoric-etypecase "anaphora" "Like `etypecase', but result of evaluating EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `etypecase'.

(fn EXPR &rest CLAUSES)" nil t) (function-put 'anaphoric-etypecase 'lisp-indent-function '1) (autoload 'anaphoric-let "anaphora" "Like `let', but the result of evaluating FORM is bound to `it'.

FORM and BODY are otherwise as documented for `let'.

(fn FORM &rest BODY)" nil t) (function-put 'anaphoric-let 'lisp-indent-function '1) (autoload 'anaphoric-+ "anaphora" "Like `+', but the result of evaluating the previous expression is bound to `it'.

The variable `it' is available within all expressions after the
initial one.

NUMBERS-OR-MARKERS are otherwise as documented for `+'.

(fn &rest NUMBERS-OR-MARKERS)" nil t) (autoload 'anaphoric-- "anaphora" "Like `-', but the result of evaluating the previous expression is bound to `it'.

The variable `it' is available within all expressions after the
initial one.

NUMBER-OR-MARKER and NUMBERS-OR-MARKERS are otherwise as
documented for `-'.

(fn &optional NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)" nil t) (autoload 'anaphoric-* "anaphora" "Like `*', but the result of evaluating the previous expression is bound to `it'.

The variable `it' is available within all expressions after the
initial one.

NUMBERS-OR-MARKERS are otherwise as documented for `*'.

(fn &rest NUMBERS-OR-MARKERS)" nil t) (autoload 'anaphoric-/ "anaphora" "Like `/', but the result of evaluating the previous divisor is bound to `it'.

The variable `it' is available within all expressions after the
first divisor.

DIVIDEND, DIVISOR, and DIVISORS are otherwise as documented for `/'.

(fn DIVIDEND DIVISOR &rest DIVISORS)" nil t) (register-definition-prefixes "anaphora" '("anaphora-install-font-lock-keywords")) (provide 'anaphora-autoloads)) "request" ((request request-autoloads) (register-definition-prefixes "request" '("request-")) (provide 'request-autoloads)) "deferred" ((deferred deferred-autoloads) (register-definition-prefixes "deferred" '("deferred:")) (provide 'deferred-autoloads)) "polymode" ((polymode-core polymode-debug polymode-autoloads polymode-export polymode-classes polymode-weave polymode-methods polymode-compat polymode poly-lock polymode-base polymode-test-utils polymode-tangle) (register-definition-prefixes "poly-lock" '("poly-lock-")) (autoload 'define-polymode "polymode" "Define a new polymode MODE.
This macro defines command MODE and an indicator variable MODE
which becomes t when MODE is active and nil otherwise.

MODE command can be used as both major and minor mode. Using
polymodes as minor modes makes sense when :hostmode (see below)
is not specified, in which case polymode installs only inner
modes and doesn't touch current major mode.

Standard hook MODE-hook is run at the end of the initialization
of each polymode buffer (both indirect and base buffers).

This macro also defines the MODE-map keymap from the :keymap
argument and PARENT-map (see below) and poly-[MODE-NAME]-polymode
variable which holds an object of class `pm-polymode' which holds
the entire configuration for this polymode.

PARENT is either the polymode configuration object or a polymode
mode (there is 1-to-1 correspondence between config
objects (`pm-polymode') and mode functions). The new polymode
MODE inherits alll the behavior from PARENT except for the
overwrites specified by the keywords (see below). The new MODE
runs all the hooks from the PARENT-mode and inherits its MODE-map
from PARENT-map.

DOC is an optional documentation string. If present PARENT must
be provided, but can be nil.

BODY is executed after the complete initialization of the
polymode but before MODE-hook. It is executed once for each
polymode buffer - host buffer on initialization and every inner
buffer subsequently created.

Before the BODY code keyword arguments (i.e. alternating keywords
and values) are allowed. The following special keywords
controlling the behavior of the new MODE are supported:

:lighter Optional LIGHTER is displayed in the mode line when the
   mode is on. If omitted, it defaults to the :lighter slot of
   CONFIG object.

:keymap If nil, a new MODE-map keymap is created what directly
  inherits from the PARENT's keymap. The last keymap in the
  inheritance chain is always `polymode-minor-mode-map'. If a
  keymap it is used directly as it is. If a list of binding of
  the form (KEY . BINDING) it is merged the bindings are added to
  the newly create keymap.

:after-hook A single form which is evaluated after the mode hooks
  have been run. It should not be quoted.

Other keywords are added to the `pm-polymode' configuration
object and should be valid slots in PARENT config object or the
root config `pm-polymode' object if PARENT is nil. By far the
most frequently used slots are:

:hostmode Symbol pointing to a `pm-host-chunkmode' object
  specifying the behavior of the hostmode. If missing or nil,
  MODE will behave as a minor-mode in the sense that it will
  reuse the currently installed major mode and will install only
  the inner modes.

:innermodes List of symbols pointing to `pm-inner-chunkmode'
  objects which specify the behavior of inner modes (or submodes).

(fn MODE &optional PARENT DOC &rest BODY)" nil t) (function-put 'define-polymode 'doc-string-elt '3) (register-definition-prefixes "polymode" '("pm-" "poly")) (register-definition-prefixes "polymode-base" '("poly-")) (register-definition-prefixes "polymode-classes" '("pm-")) (register-definition-prefixes "polymode-compat" '("*span*" "pm-" "polymode-")) (defvar-local polymode-default-inner-mode nil "Inner mode for chunks with unspecified modes.
Intended to be used as local variable in polymode buffers. A
special value 'host means use the host mode.") (put 'polymode-default-inner-mode 'safe-local-variable 'symbolp) (autoload 'define-hostmode "polymode-core" "Define a hostmode with name NAME.
Optional PARENT is a name of a hostmode to be derived (cloned)
from. If missing, the optional documentation string DOC is
generated automatically. KEY-ARGS is a list of key-value pairs.
See the documentation of the class `pm-host-chunkmode' for
possible values.

(fn NAME &optional PARENT DOC &rest KEY-ARGS)" nil t) (function-put 'define-hostmode 'doc-string-elt '3) (autoload 'define-innermode "polymode-core" "Ddefine an innermode with name NAME.
Optional PARENT is a name of a innermode to be derived (cloned)
from. If missing the optional documentation string DOC is
generated automatically. KEY-ARGS is a list of key-value pairs.
See the documentation of the class `pm-inner-chunkmode' for
possible values.

(fn NAME &optional PARENT DOC &rest KEY-ARGS)" nil t) (function-put 'define-innermode 'doc-string-elt '3) (autoload 'define-auto-innermode "polymode-core" "Ddefine an auto innermode with name NAME.
Optional PARENT is a name of an auto innermode to be
derived (cloned) from. If missing the optional documentation
string DOC is generated automatically. KEY-ARGS is a list of
key-value pairs. See the documentation of the class
`pm-inner-auto-chunkmode' for possible values.

(fn NAME &optional PARENT DOC &rest KEY-ARGS)" nil t) (function-put 'define-auto-innermode 'doc-string-elt '3) (register-definition-prefixes "polymode-core" '("*span*" "polymode-")) (autoload 'pm-debug-minor-mode "polymode-debug" "Turns on/off useful facilities for debugging polymode.

If called interactively, enable Pm-Debug minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Key bindings:
\\{pm-debug-minor-mode-map}

(fn &optional ARG)" t nil) (autoload 'pm-debug-minor-mode-on "polymode-debug" nil nil nil) (put 'pm-debug-mode 'globalized-minor-mode t) (defvar pm-debug-mode nil "Non-nil if Pm-Debug mode is enabled.
See the `pm-debug-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pm-debug-mode'.") (custom-autoload 'pm-debug-mode "polymode-debug" nil) (autoload 'pm-debug-mode "polymode-debug" "Toggle Pm-Debug minor mode in all buffers.
With prefix ARG, enable Pm-Debug mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Pm-Debug minor mode is enabled in all buffers where
`pm-debug-minor-mode-on' would do it.
See `pm-debug-minor-mode' for more information on Pm-Debug minor mode.

(fn &optional ARG)" t nil) (autoload 'pm-toggle-tracing "polymode-debug" "Toggle polymode tracing.
With numeric prefix toggle tracing for that LEVEL. Currently
universal argument toggles maximum level of tracing (4). Default
level is 3.

(fn LEVEL)" t nil) (autoload 'pm-trace "polymode-debug" "Trace function FN.
Use `untrace-function' to untrace or `untrace-all' to untrace all
currently traced functions.

(fn FN)" t nil) (autoload 'pm-debug-relevant-variables "polymode-debug" "Get the relevant polymode variables.
If OUT-TYPE is 'buffer, print the variables in the dedicated
buffer, if 'message issue a message, if nil just return a list of values.

(fn &optional OUT-TYPE)" t nil) (register-definition-prefixes "polymode-debug" '("pm-")) (register-definition-prefixes "polymode-export" '("pm-" "poly")) (register-definition-prefixes "polymode-methods" '("pm-")) (register-definition-prefixes "polymode-test-utils" '("pm-")) (register-definition-prefixes "polymode-weave" '("pm-" "polymode-")) (provide 'polymode-autoloads)) "ein" ((ein-contents-api ein-scratchsheet ein-gat ein ein-kernel ein-notebook ein-completer ein-notebooklist ein-process ein-notification ein-query ein-log ein-pager poly-ein ein-websocket ein-output-area ein-markdown-mode ob-ein ein-jupyter ein-pytools ein-traceback ein-events ein-autoloads ein-node ein-kill-ring ein-classes ein-kernelinfo ein-python-send ein-cell ein-dev ein-worksheet ein-shared-output ein-core ein-ipynb-mode ein-file ein-pkg ein-ipdb ein-utils) (register-definition-prefixes "ein-cell" '("ein:")) (register-definition-prefixes "ein-classes" '("ein:")) (register-definition-prefixes "ein-contents-api" '("*ein:content-hierarchy*" "ein:")) (register-definition-prefixes "ein-core" '("*ein:" "ein:")) (autoload 'ein:dev-start-debug "ein-dev" "Start logging a bunch of stuff." t nil) (autoload 'ein:dev-stop-debug "ein-dev" "Inverse of `ein:dev-start-debug'.
Impossible to maintain because it needs to match start." t nil) (autoload 'ein:dev-bug-report-template "ein-dev" "Open a buffer with bug report template." t nil) (register-definition-prefixes "ein-dev" '("ein:dev-")) (register-definition-prefixes "ein-events" '("ein:events-")) (register-definition-prefixes "ein-file" '("*ein:file-buffername-template*" "ein:")) (autoload 'ein:gat-create "ein-gat" "

(fn &optional REFRESH)" t nil) (autoload 'ein:gat-run-local-batch "ein-gat" "

(fn &optional REFRESH)" t nil) (autoload 'ein:gat-run-local "ein-gat" "

(fn &optional REFRESH)" t nil) (autoload 'ein:gat-run-remote-batch "ein-gat" "

(fn &optional REFRESH)" t nil) (autoload 'ein:gat-run-remote "ein-gat" "

(fn &optional REFRESH)" t nil) (register-definition-prefixes "ein-gat" '("ein:gat-")) (register-definition-prefixes "ein-ipdb" '("*ein:ipdb-sessions*" "ein:ipdb-")) (autoload 'ein:ipynb-mode "ein-ipynb-mode" "A simple mode for ipynb file.

\\{ein:ipynb-mode-map}

(fn)" t nil) (add-to-list 'auto-mode-alist '("\\.ipynb\\'" . ein:ipynb-mode)) (autoload 'ein:jupyter-crib-token "ein-jupyter" "Shell out to jupyter for its credentials knowledge.  Return list of (PASSWORD TOKEN).

(fn URL-OR-PORT)" nil nil) (autoload 'ein:jupyter-crib-running-servers "ein-jupyter" "Shell out to jupyter for running servers." nil nil) (autoload 'ein:jupyter-server-start "ein-jupyter" "Start SERVER-COMMAND with `--notebook-dir' NOTEBOOK-DIRECTORY.

Login after connection established unless NO-LOGIN-P is set.
LOGIN-CALLBACK takes two arguments, the buffer created by
`ein:notebooklist-open--finish', and the url-or-port argument
of `ein:notebooklist-open*'.

With \\[universal-argument] prefix arg, prompt the user for the
server command.

(fn SERVER-COMMAND NOTEBOOK-DIRECTORY &optional NO-LOGIN-P LOGIN-CALLBACK PORT)" t nil) (defalias 'ein:run 'ein:jupyter-server-start) (defalias 'ein:stop 'ein:jupyter-server-stop) (autoload 'ein:jupyter-server-stop "ein-jupyter" "

(fn &optional ASK-P URL-OR-PORT)" t nil) (register-definition-prefixes "ein-jupyter" '("*ein:jupyter-server-" "ein:")) (defalias 'ein:kernel-url-or-port 'ein:$kernel-url-or-port) (defalias 'ein:kernel-id 'ein:$kernel-kernel-id) (register-definition-prefixes "ein-kernel" '("ein:")) (register-definition-prefixes "ein-kernelinfo" '("ein:kernelinfo")) (register-definition-prefixes "ein-kill-ring" '("ein:")) (register-definition-prefixes "ein-log" '("ein:")) (autoload 'ein:markdown-mode "ein-markdown-mode" "Major mode for editing ein:markdown files.

(fn)" t nil) (register-definition-prefixes "ein-markdown-mode" '("defun-markdown-" "ein:markdown")) (register-definition-prefixes "ein-node" '("ein:node-")) (defalias 'ein:notebook-name 'ein:$notebook-notebook-name) (autoload 'ein:notebook-jump-to-opened-notebook "ein-notebook" "List all opened notebook buffers and switch to one that the user selects.

(fn NOTEBOOK)" t nil) (autoload 'ein:notebook-open "ein-notebook" "Returns notebook at URL-OR-PORT/PATH.

Note that notebook sends for its contents and won't have them right away.

After the notebook is opened, CALLBACK is called as::

  (funcall CALLBACK notebook created)

where `created' indicates a new notebook or an existing one.

(fn URL-OR-PORT PATH &optional KERNELSPEC CALLBACK ERRBACK NO-POP)" t nil) (register-definition-prefixes "ein-notebook" '("*ein:notebook--pending-query*" "ein:")) (autoload 'ein:notebooklist-reload "ein-notebooklist" "Reload current Notebook list.

(fn &optional NBLIST RESYNC CALLBACK)" t nil) (autoload 'ein:notebooklist-new-notebook "ein-notebooklist" "

(fn URL-OR-PORT KERNELSPEC &optional CALLBACK NO-POP RETRY EXPLICIT-PATH)" t nil) (autoload 'ein:notebooklist-new-notebook-with-name "ein-notebooklist" "Upon notebook-open, rename the notebook, then funcall CALLBACK.

(fn URL-OR-PORT KERNELSPEC NAME &optional CALLBACK NO-POP)" t nil) (autoload 'ein:notebooklist-list-paths "ein-notebooklist" "Return all files of CONTENT-TYPE for all sessions

(fn &optional CONTENT-TYPE)" nil nil) (autoload 'ein:notebooklist-load "ein-notebooklist" "Load notebook list but do not pop-up the notebook list buffer.

For example, if you want to load notebook list when Emacs starts,
add this in the Emacs initialization file::

  (add-to-hook 'after-init-hook 'ein:notebooklist-load)

or even this (if you want fast Emacs start-up)::

  ;; load notebook list if Emacs is idle for 3 sec after start-up
  (run-with-idle-timer 3 nil #'ein:notebooklist-load)

(fn &optional URL-OR-PORT)" nil nil) (autoload 'ein:notebooklist-open "ein-notebooklist" "This is now an alias for `ein:notebooklist-login'.

(fn URL-OR-PORT CALLBACK)" t nil) (defalias 'ein:login 'ein:notebooklist-login) (autoload 'ein:notebooklist-login "ein-notebooklist" "Deal with security before main entry of ein:notebooklist-open*.

CALLBACK takes two arguments, the buffer created by ein:notebooklist-open--success
and the url-or-port argument of ein:notebooklist-open*.

(fn URL-OR-PORT CALLBACK &optional COOKIE-NAME COOKIE-CONTENT TOKEN)" t nil) (register-definition-prefixes "ein-notebooklist" '("ein:" "generate-breadcrumbs" "render-")) (register-definition-prefixes "ein-notification" '("ein:")) (register-definition-prefixes "ein-output-area" '("ein:")) (register-definition-prefixes "ein-pager" '("ein:pager-")) (register-definition-prefixes "ein-process" '("ein:process-")) (register-definition-prefixes "ein-python-send" '("ein:python-send-")) (register-definition-prefixes "ein-pytools" '("ein:pytools-jump-")) (register-definition-prefixes "ein-query" '("ein:")) (register-definition-prefixes "ein-scratchsheet" '("ein:scratchsheet")) (autoload 'ein:shared-output-pop-to-buffer "ein-shared-output" "Open shared output buffer." t nil) (autoload 'ein:shared-output-show-code-cell-at-point "ein-shared-output" "Show code cell at point in shared-output buffer.
It is useful when the output of the cell at point is truncated.
See also `ein:cell-max-num-outputs'." t nil) (autoload 'ein:shared-output-eval-string "ein-shared-output" "Entry to `ein:cell-execute-internal' from the shared output cell.

(fn KERNEL CODE &rest ARGS)" nil nil) (register-definition-prefixes "ein-shared-output" '("*ein:shared-output*" "ein:")) (autoload 'ein:tb-show "ein-traceback" "Show full traceback in traceback viewer." t nil) (register-definition-prefixes "ein-traceback" '("ein:t")) (register-definition-prefixes "ein-utils" '("ein:")) (register-definition-prefixes "ein-websocket" '("ein:" "fix-request-netscape-cookie-parse")) (register-definition-prefixes "ein-worksheet" '("ein:" "hof-add")) (when (featurep 'org) (let* ((orig (get 'org-babel-load-languages 'custom-type)) (orig-cdr (cdr orig)) (choices (plist-get orig-cdr :key-type))) (push '(const :tag "Ein" ein) (nthcdr 1 choices)) (put 'org-babel-load-languages 'custom-type (cons (car orig) (plist-put orig-cdr :key-type choices))))) (register-definition-prefixes "ob-ein" '("*ob-ein-sentinel*" "ob-ein-")) (autoload 'poly-ein-mode "poly-ein") (register-definition-prefixes "poly-ein" '("pm-" "poly-ein-")) (provide 'ein-autoloads)) "tablist" ((tablist-autoloads tablist tablist-filter) (autoload 'tablist-minor-mode "tablist" "Toggle Tablist minor mode on or off.

If called interactively, enable Tablist minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{tablist-minor-mode-map}

(fn &optional ARG)" t nil) (autoload 'tablist-mode "tablist" "

(fn)" t nil) (register-definition-prefixes "tablist" '("tablist-")) (register-definition-prefixes "tablist-filter" '("tablist-filter-")) (provide 'tablist-autoloads)) "pdf-tools" ((pdf-view pdf-util pdf-dev pdf-tools pdf-outline pdf-links pdf-history pdf-isearch pdf-loader pdf-cache pdf-misc pdf-sync pdf-annot pdf-tools-autoloads pdf-macs pdf-virtual pdf-info pdf-occur) (autoload 'pdf-annot-minor-mode "pdf-annot" "Support for PDF Annotations.

This is a minor mode.  If called interactively, toggle the
`Pdf-Annot minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-annot-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{pdf-annot-minor-mode-map}

(fn &optional ARG)" t nil) (register-definition-prefixes "pdf-annot" '("pdf-annot-")) (register-definition-prefixes "pdf-cache" '("boundingbox" "define-pdf-cache-function" "page" "pdf-cache-" "textregions")) (register-definition-prefixes "pdf-dev" '("pdf-dev-")) (autoload 'pdf-history-minor-mode "pdf-history" "Keep a history of previously visited pages.

This is a minor mode.  If called interactively, toggle the
`Pdf-History minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-history-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This is a simple stack-based history.  Turning the page or
following a link pushes the left-behind page on the stack, which
may be navigated with the following keys.

\\{pdf-history-minor-mode-map}

(fn &optional ARG)" t nil) (register-definition-prefixes "pdf-history" '("pdf-history-")) (register-definition-prefixes "pdf-info" '("pdf-info-")) (autoload 'pdf-isearch-minor-mode "pdf-isearch" "Isearch mode for PDF buffer.

This is a minor mode.  If called interactively, toggle the
`Pdf-Isearch minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-isearch-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When this mode is enabled \\[isearch-forward], among other keys,
starts an incremental search in this PDF document.  Since this mode
uses external programs to highlight found matches via
image-processing, proceeding to the next match may be slow.

Therefore two isearch behaviours have been defined: Normal isearch and
batch mode.  The later one is a minor mode
(`pdf-isearch-batch-mode'), which when activated inhibits isearch
from stopping at and highlighting every single match, but rather
display them batch-wise.  Here a batch means a number of matches
currently visible in the selected window.

The kind of highlighting is determined by three faces
`pdf-isearch-match' (for the current match), `pdf-isearch-lazy'
(for all other matches) and `pdf-isearch-batch' (when in batch
mode), which see.

Colors may also be influenced by the minor-mode
`pdf-view-dark-minor-mode'.  If this is minor mode enabled, each face's
dark colors, are used (see e.g. `frame-background-mode'), instead
of the light ones.

\\{pdf-isearch-minor-mode-map}
While in `isearch-mode' the following keys are available. Note
that not every isearch command work as expected.

\\{pdf-isearch-active-mode-map}

(fn &optional ARG)" t nil) (register-definition-prefixes "pdf-isearch" '("pdf-isearch-")) (autoload 'pdf-links-minor-mode "pdf-links" "Handle links in PDF documents.\\<pdf-links-minor-mode-map>

This is a minor mode.  If called interactively, toggle the
`Pdf-Links minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-links-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

If this mode is enabled, most links in the document may be
activated by clicking on them or by pressing \\[pdf-links-action-perform] and selecting
one of the displayed keys, or by using isearch limited to
links via \\[pdf-links-isearch-link].

\\{pdf-links-minor-mode-map}

(fn &optional ARG)" t nil) (autoload 'pdf-links-action-perform "pdf-links" "Follow LINK, depending on its type.

This may turn to another page, switch to another PDF buffer or
invoke `pdf-links-browse-uri-function'.

Interactively, link is read via `pdf-links-read-link-action'.
This function displays characters around the links in the current
page and starts reading characters (ignoring case).  After a
sufficient number of characters have been read, the corresponding
link's link is invoked.  Additionally, SPC may be used to
scroll the current page.

(fn LINK)" t nil) (register-definition-prefixes "pdf-links" '("pdf-links-")) (autoload 'pdf-loader-install "pdf-loader" "Prepare Emacs for using PDF Tools.

This function acts as a replacement for `pdf-tools-install' and
makes Emacs load and use PDF Tools as soon as a PDF file is
opened, but not sooner.

The arguments are passed verbatim to `pdf-tools-install', which
see.

(fn &optional NO-QUERY-P SKIP-DEPENDENCIES-P NO-ERROR-P FORCE-DEPENDENCIES-P)" nil nil) (register-definition-prefixes "pdf-loader" '("pdf-loader--")) (register-definition-prefixes "pdf-macs" '("pdf-view-")) (autoload 'pdf-misc-minor-mode "pdf-misc" "FIXME:  Not documented.

This is a minor mode.  If called interactively, toggle the
`Pdf-Misc minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-misc-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'pdf-misc-size-indication-minor-mode "pdf-misc" "Provide a working size indication in the mode-line.

This is a minor mode.  If called interactively, toggle the
`Pdf-Misc-Size-Indication minor mode' mode.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-misc-size-indication-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'pdf-misc-menu-bar-minor-mode "pdf-misc" "Display a PDF Tools menu in the menu-bar.

This is a minor mode.  If called interactively, toggle the
`Pdf-Misc-Menu-Bar minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-misc-menu-bar-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'pdf-misc-context-menu-minor-mode "pdf-misc" "Provide a right-click context menu in PDF buffers.

This is a minor mode.  If called interactively, toggle the
`Pdf-Misc-Context-Menu minor mode' mode.  If the prefix argument
is positive, enable the mode, and if it is zero or negative,
disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-misc-context-menu-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{pdf-misc-context-menu-minor-mode-map}

(fn &optional ARG)" t nil) (register-definition-prefixes "pdf-misc" '("pdf-misc-")) (autoload 'pdf-occur "pdf-occur" "List lines matching STRING or PCRE.

Interactively search for a regexp. Unless a prefix arg was given,
in which case this functions performs a string search.

If `pdf-occur-prefer-string-search' is non-nil, the meaning of
the prefix-arg is inverted.

(fn STRING &optional REGEXP-P)" t nil) (autoload 'pdf-occur-multi-command "pdf-occur" "Perform `pdf-occur' on multiple buffer.

For a programmatic search of multiple documents see
`pdf-occur-search'." t nil) (defvar pdf-occur-global-minor-mode nil "Non-nil if Pdf-Occur-Global minor mode is enabled.
See the `pdf-occur-global-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pdf-occur-global-minor-mode'.") (custom-autoload 'pdf-occur-global-minor-mode "pdf-occur" nil) (autoload 'pdf-occur-global-minor-mode "pdf-occur" "Enable integration of Pdf Occur with other modes.

This is a minor mode.  If called interactively, toggle the
`Pdf-Occur-Global minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'pdf-occur-global-minor-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This global minor mode enables (or disables)
`pdf-occur-ibuffer-minor-mode' and `pdf-occur-dired-minor-mode'
in all current and future ibuffer/dired buffer.

(fn &optional ARG)" t nil) (autoload 'pdf-occur-ibuffer-minor-mode "pdf-occur" "Hack into ibuffer's do-occur binding.

This is a minor mode.  If called interactively, toggle the
`Pdf-Occur-Ibuffer minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-occur-ibuffer-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This mode remaps `ibuffer-do-occur' to
`pdf-occur-ibuffer-do-occur', which will start the PDF Tools
version of `occur', if all marked buffer's are in `pdf-view-mode'
and otherwise fallback to `ibuffer-do-occur'.

(fn &optional ARG)" t nil) (autoload 'pdf-occur-dired-minor-mode "pdf-occur" "Hack into dired's `dired-do-search' binding.

This is a minor mode.  If called interactively, toggle the
`Pdf-Occur-Dired minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-occur-dired-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This mode remaps `dired-do-search' to
`pdf-occur-dired-do-search', which will start the PDF Tools
version of `occur', if all marked buffer's are in `pdf-view-mode'
and otherwise fallback to `dired-do-search'.

(fn &optional ARG)" t nil) (register-definition-prefixes "pdf-occur" '("pdf-occur-")) (autoload 'pdf-outline-minor-mode "pdf-outline" "Display an outline of a PDF document.

This is a minor mode.  If called interactively, toggle the
`Pdf-Outline minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-outline-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This provides a PDF's outline on the menu bar via imenu.
Additionally the same outline may be viewed in a designated
buffer.

\\{pdf-outline-minor-mode-map}

(fn &optional ARG)" t nil) (autoload 'pdf-outline "pdf-outline" "Display an PDF outline of BUFFER.

BUFFER defaults to the current buffer.  Select the outline
buffer, unless NO-SELECT-WINDOW-P is non-nil.

(fn &optional BUFFER NO-SELECT-WINDOW-P)" t nil) (autoload 'pdf-outline-imenu-enable "pdf-outline" "Enable imenu in the current PDF buffer." t nil) (register-definition-prefixes "pdf-outline" '("pdf-outline")) (autoload 'pdf-sync-minor-mode "pdf-sync" "Correlate a PDF position with the TeX file.
\\<pdf-sync-minor-mode-map>
This works via SyncTeX, which means the TeX sources need to have
been compiled with `--synctex=1'.  In AUCTeX this can be done by
setting `TeX-source-correlate-method' to 'synctex (before AUCTeX
is loaded) and enabling `TeX-source-correlate-mode'.

This is a minor mode.  If called interactively, toggle the
`Pdf-Sync minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-sync-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Then \\[pdf-sync-backward-search-mouse] in the PDF buffer will open the
corresponding TeX location.

If AUCTeX is your preferred tex-mode, this library arranges to
bind `pdf-sync-forward-display-pdf-key' (the default is `C-c C-g')
to `pdf-sync-forward-search' in `TeX-source-correlate-map'.  This
function displays the PDF page corresponding to the current
position in the TeX buffer.  This function only works together
with AUCTeX.

(fn &optional ARG)" t nil) (register-definition-prefixes "pdf-sync" '("pdf-sync-")) (defvar pdf-tools-handle-upgrades t "Whether PDF Tools should handle upgrading itself.") (custom-autoload 'pdf-tools-handle-upgrades "pdf-tools" t) (autoload 'pdf-tools-install "pdf-tools" "Install PDF-Tools in all current and future PDF buffers.

If the `pdf-info-epdfinfo-program' is not running or does not
appear to be working, attempt to rebuild it.  If this build
succeeded, continue with the activation of the package.
Otherwise fail silently, i.e. no error is signaled.

Build the program (if necessary) without asking first, if
NO-QUERY-P is non-nil.

Don't attempt to install system packages, if SKIP-DEPENDENCIES-P
is non-nil.

Do not signal an error in case the build failed, if NO-ERROR-P is
non-nil.

Attempt to install system packages (even if it is deemed
unnecessary), if FORCE-DEPENDENCIES-P is non-nil.

Note that SKIP-DEPENDENCIES-P and FORCE-DEPENDENCIES-P are
mutually exclusive.

Note further, that you can influence the installation directory
by setting `pdf-info-epdfinfo-program' to an appropriate
value (e.g. ~/bin/epdfinfo) before calling this function.

See `pdf-view-mode' and `pdf-tools-enabled-modes'.

(fn &optional NO-QUERY-P SKIP-DEPENDENCIES-P NO-ERROR-P FORCE-DEPENDENCIES-P)" t nil) (autoload 'pdf-tools-enable-minor-modes "pdf-tools" "Enable MODES in the current buffer.

MODES defaults to `pdf-tools-enabled-modes'.

(fn &optional MODES)" t nil) (autoload 'pdf-tools-help "pdf-tools" nil t nil) (register-definition-prefixes "pdf-tools" '("pdf-tools-")) (register-definition-prefixes "pdf-util" '("display-buffer-split-below-and-attach" "pdf-util-")) (autoload 'pdf-view-bookmark-jump-handler "pdf-view" "The bookmark handler-function interface for bookmark BMK.

See also `pdf-view-bookmark-make-record'.

(fn BMK)" nil nil) (register-definition-prefixes "pdf-view" '("pdf-view-")) (autoload 'pdf-virtual-edit-mode "pdf-virtual" "Major mode when editing a virtual PDF buffer.

(fn)" t nil) (autoload 'pdf-virtual-view-mode "pdf-virtual" "Major mode in virtual PDF buffers.

(fn)" t nil) (defvar pdf-virtual-global-minor-mode nil "Non-nil if Pdf-Virtual-Global minor mode is enabled.
See the `pdf-virtual-global-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pdf-virtual-global-minor-mode'.") (custom-autoload 'pdf-virtual-global-minor-mode "pdf-virtual" nil) (autoload 'pdf-virtual-global-minor-mode "pdf-virtual" "Enable recognition and handling of VPDF files.

This is a minor mode.  If called interactively, toggle the
`Pdf-Virtual-Global minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'pdf-virtual-global-minor-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'pdf-virtual-buffer-create "pdf-virtual" "

(fn &optional FILENAMES BUFFER-NAME DISPLAY-P)" t nil) (register-definition-prefixes "pdf-virtual" '("pdf-virtual-")) (provide 'pdf-tools-autoloads)) "yatex" ((yatexhks yatexm-o yatex19 yatexpkg yatexadd yatexsec yatex yatexenv yatexmth yahtml yatexhlp yatex-autoloads yatexflt yatexinf yatexgen yatexprc yatexhie yatexlib yatex23) (register-definition-prefixes "yahtml" '("yahtml")) (register-definition-prefixes "yatex" '("LaTeX2e-fontstyle-alist" "NTT-jTeX" "YaTeX-" "bibtex-command" "dvi" "env-table" "fontsize-table" "latex-" "makeindex-command" "section-table" "singlecmd-table" "tex-" "tmp-" "user-" "yatex-mode")) (register-definition-prefixes "yatex19" '("YaTeX-" "hilit-patterns-alist")) (register-definition-prefixes "yatex23" '("YaTeX-")) (register-definition-prefixes "yatexadd" '("YaTeX")) (autoload 'YaTeX-what-column "yatexenv" "Show which kind of column the current position is belonging to." t nil) (autoload 'YaTeX-intelligent-newline "yatexenv" "Insert newline and environment-specific entry.
`\\item'	for some itemizing environment,
`\\> \\> \\'	for tabbing environemnt,
`& & \\ hline'	for tabular environment.

(fn ARG)" t nil) (autoload 'YaTeX-indent-line-equation "yatexenv" "Indent a line in equation family." nil nil) (autoload 'YaTeX-goto-corresponding-leftright "yatexenv" "Go to corresponding left or ight." nil nil) (register-definition-prefixes "yatexenv" '("YaTeX-")) (autoload 'YaTeX-filter-goto-source "yatexflt" "Go to corresponding text source of the graphic file

(fn FILE OTHER-WIN)" nil nil) (register-definition-prefixes "yatexflt" '("YaTeX-")) (register-definition-prefixes "yatexgen" '("YaTeX-")) (register-definition-prefixes "yatexhie" '("YaTeX-")) (register-definition-prefixes "yatexhks" '("YaTeX-browse-info")) (autoload 'YaTeX-apropos "yatexhlp" "

(fn KEY)" t nil) (autoload 'YaTeX-help "yatexhlp" "Show help buffer of LaTeX/TeX commands or macros.
Optional argument MACRO, if supplied, is directly selected to keyword.
Non-nil for optional second argument REF-ONLY inhibits call enrich-help
for non-interactive use.

(fn &optional MACRO REF-ONLY)" t nil) (register-definition-prefixes "yatexhlp" '("YaTeX-")) (register-definition-prefixes "yatexinf" '("yatexinfo-")) (autoload 'YaTeX-kanji-ptex-mnemonic "yatexlib" "Return the kanji-mnemonic of pTeX from current buffer's coding-system." nil nil) (autoload 'YaTeX-define-key "yatexlib" "Define key on YaTeX-prefix-map.

(fn KEY BINDING &optional MAP)" nil nil) (autoload 'YaTeX-local-table-symbol "yatexlib" "Return the lisp symbol which keeps local completion table of SYMBOL.

(fn SYMBOL)" nil nil) (autoload 'YaTeX-sync-local-table "yatexlib" "Synchronize local variable SYMBOL.
Copy its corresponding directory dependent completion table to SYMBOL.

(fn SYMBOL)" nil nil) (autoload 'YaTeX-read-user-completion-table "yatexlib" "Append user completion table of LaTeX macros

(fn &optional FORCETOREAD)" t nil) (autoload 'YaTeX-reload-dictionary "yatexlib" "Reload local dictionary.
Use this function after editing ./.yatexrc." t nil) (autoload 'YaTeX-lookup-table "yatexlib" "Lookup WORD in completion table whose type is TYPE.
This function refers the symbol tmp-TYPE-table, user-TYPE-table, TYPE-table.
Typically, TYPE is one of 'env, 'section, 'fontsize, 'singlecmd.

(fn WORD TYPE)" nil nil) (autoload 'YaTeX-update-table "yatexlib" "Update completion table if the car of VALLIST is not in current tables.
Second argument DEFAULT-TABLE is the quoted symbol of default completion
table, third argument USER-TABLE is user table which will be saved in
YaTeX-user-completion-table, fourth argument LOCAL-TABLE should have the
completion which is valid during current Emacs's session.  If you
want to make LOCAL-TABLE valid longer span (but restrict in this directory)
create the file in current directory which has the same name with
YaTeX-user-completion-table.

(fn VALLIST DEFAULT-TABLE USER-TABLE LOCAL-TABLE)" nil nil) (autoload 'YaTeX-cplread-with-learning "yatexlib" "Completing read with learning.
Do a completing read with prompt PROM.  Completion table is what
DEFAULT-TABLE, USER-TABLE, LOCAL table are appended in reverse order.
Note that these tables are passed by the symbol.
Optional arguments PRED, REQMATH and INIT are passed to completing-read
as its arguments PREDICATE, REQUIRE-MATCH and INITIAL-INPUT respectively.
If optional 8th argument HSYM, history symbol, is passed, use it as
history list variable.

(fn PROM DEFAULT-TABLE USER-TABLE LOCAL-TABLE &optional PRED REQMATCH INIT HSYM)" nil nil) (autoload 'YaTeX-update-dictionary "yatexlib" "

(fn FILE SYMBOL &optional TYPE)" nil nil) (autoload 'YaTeX-define-begend-key-normal "yatexlib" "Define short cut YaTeX-make-begin-end key.

(fn KEY ENV &optional MAP)" nil nil) (autoload 'YaTeX-define-begend-region-key "yatexlib" "Define short cut YaTeX-make-begin-end-region key.

(fn KEY ENV &optional MAP)" nil nil) (autoload 'YaTeX-define-begend-key "yatexlib" "Define short cut key for begin type completion.
Define both strokes for normal and region mode.
To customize YaTeX, user should use this function.

(fn KEY ENV &optional MAP)" nil nil) (autoload 'YaTeX-search-active-forward "yatexlib" "Search STRING which is not commented out by CMNTRX.
Optional arguments after BOUND, ERR, CNT are passed literally to search-forward
or search-backward.
Optional sixth argument FUNC changes search-function.

(fn STRING CMNTRX &optional BOUND ERR CNT FUNC)" nil nil) (autoload 'YaTeX-switch-to-buffer "yatexlib" "Switch to buffer if buffer exists, find file if not.
Optional second arg SETBUF t make use set-buffer instead of switch-to-buffer.

(fn FILE &optional SETBUF)" t nil) (autoload 'YaTeX-switch-to-buffer-other-window "yatexlib" "Switch to buffer if buffer exists, find file if not.

(fn FILE)" t nil) (autoload 'YaTeX-replace-format "yatexlib" "In STRING, replace first appearance of FORMAT to REPL as if
function `format' does.  FORMAT does not contain `%'

(fn STRING FORMAT REPL)" nil nil) (autoload 'YaTeX-replace-formats "yatexlib" "

(fn STRING REPLACE-LIST)" nil nil) (autoload 'YaTeX-replace-format-args "yatexlib" "Translate the argument mark #1, #2, ... #n in the STRING into the
corresponding real arguments ARGS.

(fn STRING &rest ARGS)" nil nil) (autoload 'rindex "yatexlib" "Return the last position of STRING where character CHAR found.

(fn STRING CHAR)" nil nil) (autoload 'point-beginning-of-line "yatexlib" nil nil nil) (autoload 'point-end-of-line "yatexlib" nil nil nil) (autoload 'YaTeX-showup-buffer "yatexlib" "Make BUFFER show up in certain window (but current window)
that gives the maximum value by the FUNC.  FUNC should take an argument
of its window object.  Non-nil for optional third argument SELECT selects
that window.  This function never selects minibuffer window.

(fn BUFFER &optional FUNC SELECT)" nil nil) (autoload 'split-window-calculate-height "yatexlib" "Split current window wight specified HEIGHT.
If HEIGHT is number, make a new window that has HEIGHT lines.
If HEIGHT is string, make a new window that occupies HEIGT % of screen height.
Otherwise split window conventionally.

(fn HEIGHT)" nil nil) (autoload 'YaTeX-window-list "yatexlib" nil nil nil) (autoload 'substitute-all-key-definition "yatexlib" "Replace recursively OLDDEF with NEWDEF for any keys in KEYMAP now
defined as OLDDEF. In other words, OLDDEF is replaced with NEWDEF
where ever it appears.

(fn OLDDEF NEWDEF KEYMAP)" nil nil) (autoload 'YaTeX-match-string "yatexlib" "Return (buffer-substring (match-beginning n) (match-beginning m)).

(fn N &optional M)" nil nil) (autoload 'YaTeX-minibuffer-complete "yatexlib" "Complete in minibuffer.
  If the symbol 'delim is bound and is string, its value is assumed to be
the character class of delimiters.  Completion will be performed on
the last field separated by those delimiters.
  If the symbol 'quick is bound and is 't, when the try-completion results
in t, exit minibuffer immediately." t nil) (autoload 'completing-read-with-history "yatexlib" "Completing read with general history: gmhist, Emacs-19.

(fn PROMPT TABLE &optional PREDICATE MUST-MATCH INITIAL HSYM)" nil nil) (autoload 'read-from-minibuffer-with-history "yatexlib" "Read from minibuffer with general history: gmhist, Emacs-19.

(fn PROMPT &optional INIT MAP READ HSYM)" nil nil) (autoload 'read-string-with-history "yatexlib" "Read string with history: gmhist(Emacs-18) and Emacs-19.

(fn PROMPT &optional INIT HSYM)" nil nil) (fset 'YaTeX-rassoc (if (and nil (fboundp 'rassoc) (subrp (symbol-function 'rassoc))) (symbol-function 'rassoc) #'(lambda (key list) (let ((l list)) (catch 'found (while l (if (equal key (cdr (car l))) (throw 'found (car l))) (setq l (cdr l)))))))) (autoload 'YaTeX-delete1 "yatexlib" "Delete

(fn ELT LIST)" nil nil) (fset 'YaTeX-last-key (if (fboundp 'win:last-key) 'win:last-key #'(lambda nil (if (boundp 'last-command-char) last-command-char last-command-event)))) (autoload 'YaTeX-command-to-string "yatexlib" "

(fn CMD)" nil nil) (autoload 'YaTeX-reindent "yatexlib" "Remove current indentation and reindento to COL column.

(fn COL)" nil nil) (register-definition-prefixes "yatexlib" '("YaTeX-" "bcf-and-exit" "foreach-buffers" "goto-buffer-window" "latex-message-kanji-code" "tfb-and-exit")) (register-definition-prefixes "yatexm-o" '("yatex-mode-")) (autoload 'YaTeX-toggle-math-mode "yatexmth" "

(fn &optional ARG)" t nil) (autoload 'YaTeX-goto-corresponding-paren "yatexmth" "Go to corresponding mathematical parentheses." nil nil) (register-definition-prefixes "yatexmth" '("YaTeX-")) (register-definition-prefixes "yatexpkg" '("YaTeX")) (register-definition-prefixes "yatexprc" '("YaTeX-")) (autoload 'YaTeX-read-section-in-minibuffer "yatexsec" "

(fn PROMPT TABLE &optional DEFAULT DELIM)" t nil) (autoload 'YaTeX-make-section-with-overview "yatexsec" "Input sectining command with previous overview." t nil) (register-definition-prefixes "yatexsec" '("YaTeX-")) (provide 'yatex-autoloads)) "rainbow-mode" ((rainbow-mode-autoloads rainbow-mode) (autoload 'rainbow-mode "rainbow-mode" "Colorize strings that represent colors.
This will fontify with colors the string like \"#aabbcc\" or \"blue\".

If called interactively, enable Rainbow mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "rainbow-mode" '("rainbow-")) (provide 'rainbow-mode-autoloads)) "migemo" ((migemo migemo-autoloads) (register-definition-prefixes "migemo" '("migemo-")) (provide 'migemo-autoloads)) "google-translate" ((google-translate-core-ui \.bump-version google-translate-core google-translate-default-ui google-translate-tk google-translate google-translate-backend google-translate-pkg google-translate-smooth-ui google-translate-autoloads) (register-definition-prefixes "google-translate-backend" '("google-translate-backend-")) (register-definition-prefixes "google-translate-core" '("google-translate-")) (register-definition-prefixes "google-translate-core-ui" '("google-translate-")) (autoload 'google-translate-query-translate "google-translate-default-ui" "Interactively translate text with Google Translate.

Query a text (a word or a phrase), and pop up a buffer named *Google
Translate* displaying available translations of the text.

If no defaults for the source and target languages are specified (by
setting the variables `google-translate-default-source-language' and
`google-translate-default-target-language'), interactively query the
missing parts.  For example, a reasonable option may be to specify a
default for the target language and always be queried for the source
language.

With a `C-u' prefix argument, query the source and target languages,
even if any defaults are specified.  For example, you may frequently
need to translate from English to Russian, and you may choose to set
the default source and target languages to \"en\" and  \"ru\", resp.
However, occasionally you may also need to translate from Russian to
English.  With a `C-u' prefix argument you can override the defaults
and specify the source and target languages explicitly.

The languages are queried with completion, and the null input at the
source language prompt is considered as an instruction for Google
Translate to detect the source language.

(fn &optional OVERRIDE-P)" t nil) (autoload 'google-translate-query-translate-reverse "google-translate-default-ui" "Like `google-translate-query-translate', but performs translation
in the reverse direction.

The value of the variable `google-translate-default-source-language'
(if set) becomes the target language, and the value of the variable
`google-translate-default-target-language' (if also set) becomes the
source language.

In particular, when both variables are set, translation is performed
in the reverse direction.

(fn &optional OVERRIDE-P)" t nil) (autoload 'google-translate-at-point "google-translate-default-ui" "Translate the word at point or the words in the active region.

For the meaning of OVERRIDE-P, see `google-translate-query-translate'.

(fn &optional OVERRIDE-P)" t nil) (autoload 'google-translate-at-point-reverse "google-translate-default-ui" "Like `google-translate-at-point', but performs translation in the
reverse direction.

(fn &optional OVERRIDE-P)" t nil) (autoload 'google-translate-buffer "google-translate-default-ui" "Translate current buffer.

For the meaning of OVERRIDE-P, see `google-translate-query-translate'.

(fn &optional OVERRIDE-P REVERSE-P)" t nil) (autoload 'google-translate-paragraphs-overlay "google-translate-default-ui" "Translate current buffer with paragraph by paragraph and SHOW results in overlay below paragraph.
This command also specificly support org-mode.

(fn &optional OVERRIDE-P REVERSE-P)" t nil) (autoload 'google-translate-paragraphs-insert "google-translate-default-ui" "Translate current buffer with paragraph by paragraph and INSERT results below paragraph.
This command does NOT support document format like org-mode.

(fn &optional OVERRIDE-P REVERSE-P)" t nil) (register-definition-prefixes "google-translate-default-ui" '("%google-translate-" "google-translate-")) (autoload 'google-translate-smooth-translate "google-translate-smooth-ui" "Translate a text using translation directions.

Make a prompt in minibuffer for a text to translate. Default text
is word at point.

In case of `google-translate-translation-directions-alist' is
empty list then after inputed translating text prompts for source
language and then for target languages.

In case of `google-translate-translation-directions-alist' is not
empty list takes current translation direction and makes
appropriate translation. Current translation direction indicates
in the minibuffers' prompt.

A current translation direction could be changed directly in the
minibuffer by means of key bindings such as C-n and C-p for
changing to the next translation direction and to the previous
one respectively." t nil) (register-definition-prefixes "google-translate-smooth-ui" '("google-translate-")) (register-definition-prefixes "google-translate-tk" '("google-translate--")) (provide 'google-translate-autoloads)) "avy" ((avy avy-autoloads) (autoload 'avy-process "avy" "Select one of CANDIDATES using `avy-read'.
Use OVERLAY-FN to visualize the decision overlay.
CLEANUP-FN should take no arguments and remove the effects of
multiple OVERLAY-FN invocations.

(fn CANDIDATES &optional OVERLAY-FN CLEANUP-FN)" nil nil) (autoload 'avy-goto-char "avy" "Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-char-in-line "avy" "Jump to the currently visible CHAR in the current line.

(fn CHAR)" t nil) (autoload 'avy-goto-char-2 "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn CHAR1 CHAR2 &optional ARG BEG END)" t nil) (autoload 'avy-goto-char-2-above "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t nil) (autoload 'avy-goto-char-2-below "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t nil) (autoload 'avy-isearch "avy" "Jump to one of the current isearch candidates." t nil) (autoload 'avy-goto-word-0 "avy" "Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t nil) (autoload 'avy-goto-whitespace-end "avy" "Jump to the end of a whitespace sequence.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t nil) (autoload 'avy-goto-word-1 "avy" "Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start.

(fn CHAR &optional ARG BEG END SYMBOL)" t nil) (autoload 'avy-goto-word-1-above "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-word-1-below "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-symbol-1 "avy" "Jump to the currently visible CHAR at a symbol start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-symbol-1-above "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-symbol-1-below "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-subword-0 "avy" "Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true.

BEG and END narrow the scope where candidates are searched.

(fn &optional ARG PREDICATE BEG END)" t nil) (autoload 'avy-goto-subword-1 "avy" "Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-word-or-subword-1 "avy" "Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'." t nil) (autoload 'avy-goto-line "avy" "Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG.

(fn &optional ARG)" t nil) (autoload 'avy-goto-line-above "avy" "Goto visible line above the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t nil) (autoload 'avy-goto-line-below "avy" "Goto visible line below the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t nil) (autoload 'avy-goto-end-of-line "avy" "Call `avy-goto-line' and move to the end of the line.

(fn &optional ARG)" t nil) (autoload 'avy-copy-line "avy" "Copy a selected line above the current line.
ARG lines can be used.

(fn ARG)" t nil) (autoload 'avy-move-line "avy" "Move a selected line above the current line.
ARG lines can be used.

(fn ARG)" t nil) (autoload 'avy-copy-region "avy" "Select two lines and copy the text between them to point.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t nil) (autoload 'avy-move-region "avy" "Select two lines and move the text between them above the current line." t nil) (autoload 'avy-kill-region "avy" "Select two lines and kill the region between them.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t nil) (autoload 'avy-kill-ring-save-region "avy" "Select two lines and save the region between them to the kill ring.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn ARG)" t nil) (autoload 'avy-kill-whole-line "avy" "Select line and kill the whole selected line.

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\\[universal-argument] 3 \\[avy-kil-whole-line] kill three lines
starting from the selected line.  \\[universal-argument] -3

\\[avy-kill-whole-line] kill three lines backward including the
selected line.

(fn ARG)" t nil) (autoload 'avy-kill-ring-save-whole-line "avy" "Select line and save the whole selected line as if killed, but don\342\200\231t kill it.

This command is similar to `avy-kill-whole-line', except that it
saves the line(s) as if killed, but does not kill it(them).

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

(fn ARG)" t nil) (autoload 'avy-setup-default "avy" "Setup the default shortcuts." nil nil) (autoload 'avy-goto-char-timer "avy" "Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn &optional ARG)" t nil) (autoload 'avy-transpose-lines-in-region "avy" "Transpose lines in the active region." t nil) (register-definition-prefixes "avy" '("avy-")) (provide 'avy-autoloads)) "helm-descbinds" ((helm-descbinds helm-descbinds-autoloads) (defvar helm-descbinds-mode nil "Non-nil if Helm-Descbinds mode is enabled.
See the `helm-descbinds-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-descbinds-mode'.") (custom-autoload 'helm-descbinds-mode "helm-descbinds" nil) (autoload 'helm-descbinds-mode "helm-descbinds" "Use `helm' for `describe-bindings'.

If called interactively, enable Helm-Descbinds mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'helm-descbinds-install "helm-descbinds" "Use `helm-descbinds' as a replacement of `describe-bindings'." t nil) (autoload 'helm-descbinds-uninstall "helm-descbinds" "Restore original `describe-bindings'." t nil) (autoload 'helm-descbinds "helm-descbinds" "A convenient helm version of `describe-bindings'.

Turning on `helm-descbinds-mode' is the recommended way to
install this command to replace `describe-bindings'.

You complete against a list of keys + command pairs presented in
a similar way as `describe-bindings' does, split into sections
defined by the types of the key bindings (minor and major modes,
global bindings, etc).

The default action executes a command as if the binding had been
entered, or narrows the commands according to a prefix key,
respectively.

The persistent action pops up a help buffer for the selected
command without quitting.

For key translation maps, the default actions are not very
useful, yet they are listed for completeness.

(fn &optional PREFIX BUFFER)" t nil) (register-definition-prefixes "helm-descbinds" '("helm-descbind")) (provide 'helm-descbinds-autoloads)) "helm-tramp" ((helm-tramp-autoloads helm-tramp) (autoload 'helm-tramp "helm-tramp" "Open your ~/.ssh/config with helm interface.
You can connect your server with tramp" t nil) (register-definition-prefixes "helm-tramp" '("helm-tramp-")) (provide 'helm-tramp-autoloads)) "helm-ag" ((helm-ag helm-ag-autoloads) (autoload 'helm-ag-pop-stack "helm-ag" "Not documented." t nil) (autoload 'helm-ag-clear-stack "helm-ag" "Not documented." t nil) (autoload 'helm-ag-this-file "helm-ag" "Do ag with in this file with QUERY.

(fn &optional QUERY)" t nil) (autoload 'helm-ag "helm-ag" "Do ag with in BASEDIR and with QUERY.

(fn &optional BASEDIR QUERY)" t nil) (autoload 'helm-do-ag-this-file "helm-ag" "Not documented, QUERY.

(fn &optional QUERY)" t nil) (autoload 'helm-do-ag "helm-ag" "Not documented, BASEDIR, TARGETS, DEFAULT-INPUT.

(fn &optional BASEDIR TARGETS DEFAULT-INPUT)" t nil) (autoload 'helm-ag-project-root "helm-ag" "Not documented, QUERY.

(fn &optional QUERY)" t nil) (autoload 'helm-do-ag-project-root "helm-ag" "Not documented, QUERY.

(fn &optional QUERY)" t nil) (autoload 'helm-ag-buffers "helm-ag" "Not documented, QUERY.

(fn &optional QUERY)" t nil) (autoload 'helm-do-ag-buffers "helm-ag" "Not documented, QUERY.

(fn &optional QUERY)" t nil) (register-definition-prefixes "helm-ag" '("helm-")) (provide 'helm-ag-autoloads)) "docker-tramp" ((docker-tramp-autoloads docker-tramp docker-tramp-compat) (defvar docker-tramp-docker-options nil "List of docker options.") (custom-autoload 'docker-tramp-docker-options "docker-tramp" t) (defconst docker-tramp-completion-function-alist '((docker-tramp--parse-running-containers "")) "Default list of (FUNCTION FILE) pairs to be examined for docker method.") (defconst docker-tramp-method "docker" "Method to connect docker containers.") (autoload 'docker-tramp-cleanup "docker-tramp" "Cleanup TRAMP cache for docker method." t nil) (autoload 'docker-tramp-add-method "docker-tramp" "Add docker tramp method." nil nil) (eval-after-load 'tramp '(progn (docker-tramp-add-method) (tramp-set-completion-function docker-tramp-method docker-tramp-completion-function-alist))) (register-definition-prefixes "docker-tramp" '("docker-tramp-")) (provide 'docker-tramp-autoloads)) "json-snatcher" ((json-snatcher-autoloads json-snatcher) (autoload 'jsons-print-path "json-snatcher" "Print the path to the JSON value under point, and save it in the kill ring." t nil) (register-definition-prefixes "json-snatcher" '("jsons-")) (provide 'json-snatcher-autoloads)) "json-mode" ((json-mode-autoloads json-mode) (defconst json-mode-standard-file-ext '(".json" ".jsonld") "List of JSON file extensions.") (defsubst json-mode--update-auto-mode (filenames) "Update the `json-mode' entry of `auto-mode-alist'.

FILENAMES should be a list of file as string.
Return the new `auto-mode-alist' entry" (let* ((new-regexp (rx-to-string `(seq (eval (cons 'or (append json-mode-standard-file-ext ',filenames))) eot))) (new-entry (cons new-regexp 'json-mode)) (old-entry (when (boundp 'json-mode--auto-mode-entry) json-mode--auto-mode-entry))) (setq auto-mode-alist (delete old-entry auto-mode-alist)) (add-to-list 'auto-mode-alist new-entry) new-entry)) (defvar json-mode-auto-mode-list '(".babelrc" ".bowerrc" "composer.lock") "List of filenames for the JSON entry of `auto-mode-alist'.

Note however that custom `json-mode' entries in `auto-mode-alist'
won\342\200\231t be affected.") (custom-autoload 'json-mode-auto-mode-list "json-mode" nil) (defvar json-mode--auto-mode-entry (json-mode--update-auto-mode json-mode-auto-mode-list) "Regexp generated from the `json-mode-auto-mode-list'.") (autoload 'json-mode "json-mode" "Major mode for editing JSON files

(fn)" t nil) (autoload 'jsonc-mode "json-mode" "Major mode for editing JSON files with comments

(fn)" t nil) (add-to-list 'magic-fallback-mode-alist '("^[{[]$" . json-mode)) (autoload 'json-mode-show-path "json-mode" "Print the path to the node at point to the minibuffer." t nil) (autoload 'json-mode-kill-path "json-mode" "Save JSON path to object at point to kill ring." t nil) (autoload 'json-mode-beautify "json-mode" "Beautify / pretty-print the active region (or the entire buffer if no active region).

(fn BEGIN END)" t nil) (register-definition-prefixes "json-mode" '("json")) (provide 'json-mode-autoloads)) "docker" ((docker-utils docker-network docker-core docker-faces docker docker-container docker-compose docker-autoloads docker-image docker-volume) (autoload 'docker "docker" nil t) (register-definition-prefixes "docker" '("docker-read-")) (autoload 'docker-compose "docker-compose" nil t) (register-definition-prefixes "docker-compose" '("docker-compose-")) (autoload 'docker-container-eshell "docker-container" "Open `eshell' in CONTAINER.

(fn CONTAINER)" t nil) (autoload 'docker-container-find-directory "docker-container" "Inside CONTAINER open DIRECTORY.

(fn CONTAINER DIRECTORY)" t nil) (autoload 'docker-container-find-file "docker-container" "Open FILE inside CONTAINER.

(fn CONTAINER FILE)" t nil) (autoload 'docker-container-shell "docker-container" "Open `shell' in CONTAINER.  When READ-SHELL is not nil, ask the user for it.

(fn CONTAINER &optional READ-SHELL)" t nil) (autoload 'docker-container-shell-env "docker-container" "Open `shell' in CONTAINER with the environment variable set
and default directory set to workdir. When READ-SHELL is not
nil, ask the user for it.

(fn CONTAINER &optional READ-SHELL)" t nil) (autoload 'docker-containers "docker-container" "List docker containers." t nil) (register-definition-prefixes "docker-container" '("docker-container-")) (register-definition-prefixes "docker-core" '("docker-")) (autoload 'docker-image-pull-one "docker-image" "Pull the image named NAME.  If ALL is set, use \"-a\".

(fn NAME &optional ALL)" t nil) (autoload 'docker-images "docker-image" "List docker images." t nil) (register-definition-prefixes "docker-image" '("docker-")) (autoload 'docker-networks "docker-network" "List docker networks." t nil) (register-definition-prefixes "docker-network" '("docker-network-")) (register-definition-prefixes "docker-utils" '("docker-utils-")) (autoload 'docker-volume-dired "docker-volume" "Enter `dired' in the volume named NAME.

(fn NAME)" t nil) (autoload 'docker-volumes "docker-volume" "List docker volumes." t nil) (register-definition-prefixes "docker-volume" '("docker-volume-")) (provide 'docker-autoloads)) "lv" ((lv-autoloads lv) (register-definition-prefixes "lv" '("lv-")) (provide 'lv-autoloads)) "hydra" ((hydra-ox hydra-autoloads hydra hydra-examples) (autoload 'defhydra "hydra" "Create a Hydra - a family of functions with prefix NAME.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY has the format:

    (BODY-MAP BODY-KEY &rest BODY-PLIST)

DOCSTRING will be displayed in the echo area to identify the
Hydra.  When DOCSTRING starts with a newline, special Ruby-style
substitution will be performed by `hydra--format'.

Functions are created on basis of HEADS, each of which has the
format:

    (KEY CMD &optional HINT &rest PLIST)

BODY-MAP is a keymap; `global-map' is used quite often.  Each
function generated from HEADS will be bound in BODY-MAP to
BODY-KEY + KEY (both are strings passed to `kbd'), and will set
the transient map so that all following heads can be called
though KEY only.  BODY-KEY can be an empty string.

CMD is a callable expression: either an interactive function
name, or an interactive lambda, or a single sexp (it will be
wrapped in an interactive lambda).

HINT is a short string that identifies its head.  It will be
printed beside KEY in the echo erea if `hydra-is-helpful' is not
nil.  If you don't even want the KEY to be printed, set HINT
explicitly to nil.

The heads inherit their PLIST from BODY-PLIST and are allowed to
override some keys.  The keys recognized are :exit, :bind, and :column.
:exit can be:

- nil (default): this head will continue the Hydra state.
- t: this head will stop the Hydra state.

:bind can be:
- nil: this head will not be bound in BODY-MAP.
- a lambda taking KEY and CMD used to bind a head.

:column is a string that sets the column for all subsequent heads.

It is possible to omit both BODY-MAP and BODY-KEY if you don't
want to bind anything.  In that case, typically you will bind the
generated NAME/body command.  This command is also the return
result of `defhydra'.

(fn NAME BODY &optional DOCSTRING &rest HEADS)" nil t) (function-put 'defhydra 'lisp-indent-function 'defun) (function-put 'defhydra 'doc-string-elt '3) (register-definition-prefixes "hydra" '("defhydra" "hydra-")) (register-definition-prefixes "hydra-examples" '("hydra-" "org-agenda-cts" "whitespace-mode")) (register-definition-prefixes "hydra-ox" '("hydra-ox")) (provide 'hydra-autoloads)) "yapfify" ((yapfify yapfify-autoloads) (autoload 'yapfify-region "yapfify" "Try to yapfify the current region.

If yapf exits with an error, the output will be shown in a help-window.

(fn BEGINNING END)" t nil) (autoload 'yapfify-buffer "yapfify" "Yapfify whole buffer." t nil) (autoload 'yapfify-region-or-buffer "yapfify" "Yapfify the region if it is active. Otherwise, yapfify the buffer" t nil) (autoload 'yapf-mode "yapfify" "Automatically run YAPF before saving.

If called interactively, enable Yapf mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "yapfify" '("get-buffer-string" "yapfify-")) (provide 'yapfify-autoloads)) "py-isort" ((py-isort py-isort-autoloads) (autoload 'py-isort-region "py-isort" "Uses the \"isort\" tool to reformat the current region." t nil) (autoload 'py-isort-buffer "py-isort" "Uses the \"isort\" tool to reformat the current buffer." t nil) (autoload 'py-isort-before-save "py-isort" nil t nil) (register-definition-prefixes "py-isort" '("py-isort-")) (provide 'py-isort-autoloads)) "ace-window" ((ace-window ace-window-autoloads) (autoload 'ace-select-window "ace-window" "Ace select window." t nil) (autoload 'ace-delete-window "ace-window" "Ace delete window." t nil) (autoload 'ace-swap-window "ace-window" "Ace swap window." t nil) (autoload 'ace-delete-other-windows "ace-window" "Ace delete other windows." t nil) (autoload 'ace-display-buffer "ace-window" "Make `display-buffer' and `pop-to-buffer' select using `ace-window'.
See sample config for `display-buffer-base-action' and `display-buffer-alist':
https://github.com/abo-abo/ace-window/wiki/display-buffer.

(fn BUFFER ALIST)" nil nil) (autoload 'ace-window "ace-window" "Select a window.
Perform an action based on ARG described below.

By default, behaves like extended `other-window'.
See `aw-scope' which extends it to work with frames.

Prefixed with one \\[universal-argument], does a swap between the
selected window and the current window, so that the selected
buffer moves to current window (and current buffer moves to
selected window).

Prefixed with two \\[universal-argument]'s, deletes the selected
window.

(fn ARG)" t nil) (defvar ace-window-display-mode nil "Non-nil if Ace-Window-Display mode is enabled.
See the `ace-window-display-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ace-window-display-mode'.") (custom-autoload 'ace-window-display-mode "ace-window" nil) (autoload 'ace-window-display-mode "ace-window" "Minor mode for showing the ace window key in the mode line.

If called interactively, enable Ace-Window-Display mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "ace-window" '("ace-window-mode" "aw-")) (provide 'ace-window-autoloads)) "which-key" ((which-key which-key-autoloads) (defvar which-key-mode nil "Non-nil if Which-Key mode is enabled.
See the `which-key-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `which-key-mode'.") (custom-autoload 'which-key-mode "which-key" nil) (autoload 'which-key-mode "which-key" "Toggle which-key-mode.

If called interactively, enable Which-Key mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'which-key-setup-side-window-right "which-key" "Apply suggested settings for side-window that opens on right." t nil) (autoload 'which-key-setup-side-window-right-bottom "which-key" "Apply suggested settings for side-window that opens on right
if there is space and the bottom otherwise." t nil) (autoload 'which-key-setup-side-window-bottom "which-key" "Apply suggested settings for side-window that opens on bottom." t nil) (autoload 'which-key-setup-minibuffer "which-key" "Apply suggested settings for minibuffer.
Do not use this setup if you use the paging commands. Instead use
`which-key-setup-side-window-bottom', which is nearly identical
but more functional." t nil) (autoload 'which-key-add-keymap-based-replacements "which-key" "Replace the description of KEY using REPLACEMENT in KEYMAP.
KEY should take a format suitable for use in `kbd'. REPLACEMENT
should be a cons cell of the form (STRING . COMMAND) for each
REPLACEMENT, where STRING is the replacement string and COMMAND
is a symbol corresponding to the intended command to be
replaced. COMMAND can be nil if the binding corresponds to a key
prefix. An example is

(which-key-add-keymap-based-replacements global-map
  \"C-x w\" '(\"Save as\" . write-file)).

For backwards compatibility, REPLACEMENT can also be a string,
but the above format is preferred, and the option to use a string
for REPLACEMENT will eventually be removed.

(fn KEYMAP KEY REPLACEMENT &rest MORE)" nil nil) (autoload 'which-key-add-key-based-replacements "which-key" "Replace the description of KEY-SEQUENCE with REPLACEMENT.
KEY-SEQUENCE is a string suitable for use in `kbd'. REPLACEMENT
may either be a string, as in

(which-key-add-key-based-replacements \"C-x 1\" \"maximize\")

a cons of two strings as in

(which-key-add-key-based-replacements \"C-x 8\"
                                        '(\"unicode\" . \"Unicode keys\"))

or a function that takes a (KEY . BINDING) cons and returns a
replacement.

In the second case, the second string is used to provide a longer
name for the keys under a prefix.

MORE allows you to specifcy additional KEY REPLACEMENT pairs.  All
replacements are added to `which-key-replacement-alist'.

(fn KEY-SEQUENCE REPLACEMENT &rest MORE)" nil nil) (autoload 'which-key-add-major-mode-key-based-replacements "which-key" "Functions like `which-key-add-key-based-replacements'.
The difference is that MODE specifies the `major-mode' that must
be active for KEY-SEQUENCE and REPLACEMENT (MORE contains
addition KEY-SEQUENCE REPLACEMENT pairs) to apply.

(fn MODE KEY-SEQUENCE REPLACEMENT &rest MORE)" nil nil) (autoload 'which-key-reload-key-sequence "which-key" "Simulate entering the key sequence KEY-SEQ.
KEY-SEQ should be a list of events as produced by
`listify-key-sequence'. If nil, KEY-SEQ defaults to
`which-key--current-key-list'. Any prefix arguments that were
used are reapplied to the new key sequence.

(fn &optional KEY-SEQ)" nil nil) (autoload 'which-key-show-standard-help "which-key" "Call the command in `which-key--prefix-help-cmd-backup'.
Usually this is `describe-prefix-bindings'.

(fn &optional _)" t nil) (autoload 'which-key-show-next-page-no-cycle "which-key" "Show next page of keys unless on the last page, in which case
call `which-key-show-standard-help'." t nil) (autoload 'which-key-show-previous-page-no-cycle "which-key" "Show previous page of keys unless on the first page, in which
case do nothing." t nil) (autoload 'which-key-show-next-page-cycle "which-key" "Show the next page of keys, cycling from end to beginning
after last page.

(fn &optional _)" t nil) (autoload 'which-key-show-previous-page-cycle "which-key" "Show the previous page of keys, cycling from beginning to end
after first page.

(fn &optional _)" t nil) (autoload 'which-key-show-top-level "which-key" "Show top-level bindings.

(fn &optional _)" t nil) (autoload 'which-key-show-major-mode "which-key" "Show top-level bindings in the map of the current major mode.

This function will also detect evil bindings made using
`evil-define-key' in this map. These bindings will depend on the
current evil state. 

(fn &optional ALL)" t nil) (autoload 'which-key-show-full-major-mode "which-key" "Show all bindings in the map of the current major mode.

This function will also detect evil bindings made using
`evil-define-key' in this map. These bindings will depend on the
current evil state. " t nil) (autoload 'which-key-dump-bindings "which-key" "Dump bindings from PREFIX into buffer named BUFFER-NAME.

PREFIX should be a string suitable for `kbd'.

(fn PREFIX BUFFER-NAME)" t nil) (autoload 'which-key-undo-key "which-key" "Undo last keypress and force which-key update.

(fn &optional _)" t nil) (autoload 'which-key-C-h-dispatch "which-key" "Dispatch C-h commands by looking up key in
`which-key-C-h-map'. This command is always accessible (from any
prefix) if `which-key-use-C-h-commands' is non nil." t nil) (autoload 'which-key-show-keymap "which-key" "Show the top-level bindings in KEYMAP using which-key.
KEYMAP is selected interactively from all available keymaps.

If NO-PAGING is non-nil, which-key will not intercept subsequent
keypresses for the paging functionality.

(fn KEYMAP &optional NO-PAGING)" t nil) (autoload 'which-key-show-full-keymap "which-key" "Show all bindings in KEYMAP using which-key.
KEYMAP is selected interactively from all available keymaps.

(fn KEYMAP)" t nil) (autoload 'which-key-show-minor-mode-keymap "which-key" "Show the top-level bindings in KEYMAP using which-key.
KEYMAP is selected interactively by mode in
`minor-mode-map-alist'.

(fn &optional ALL)" t nil) (autoload 'which-key-show-full-minor-mode-keymap "which-key" "Show all bindings in KEYMAP using which-key.
KEYMAP is selected interactively by mode in
`minor-mode-map-alist'." t nil) (register-definition-prefixes "which-key" '("evil-state" "which-key-")) (provide 'which-key-autoloads)) "package-lint" ((package-lint-autoloads package-lint) (autoload 'package-lint-describe-symbol-history "package-lint" "Show the version history of SYM, if any.

(fn SYM)" t nil) (autoload 'package-lint-buffer "package-lint" "Get linter errors and warnings for BUFFER.

Returns a list, each element of which is list of

   (LINE COL TYPE MESSAGE)

where TYPE is either 'warning or 'error.

Current buffer is used if none is specified.

(fn &optional BUFFER)" nil nil) (autoload 'package-lint-current-buffer "package-lint" "Display lint errors and warnings for the current buffer." t nil) (autoload 'package-lint-batch-and-exit "package-lint" "Run `package-lint-buffer' on the files remaining on the command line.
Use this only with -batch, it won't work interactively.

When done, exit Emacs with status 1 in case of any errors, otherwise exit
with status 0.  The variable `package-lint-batch-fail-on-warnings' controls
whether or not warnings alone produce a non-zero exit code." nil nil) (autoload 'package-lint-looks-like-a-package-p "package-lint" "Return non-nil if the current buffer appears to be intended as a package." nil nil) (register-definition-prefixes "package-lint" '("package-lint-")) (provide 'package-lint-autoloads)) "f" ((f-autoloads f) (register-definition-prefixes "f" '("f-")) (provide 'f-autoloads)) "ht" ((ht-autoloads ht) (register-definition-prefixes "ht" 'nil) (provide 'ht-autoloads)) "spinner" ((spinner spinner-autoloads) (autoload 'spinner-create "spinner" "Create a spinner of the given TYPE.
The possible TYPEs are described in `spinner--type-to-frames'.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

If BUFFER-LOCAL is non-nil, the spinner will be automatically
deactivated if the buffer is killed.  If BUFFER-LOCAL is a
buffer, use that instead of current buffer.

When started, in order to function properly, the spinner runs a
timer which periodically calls `force-mode-line-update' in the
current buffer.  If BUFFER-LOCAL was set at creation time, then
`force-mode-line-update' is called in that buffer instead.  When
the spinner is stopped, the timer is deactivated.

DELAY, if given, is the number of seconds to wait after starting
the spinner before actually displaying it. It is safe to cancel
the spinner before this time, in which case it won't display at
all.

(fn &optional TYPE BUFFER-LOCAL FPS DELAY)" nil nil) (autoload 'spinner-start "spinner" "Start a mode-line spinner of given TYPE-OR-OBJECT.
If TYPE-OR-OBJECT is an object created with `make-spinner',
simply activate it.  This method is designed for minor modes, so
they can use the spinner as part of their lighter by doing:
    '(:eval (spinner-print THE-SPINNER))
To stop this spinner, call `spinner-stop' on it.

If TYPE-OR-OBJECT is anything else, a buffer-local spinner is
created with this type, and it is displayed in the
`mode-line-process' of the buffer it was created it.  Both
TYPE-OR-OBJECT and FPS are passed to `make-spinner' (which see).
To stop this spinner, call `spinner-stop' in the same buffer.

Either way, the return value is a function which can be called
anywhere to stop this spinner.  You can also call `spinner-stop'
in the same buffer where the spinner was created.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

DELAY, if given, is the number of seconds to wait until actually
displaying the spinner. It is safe to cancel the spinner before
this time, in which case it won't display at all.

(fn &optional TYPE-OR-OBJECT FPS DELAY)" nil nil) (register-definition-prefixes "spinner" '("spinner-")) (provide 'spinner-autoloads)) "lsp-mode" ((lsp-bash lsp-groovy lsp-go lsp-iedit lsp-vetur lsp-dockerfile lsp-yaml lsp-mode lsp-protocol lsp-diagnostics lsp-v lsp-vhdl lsp-nix lsp-svelte lsp-semantic-tokens lsp-d lsp-sorbet lsp-tex lsp-crystal lsp-zig lsp-dhall lsp-angular lsp-mode-autoloads lsp-json lsp-solargraph lsp-hack lsp-icons lsp-nim lsp-vala lsp-pylsp lsp-cmake lsp-toml lsp-racket lsp-pwsh lsp-graphql lsp-fortran lsp-rust lsp-modeline lsp-xml lsp-steep lsp-php lsp-rf lsp-r lsp-ocaml lsp-beancount lsp lsp-haxe lsp-markdown lsp-dired lsp-javascript lsp-ido lsp-html lsp-verilog lsp-css lsp-kotlin lsp-fsharp lsp-terraform lsp-csharp lsp-eslint lsp-ada lsp-gdscript lsp-elm lsp-sqls lsp-prolog lsp-clangd lsp-actionscript lsp-erlang lsp-completion lsp-lens lsp-purescript lsp-perl lsp-vimscript lsp-clojure lsp-lua lsp-elixir lsp-pyls lsp-headerline) (register-definition-prefixes "lsp-actionscript" '("lsp-actionscript-")) (register-definition-prefixes "lsp-ada" '("lsp-ada-")) (register-definition-prefixes "lsp-angular" '("lsp-client")) (register-definition-prefixes "lsp-bash" '("lsp-bash-")) (register-definition-prefixes "lsp-beancount" '("lsp-beancount-")) (autoload 'lsp-cpp-flycheck-clang-tidy-error-explainer "lsp-clangd" "Explain a clang-tidy ERROR by scraping documentation from llvm.org.

(fn ERROR)" nil nil) (register-definition-prefixes "lsp-clangd" '("lsp-c")) (register-definition-prefixes "lsp-clojure" '("lsp-clojure-")) (define-obsolete-variable-alias 'lsp-prefer-capf 'lsp-completion-provider "lsp-mode 7.0.1") (define-obsolete-variable-alias 'lsp-enable-completion-at-point 'lsp-completion-enable "lsp-mode 7.0.1") (autoload 'lsp-completion-at-point "lsp-completion" "Get lsp completions." nil nil) (autoload 'lsp-completion--enable "lsp-completion" "Enable LSP completion support." nil nil) (autoload 'lsp-completion-mode "lsp-completion" "Toggle LSP completion support.

This is a minor mode.  If called interactively, toggle the
`Lsp-Completion mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `lsp-completion-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (add-hook 'lsp-configure-hook (lambda nil (when (and lsp-auto-configure lsp-completion-enable) (lsp-completion--enable)))) (register-definition-prefixes "lsp-completion" '("lsp-")) (register-definition-prefixes "lsp-crystal" '("lsp-clients-crystal-executable")) (register-definition-prefixes "lsp-csharp" '("lsp-csharp-")) (register-definition-prefixes "lsp-css" '("lsp-css-")) (define-obsolete-variable-alias 'lsp-diagnostic-package 'lsp-diagnostics-provider "lsp-mode 7.0.1") (define-obsolete-variable-alias 'lsp-flycheck-default-level 'lsp-diagnostics-flycheck-default-level "lsp-mode 7.0.1") (autoload 'lsp-diagnostics-lsp-checker-if-needed "lsp-diagnostics" nil nil nil) (autoload 'lsp-diagnostics--enable "lsp-diagnostics" "Enable LSP checker support." nil nil) (autoload 'lsp-diagnostics-mode "lsp-diagnostics" "Toggle LSP diagnostics integration.

This is a minor mode.  If called interactively, toggle the
`Lsp-Diagnostics mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `lsp-diagnostics-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (add-hook 'lsp-configure-hook (lambda nil (when lsp-auto-configure (lsp-diagnostics--enable)))) (register-definition-prefixes "lsp-diagnostics" '("lsp-diagnostics-")) (defvar lsp-dired-mode nil "Non-nil if Lsp-Dired mode is enabled.
See the `lsp-dired-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `lsp-dired-mode'.") (custom-autoload 'lsp-dired-mode "lsp-dired" nil) (autoload 'lsp-dired-mode "lsp-dired" "Display `lsp-mode' icons for each file in a dired buffer.

This is a minor mode.  If called interactively, toggle the
`Lsp-Dired mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'lsp-dired-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "lsp-dired" '("lsp-dired-")) (register-definition-prefixes "lsp-dockerfile" '("lsp-dockerfile-language-server-command")) (register-definition-prefixes "lsp-elixir" '("lsp-elixir-")) (register-definition-prefixes "lsp-elm" '("lsp-")) (register-definition-prefixes "lsp-erlang" '("lsp-erlang-server-")) (register-definition-prefixes "lsp-eslint" '("lsp-")) (register-definition-prefixes "lsp-fortran" '("lsp-clients-")) (autoload 'lsp-fsharp--workspace-load "lsp-fsharp" "Load all of the provided PROJECTS.

(fn PROJECTS)" nil nil) (register-definition-prefixes "lsp-fsharp" '("lsp-fsharp-")) (register-definition-prefixes "lsp-gdscript" '("lsp-gdscript-")) (register-definition-prefixes "lsp-go" '("lsp-go-")) (register-definition-prefixes "lsp-graphql" '("lsp-")) (register-definition-prefixes "lsp-groovy" '("lsp-groovy-")) (register-definition-prefixes "lsp-hack" '("lsp-clients-hack-command")) (register-definition-prefixes "lsp-haxe" '("lsp-")) (autoload 'lsp-headerline-breadcrumb-mode "lsp-headerline" "Toggle breadcrumb on headerline.

This is a minor mode.  If called interactively, toggle the
`Lsp-Headerline-Breadcrumb mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `lsp-headerline-breadcrumb-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'lsp-breadcrumb-go-to-symbol "lsp-headerline" "Go to the symbol on breadcrumb at SYMBOL-POSITION.

(fn SYMBOL-POSITION)" t nil) (autoload 'lsp-breadcrumb-narrow-to-symbol "lsp-headerline" "Narrow to the symbol range on breadcrumb at SYMBOL-POSITION.

(fn SYMBOL-POSITION)" t nil) (register-definition-prefixes "lsp-headerline" '("lsp-headerline-")) (register-definition-prefixes "lsp-html" '("lsp-html-")) (register-definition-prefixes "lsp-icons" '("lsp-")) (autoload 'lsp-ido-workspace-symbol "lsp-ido" "`ido' for lsp workspace/symbol.
When called with prefix ARG the default selection will be symbol at point.

(fn ARG)" t nil) (register-definition-prefixes "lsp-ido" '("lsp-ido-")) (autoload 'lsp-iedit-highlights "lsp-iedit" "Start an `iedit' operation on the documentHighlights at point.
This can be used as a primitive `lsp-rename' replacement if the
language server doesn't support renaming.

See also `lsp-enable-symbol-highlighting'." t nil) (autoload 'lsp-iedit-linked-ranges "lsp-iedit" "Start an `iedit' for `textDocument/linkedEditingRange'" t nil) (autoload 'lsp-evil-multiedit-highlights "lsp-iedit" "Start an `evil-multiedit' operation on the documentHighlights at point.
This can be used as a primitive `lsp-rename' replacement if the
language server doesn't support renaming.

See also `lsp-enable-symbol-highlighting'." t nil) (autoload 'lsp-evil-multiedit-linked-ranges "lsp-iedit" "Start an `evil-multiedit' for `textDocument/linkedEditingRange'" t nil) (autoload 'lsp-evil-state-highlights "lsp-iedit" "Start `iedit-mode'. for `textDocument/documentHighlight'" t nil) (autoload 'lsp-evil-state-linked-ranges "lsp-iedit" "Start `iedit-mode'. for `textDocument/linkedEditingRange'" t nil) (register-definition-prefixes "lsp-iedit" '("lsp-iedit--on-ranges")) (register-definition-prefixes "lsp-javascript" '("lsp-")) (register-definition-prefixes "lsp-json" '("lsp-")) (register-definition-prefixes "lsp-kotlin" '("lsp-")) (autoload 'lsp-lens--enable "lsp-lens" "Enable lens mode." nil nil) (autoload 'lsp-lens-show "lsp-lens" "Display lenses in the buffer." t nil) (autoload 'lsp-lens-hide "lsp-lens" "Delete all lenses." t nil) (autoload 'lsp-lens-mode "lsp-lens" "Toggle code-lens overlays.

This is a minor mode.  If called interactively, toggle the
`Lsp-Lens mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `lsp-lens-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'lsp-avy-lens "lsp-lens" "Click lsp lens using `avy' package." t nil) (register-definition-prefixes "lsp-lens" '("lsp-")) (register-definition-prefixes "lsp-lua" '("lsp-")) (register-definition-prefixes "lsp-markdown" '("lsp-markdown-")) (put 'lsp-enable-file-watchers 'safe-local-variable #'booleanp) (put 'lsp-file-watch-threshold 'safe-local-variable (lambda (i) (or (numberp i) (not i)))) (autoload 'lsp-load-vscode-workspace "lsp-mode" "Load vscode workspace from FILE

(fn FILE)" t nil) (autoload 'lsp-save-vscode-workspace "lsp-mode" "Save vscode workspace to FILE

(fn FILE)" t nil) (autoload 'lsp-install-server "lsp-mode" "Interactively install or re-install server.
When prefix UPDATE? is t force installation even if the server is present.

(fn UPDATE\\=\\? &optional SERVER-ID)" t nil) (autoload 'lsp-update-server "lsp-mode" "Interactively update a server.

(fn &optional SERVER-ID)" t nil) (autoload 'lsp-ensure-server "lsp-mode" "Ensure server SERVER-ID

(fn SERVER-ID)" nil nil) (autoload 'lsp "lsp-mode" "Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be opened in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start.

(fn &optional ARG)" t nil) (autoload 'lsp-deferred "lsp-mode" "Entry point that defers server startup until buffer is visible.
`lsp-deferred' will wait until the buffer is visible before invoking `lsp'.
This avoids overloading the server with many files when starting Emacs." nil nil) (register-definition-prefixes "lsp-mode" '("defcustom-lsp" "lsp-" "make-lsp-client" "when-lsp-workspace" "with-lsp-workspace")) (define-obsolete-variable-alias 'lsp-diagnostics-modeline-scope 'lsp-modeline-diagnostics-scope "lsp-mode 7.0.1") (autoload 'lsp-modeline-code-actions-mode "lsp-modeline" "Toggle code actions on modeline.

This is a minor mode.  If called interactively, toggle the
`Lsp-Modeline-Code-Actions mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `lsp-modeline-code-actions-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (define-obsolete-function-alias 'lsp-diagnostics-modeline-mode 'lsp-modeline-diagnostics-mode "lsp-mode 7.0.1") (autoload 'lsp-modeline-diagnostics-mode "lsp-modeline" "Toggle diagnostics modeline.

This is a minor mode.  If called interactively, toggle the
`Lsp-Modeline-Diagnostics mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `lsp-modeline-diagnostics-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'lsp-modeline-workspace-status-mode "lsp-modeline" "Toggle workspace status on modeline.

This is a minor mode.  If called interactively, toggle the
`Lsp-Modeline-Workspace-Status mode' mode.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `lsp-modeline-workspace-status-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "lsp-modeline" '("lsp-")) (register-definition-prefixes "lsp-nix" '("lsp-nix-server-path")) (register-definition-prefixes "lsp-ocaml" '("lsp-ocaml-l")) (register-definition-prefixes "lsp-perl" '("lsp-perl-")) (register-definition-prefixes "lsp-php" '("lsp-")) (register-definition-prefixes "lsp-prolog" '("lsp-prolog-server-command")) (register-definition-prefixes "lsp-protocol" '("dash-expand:&RangeToPoint" "lsp")) (register-definition-prefixes "lsp-purescript" '("lsp-purescript-")) (register-definition-prefixes "lsp-pwsh" '("lsp-pwsh-")) (register-definition-prefixes "lsp-pyls" '("lsp-")) (register-definition-prefixes "lsp-pylsp" '("lsp-")) (register-definition-prefixes "lsp-r" '("lsp-clients-r-server-command")) (register-definition-prefixes "lsp-racket" '("lsp-racket-lang")) (register-definition-prefixes "lsp-rf" '("expand-start-command" "lsp-rf-language-server-" "parse-rf-language-server-")) (register-definition-prefixes "lsp-rust" '("lsp-")) (autoload 'lsp--semantic-tokens-initialize-buffer "lsp-semantic-tokens" "Initialize the buffer for semantic tokens.
IS-RANGE-PROVIDER is non-nil when server supports range requests." nil nil) (autoload 'lsp--semantic-tokens-initialize-workspace "lsp-semantic-tokens" "Initialize semantic tokens for WORKSPACE.

(fn WORKSPACE)" nil nil) (autoload 'lsp-semantic-tokens--warn-about-deprecated-setting "lsp-semantic-tokens" "Warn about deprecated semantic highlighting variable." nil nil) (autoload 'lsp-semantic-tokens--enable "lsp-semantic-tokens" "Enable semantic tokens mode." nil nil) (autoload 'lsp-semantic-tokens-mode "lsp-semantic-tokens" "Toggle semantic-tokens support.

This is a minor mode.  If called interactively, toggle the
`Lsp-Semantic-Tokens mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `lsp-semantic-tokens-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "lsp-semantic-tokens" '("lsp-")) (register-definition-prefixes "lsp-solargraph" '("lsp-solargraph-")) (register-definition-prefixes "lsp-sorbet" '("lsp-sorbet-")) (register-definition-prefixes "lsp-sqls" '("lsp-sql")) (register-definition-prefixes "lsp-steep" '("lsp-steep-")) (register-definition-prefixes "lsp-svelte" '("lsp-svelte-plugin-")) (register-definition-prefixes "lsp-terraform" '("lsp-terraform-")) (register-definition-prefixes "lsp-tex" '("lsp-")) (register-definition-prefixes "lsp-toml" '("lsp-toml-")) (register-definition-prefixes "lsp-v" '("lsp-v-vls-executable")) (register-definition-prefixes "lsp-vala" '("lsp-clients-vala-ls-executable")) (register-definition-prefixes "lsp-verilog" '("lsp-clients-")) (register-definition-prefixes "lsp-vetur" '("lsp-")) (register-definition-prefixes "lsp-vhdl" '("ghdl-ls-bin-name" "hdl-checker-bin-name" "lsp-vhdl-" "vhdl-")) (register-definition-prefixes "lsp-vimscript" '("lsp-clients-vim-")) (register-definition-prefixes "lsp-xml" '("lsp-xml-")) (register-definition-prefixes "lsp-yaml" '("lsp-yaml-")) (register-definition-prefixes "lsp-zig" '("lsp-zig-zls-executable")) (provide 'lsp-mode-autoloads)) "helm-lsp" ((helm-lsp helm-lsp-autoloads) (autoload 'helm-lsp-workspace-symbol "helm-lsp" "`helm' for lsp workspace/symbol.
When called with prefix ARG the default selection will be symbol at point.

(fn ARG)" t nil) (autoload 'helm-lsp-global-workspace-symbol "helm-lsp" "`helm' for lsp workspace/symbol for all of the current workspaces.
When called with prefix ARG the default selection will be symbol at point.

(fn ARG)" t nil) (autoload 'helm-lsp-code-actions "helm-lsp" "Show lsp code actions using helm." t nil) (autoload 'helm-lsp-diagnostics "helm-lsp" "Diagnostics using `helm'

(fn ARG)" t nil) (register-definition-prefixes "helm-lsp" '("helm-lsp-")) (provide 'helm-lsp-autoloads)) "init-loader" ((init-loader-autoloads init-loader) (autoload 'init-loader-load "init-loader" "Load configuration files in INIT-DIR.

(fn &optional (INIT-DIR init-loader-directory))" nil nil) (autoload 'init-loader-show-log "init-loader" "Show init-loader log buffer." t nil) (register-definition-prefixes "init-loader" '("init-loader-")) (provide 'init-loader-autoloads)) "lsp-docker" ((lsp-docker lsp-docker-autoloads) (register-definition-prefixes "lsp-docker" '("lsp-docker-")) (provide 'lsp-docker-autoloads)) "lsp-docker+" ((lsp-docker+-autoloads lsp-docker+) (autoload 'lsp-docker+-enable "lsp-docker+" "Enable lsp-docker+.
This function add advice function `lsp-docker+-before-lsp' to `lsp'" t nil) (autoload 'lsp-docker+-disable "lsp-docker+" "Disable lsp-docker+.
This function remove advice function `lsp-docker+-before-lsp' from `lsp'" t nil) (register-definition-prefixes "lsp-docker+" '("lsp-docker+-")) (provide 'lsp-docker+-autoloads)) "lsp-ui" ((lsp-ui-imenu lsp-ui lsp-ui-doc lsp-ui-peek lsp-ui-autoloads lsp-ui-util lsp-ui-flycheck lsp-ui-sideline) (autoload 'lsp-ui-mode "lsp-ui" "Toggle language server UI mode on or off.
\342\200\230lsp-ui-mode\342\200\231 is a minor mode that contains a series of useful UI
integrations for \342\200\230lsp-mode\342\200\231.  With a prefix argument ARG, enable
language server UI mode if ARG is positive, and disable it
otherwise.  If called from Lisp, enable the mode if ARG is
omitted or nil, and toggle it if ARG is \342\200\230toggle\342\200\231.

(fn &optional ARG)" t nil) (register-definition-prefixes "lsp-ui" '("lsp-ui-")) (register-definition-prefixes "lsp-ui-doc" '("lsp-ui-doc-")) (register-definition-prefixes "lsp-ui-flycheck" '("lsp-ui-flycheck-")) (register-definition-prefixes "lsp-ui-imenu" '("lsp-ui-imenu")) (register-definition-prefixes "lsp-ui-peek" '("lsp-")) (register-definition-prefixes "lsp-ui-sideline" '("lsp-ui-sideline")) (register-definition-prefixes "lsp-ui-util" '("lsp-ui-util-")) (provide 'lsp-ui-autoloads)) "lsp-pyright" ((lsp-pyright lsp-pyright-autoloads) (register-definition-prefixes "lsp-pyright" '("lsp-pyright-")) (provide 'lsp-pyright-autoloads)) "ccls" ((ccls-semantic-highlight ccls-member-hierarchy ccls ccls-autoloads ccls-code-lens ccls-inheritance-hierarchy ccls-tree ccls-common ccls-call-hierarchy) (register-definition-prefixes "ccls" '("ccls-")) (register-definition-prefixes "ccls-call-hierarchy" '("ccls-call-hierarchy")) (register-definition-prefixes "ccls-code-lens" '("ccls-")) (register-definition-prefixes "ccls-inheritance-hierarchy" '("ccls-inheritance-hierarchy")) (register-definition-prefixes "ccls-member-hierarchy" '("ccls-member-hierarchy")) (register-definition-prefixes "ccls-semantic-highlight" '("ccls-")) (register-definition-prefixes "ccls-tree" '("ccls-tree-")) (provide 'ccls-autoloads)) "yasnippet" ((yasnippet yasnippet-autoloads) (autoload 'yas-minor-mode "yasnippet" "Toggle YASnippet mode.

If called interactively, enable Yas minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When YASnippet mode is enabled, `yas-expand', normally bound to
the TAB key, expands snippets of code depending on the major
mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

Key bindings:
\\{yas-minor-mode-map}

(fn &optional ARG)" t nil) (put 'yas-global-mode 'globalized-minor-mode t) (defvar yas-global-mode nil "Non-nil if Yas-Global mode is enabled.
See the `yas-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `yas-global-mode'.") (custom-autoload 'yas-global-mode "yasnippet" nil) (autoload 'yas-global-mode "yasnippet" "Toggle Yas minor mode in all buffers.
With prefix ARG, enable Yas-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Yas minor mode is enabled in all buffers where
`yas-minor-mode-on' would do it.
See `yas-minor-mode' for more information on Yas minor mode.

(fn &optional ARG)" t nil) (autoload 'snippet-mode "yasnippet" "A mode for editing yasnippets" t nil) (register-definition-prefixes "yasnippet" '("help-snippet-def" "snippet-mode-map" "yas")) (provide 'yasnippet-autoloads)) "company" ((company-tempo company-capf company-dabbrev-code company-clang company-elisp company-template company-files company-etags company-bbdb company company-dabbrev company-keywords company-css company-tng company-cmake company-autoloads company-nxml company-abbrev company-ispell company-semantic company-gtags company-oddmuse company-yasnippet) (autoload 'company-mode "company" "\"complete anything\"; is an in-buffer completion framework.
Completion starts automatically, depending on the values
`company-idle-delay' and `company-minimum-prefix-length'.

If called interactively, enable Company mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Completion can be controlled with the commands:
`company-complete-common', `company-complete-selection', `company-complete',
`company-select-next', `company-select-previous'.  If these commands are
called before `company-idle-delay', completion will also start.

Completions can be searched with `company-search-candidates' or
`company-filter-candidates'.  These can be used while completion is
inactive, as well.

The completion data is retrieved using `company-backends' and displayed
using `company-frontends'.  If you want to start a specific backend, call
it interactively or use `company-begin-backend'.

By default, the completions list is sorted alphabetically, unless the
backend chooses otherwise, or `company-transformers' changes it later.

regular keymap (`company-mode-map'):

\\{company-mode-map}
keymap during active completions (`company-active-map'):

\\{company-active-map}

(fn &optional ARG)" t nil) (put 'global-company-mode 'globalized-minor-mode t) (defvar global-company-mode nil "Non-nil if Global Company mode is enabled.
See the `global-company-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-company-mode'.") (custom-autoload 'global-company-mode "company" nil) (autoload 'global-company-mode "company" "Toggle Company mode in all buffers.
With prefix ARG, enable Global Company mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Company mode is enabled in all buffers where
`company-mode-on' would do it.
See `company-mode' for more information on Company mode.

(fn &optional ARG)" t nil) (autoload 'company-manual-begin "company" nil t nil) (autoload 'company-complete "company" "Insert the common part of all candidates or the current selection.
The first time this is called, the common part is inserted, the second
time, or when the selection has been changed, the selected candidate is
inserted." t nil) (register-definition-prefixes "company" '("company-")) (autoload 'company-abbrev "company-abbrev" "`company-mode' completion backend for abbrev.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (register-definition-prefixes "company-abbrev" '("company-abbrev-insert")) (autoload 'company-bbdb "company-bbdb" "`company-mode' completion backend for BBDB.

(fn COMMAND &optional ARG &rest IGNORE)" t nil) (register-definition-prefixes "company-bbdb" '("company-bbdb-")) (register-definition-prefixes "company-capf" '("company-")) (register-definition-prefixes "company-clang" '("company-clang")) (register-definition-prefixes "company-cmake" '("company-cmake")) (autoload 'company-css "company-css" "`company-mode' completion backend for `css-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (register-definition-prefixes "company-css" '("company-css-")) (autoload 'company-dabbrev "company-dabbrev" "dabbrev-like `company-mode' completion backend.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (register-definition-prefixes "company-dabbrev" '("company-dabbrev-")) (autoload 'company-dabbrev-code "company-dabbrev-code" "dabbrev-like `company-mode' backend for code.
The backend looks for all symbols in the current buffer that aren't in
comments or strings.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (register-definition-prefixes "company-dabbrev-code" '("company-dabbrev-code-")) (autoload 'company-elisp "company-elisp" "`company-mode' completion backend for Emacs Lisp.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (register-definition-prefixes "company-elisp" '("company-elisp-")) (autoload 'company-etags "company-etags" "`company-mode' completion backend for etags.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (register-definition-prefixes "company-etags" '("company-etags-")) (autoload 'company-files "company-files" "`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (register-definition-prefixes "company-files" '("company-file")) (autoload 'company-gtags "company-gtags" "`company-mode' completion backend for GNU Global.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (register-definition-prefixes "company-gtags" '("company-gtags-")) (autoload 'company-ispell "company-ispell" "`company-mode' completion backend using Ispell.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (register-definition-prefixes "company-ispell" '("company-ispell-")) (autoload 'company-keywords "company-keywords" "`company-mode' backend for programming language keywords.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (register-definition-prefixes "company-keywords" '("company-keywords-")) (autoload 'company-nxml "company-nxml" "`company-mode' completion backend for `nxml-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (register-definition-prefixes "company-nxml" '("company-nxml-")) (autoload 'company-oddmuse "company-oddmuse" "`company-mode' completion backend for `oddmuse-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (register-definition-prefixes "company-oddmuse" '("company-oddmuse-")) (autoload 'company-semantic "company-semantic" "`company-mode' completion backend using CEDET Semantic.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (register-definition-prefixes "company-semantic" '("company-semantic-")) (register-definition-prefixes "company-template" '("company-template-")) (autoload 'company-tempo "company-tempo" "`company-mode' completion backend for tempo.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (register-definition-prefixes "company-tempo" '("company-tempo-")) (autoload 'company-tng-frontend "company-tng" "When the user changes the selection at least once, this
frontend will display the candidate in the buffer as if it's
already there and any key outside of `company-active-map' will
confirm the selection and finish the completion.

(fn COMMAND)" nil nil) (define-obsolete-function-alias 'company-tng-configure-default 'company-tng-mode "0.9.14" "Applies the default configuration to enable company-tng.") (defvar company-tng-mode nil "Non-nil if Company-Tng mode is enabled.
See the `company-tng-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-tng-mode'.") (custom-autoload 'company-tng-mode "company-tng" nil) (autoload 'company-tng-mode "company-tng" "This minor mode enables `company-tng-frontend'.

If called interactively, enable Company-Tng mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "company-tng" '("company-tng-")) (autoload 'company-yasnippet "company-yasnippet" "`company-mode' backend for `yasnippet'.

This backend should be used with care, because as long as there are
snippets defined for the current major mode, this backend will always
shadow backends that come after it.  Recommended usages:

* In a buffer-local value of `company-backends', grouped with a backend or
  several that provide actual text completions.

  (add-hook \\='js-mode-hook
            (lambda ()
              (set (make-local-variable \\='company-backends)
                   \\='((company-dabbrev-code company-yasnippet)))))

* After keyword `:with', grouped with other backends.

  (push \\='(company-semantic :with company-yasnippet) company-backends)

* Not in `company-backends', just bound to a key.

  (global-set-key (kbd \"C-c y\") \\='company-yasnippet)

(fn COMMAND &optional ARG &rest IGNORE)" t nil) (register-definition-prefixes "company-yasnippet" '("company-yasnippet-")) (provide 'company-autoloads)) "names" ((names-autoloads names-dev names) (defvar names--inside-make-autoload nil "Used in `make-autoload' to indicate we're making autoloads.") (autoload 'define-namespace "names" "Inside the namespace NAME, execute BODY.
NAME can be any symbol (not quoted), but it's highly recommended
to use some form of separator (such as :, /, or -). For a
complete description of this macro, please visit the frontpage
with \\[names-view-manual].

In summary, this macro has two main effects:

1. Any definitions inside BODY will have NAME prepended to the
symbol given. Ex:

    (define-namespace foo-
    (defvar bar 1 \"docs\")
    )

expands to

    (defvar foo-bar 1 \"docs\")


2. Any function calls and variable names get NAME prepended to
them if such a variable or function exists. Ex:

    (define-namespace foo:
    (defun message (x y) nil)
    (message \"%s\" my-var)
    )

expands to

    (defun foo:message (x y) nil)
    (foo:message \"%s\" my-var)

Note how `message' is expanded to `foo:message' in the second
form, because that function exists. Meanwhile, `bar' is left
untouched because `foo:bar' is not a known variable name.

===============================

AUTOLOAD

In order for `define-namespace' to work with \";;;###autoload\"
comments must replace all instances of \";;;###autoload\" inside
your `define-namespace' with `:autoload'.
Afterwards, add an \";;;###autoload\" comment just above your
`define-namespace'.

===============================

KEYWORDS

Immediately after NAME you may add keywords which customize the
behaviour of `define-namespace'. For a list of possible keywords
and a description of their effects, see the variable
`names--keyword-list'.

(fn NAME [KEYWORD ...] BODY)" nil t) (function-put 'define-namespace 'lisp-indent-function '(lambda (&rest x) 0)) (eval-after-load 'find-func '(defadvice find-function-search-for-symbol (around names-around-find-function-search-for-symbol-advice (symbol type library) activate) "Make sure `find-function-search-for-symbol' understands namespaces." ad-do-it (ignore-errors (unless (cdr ad-return-value) (with-current-buffer (car ad-return-value) (search-forward-regexp "^(define-namespace\\_>") (skip-chars-forward "
[:blank:]") (let* ((names--regexp (concat "\\`" (regexp-quote (symbol-name (read (current-buffer)))))) (short-symbol (let ((name (symbol-name symbol))) (when (string-match names--regexp name) (intern (replace-match "" nil nil name)))))) (when short-symbol (ad-set-arg 0 short-symbol) ad-do-it))))))) (defadvice make-autoload (around names-before-make-autoload-advice (form file &optional expansion) activate) "Make sure `make-autoload' understands `define-namespace'.
Use the `names--inside-make-autoload' variable to indicate to
`define-namespace' that we're generating autoloads." (require 'names) (if (null (eq (car-safe form) 'define-namespace)) ad-do-it (setq names--inside-make-autoload t) (setq form (macroexpand form)) (setq names--inside-make-autoload nil) (if (version< emacs-version "24.3") (setq ad-return-value (cons 'progn (mapcar (lambda (x) (names--make-autoload-compat x file)) (cdr form)))) (ad-set-arg 2 'expansion) (ad-set-arg 0 form) ad-do-it))) (register-definition-prefixes "names" '("names-")) (register-definition-prefixes "names-dev" '("find-function-read" "names-")) (provide 'names-autoloads)) "unicode-escape" ((unicode-escape unicode-escape-autoloads) (autoload 'unicode-escape "unicode-escape" nil nil nil) (autoload 'unicode-escape* "unicode-escape" nil nil nil) (autoload 'unicode-unescape "unicode-escape" nil nil nil) (autoload 'unicode-unescape* "unicode-escape" nil nil nil) (autoload 'unicode-escape-region "unicode-escape" nil nil nil) (autoload 'unicode-unescape-region "unicode-escape" nil nil nil) (provide 'unicode-escape-autoloads)) "company-tabnine" ((company-tabnine company-tabnine-autoloads) (autoload 'company-tabnine "company-tabnine" "`company-mode' backend for TabNine.

See documentation of `company-backends' for details.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (register-definition-prefixes "company-tabnine" '("company-tabnine-")) (provide 'company-tabnine-autoloads)) "bui" ((bui-list bui-autoloads bui-utils bui-core bui bui-entry bui-history bui-button bui-info) (register-definition-prefixes "bui" '("bui-define-")) (register-definition-prefixes "bui-button" '("bui")) (register-definition-prefixes "bui-core" '("bui-")) (register-definition-prefixes "bui-entry" '("bui-")) (register-definition-prefixes "bui-history" '("bui-history")) (register-definition-prefixes "bui-info" '("bui-info-")) (register-definition-prefixes "bui-list" '("bui-list-")) (register-definition-prefixes "bui-utils" '("bui-")) (provide 'bui-autoloads)) "pfuture" ((pfuture-autoloads pfuture) (autoload 'pfuture-new "pfuture" "Create a new future process for command CMD.
Any arguments after the command are interpreted as arguments to the command.
This will return a process object with additional 'stderr and 'stdout
properties, which can be read via (process-get process 'stdout) and
(process-get process 'stderr) or alternatively with
(pfuture-result process) or (pfuture-stderr process).

Note that CMD must be a *sequence* of strings, meaning
this is wrong: (pfuture-new \"git status\")
this is right: (pfuture-new \"git\" \"status\")

(fn &rest CMD)" nil nil) (register-definition-prefixes "pfuture" '("pfuture-")) (provide 'pfuture-autoloads)) "posframe" ((posframe-benchmark posframe-autoloads posframe) (autoload 'posframe-workable-p "posframe" "Test posframe workable status." nil nil) (autoload 'posframe-show "posframe" "Pop up a posframe to show STRING at POSITION.

 (1) POSITION

POSITION can be:
1. An integer, meaning point position.
2. A cons of two integers, meaning absolute X and Y coordinates.
3. Other type, in which case the corresponding POSHANDLER should be
   provided.

 (2) POSHANDLER

POSHANDLER is a function of one argument returning an actual
position.  Its argument is a plist of the following form:

  (:position xxx
   :poshandler xxx
   :font-height xxx
   :font-width xxx
   :posframe xxx
   :posframe-width xxx
   :posframe-height xxx
   :posframe-buffer xxx
   :parent-frame xxx
   :parent-window-left xxx
   :parent-window-top xxx
   :parent-frame-width xxx
   :parent-frame-height xxx
   :parent-window xxx
   :parent-window-width  xxx
   :parent-window-height xxx
   :mouse-x xxx
   ;mouse-y xxx
   :minibuffer-height xxx
   :mode-line-height  xxx
   :header-line-height xxx
   :tab-line-height xxx
   :x-pixel-offset xxx
   :y-pixel-offset xxx)

By default, poshandler is auto-selected based on the type of POSITION,
but the selection can be overridden using the POSHANDLER argument.

The names of poshandler functions are like:

   `posframe-poshandler-p0.5p0-to-w0.5p1'

which mean align posframe(0.5, 0) to a position(a, b)

1. a = x of window(0.5, 0)
2. b = y of point(1, 1)

    posframe(p), frame(f), window(w), point(p), mouse(m)

         (0,0)      (0.5,0)      (1,0)
          +------------+-----------+
          |                        |
          |                        |
          |                        |
 (0, 0.5) +                        + (1, 0.5)
          |                        |
          |                        |
          |                        |
          +------------+-----------+
         (0,1)      (0.5,1)      (1,1)

The alias of builtin poshandler functions are listed below:

1.  `posframe-poshandler-frame-center'
2.  `posframe-poshandler-frame-top-center'
3.  `posframe-poshandler-frame-top-left-corner'
4.  `posframe-poshandler-frame-top-right-corner'
5.  `posframe-poshandler-frame-bottom-center'
6.  `posframe-poshandler-frame-bottom-left-corner'
7.  `posframe-poshandler-frame-bottom-right-corner'
8.  `posframe-poshandler-window-center'
9.  `posframe-poshandler-window-top-center'
10. `posframe-poshandler-window-top-left-corner'
11. `posframe-poshandler-window-top-right-corner'
12. `posframe-poshandler-window-bottom-center'
13. `posframe-poshandler-window-bottom-left-corner'
14. `posframe-poshandler-window-bottom-right-corner'
15. `posframe-poshandler-point-top-left-corner'
16. `posframe-poshandler-point-bottom-left-corner'
17. `posframe-poshandler-point-bottom-left-corner-upward'
18. `posframe-poshandler-point-window-center'

by the way, poshandler can be used by other packages easily with
the help of function `posframe-poshandler-argbuilder'.  like:

   (let* ((info (posframe-poshandler-argbuilder *MY-CHILD-FRAME*))
          (posn (posframe-poshandler-window-center
                 `(:posframe-width 800 :posframe-height 400 ,@info))))
     `((left . ,(car posn))
       (top . ,(cdr posn))))

 (3) POSHANDLER-EXTRA-INFO

POSHANDLER-EXTRA-INFO is a plist, which will prepend to the
argument of poshandler function: 'info', it will *OVERRIDE* the
exist key in 'info'.

 (4) BUFFER-OR-NAME

This posframe's buffer is BUFFER-OR-NAME, which can be a buffer
or a name of a (possibly nonexistent) buffer.

buffer name can prefix with space, for example ' *mybuffer*', so
the buffer name will hide for ibuffer and `list-buffers'.

 (5) NO-PROPERTIES

If NO-PROPERTIES is non-nil, The STRING's properties will
be removed before being shown in posframe.

 (6) HEIGHT, MAX-HEIGHT, MIN-HEIGHT, WIDTH, MAX-WIDTH and MIN-WIDTH

These arguments are specified in the canonical character width
and height of posframe, more details can be found in docstring of
function `fit-frame-to-buffer',

 (7) LEFT-FRINGE and RIGHT-FRINGE

If LEFT-FRINGE or RIGHT-FRINGE is a number, left fringe or
right fringe with be shown with the specified width.

 (8) BORDER-WIDTH, BORDER-COLOR, INTERNAL-BORDER-WIDTH and INTERNAL-BORDER-COLOR

By default, posframe shows no borders, but users can specify
borders by setting BORDER-WIDTH to a positive number.  Border
color can be specified by BORDER-COLOR.

INTERNAL-BORDER-WIDTH and INTERNAL-BORDER-COLOR are same as
BORDER-WIDTH and BORDER-COLOR, but do not suggest to use for the
reason:

   Add distinct controls for child frames' borders (Bug#45620)
   http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=ff7b1a133bfa7f2614650f8551824ffaef13fadc

 (9) FONT, FOREGROUND-COLOR and BACKGROUND-COLOR

Posframe's font as well as foreground and background colors are
derived from the current frame by default, but can be overridden
using the FONT, FOREGROUND-COLOR and BACKGROUND-COLOR arguments,
respectively.

 (10) RESPECT-HEADER-LINE and RESPECT-MODE-LINE

By default, posframe will display no header-line, mode-line and
tab-line.  In case a header-line, mode-line or tab-line is
desired, users can set RESPECT-HEADER-LINE and RESPECT-MODE-LINE
to t.

 (11) INITIALIZE

INITIALIZE is a function with no argument.  It will run when
posframe buffer is first selected with `with-current-buffer'
in `posframe-show', and only run once (for performance reasons).

 (12) LINES-TRUNCATE

If LINES-TRUNCATE is non-nil, then lines will truncate in the
posframe instead of wrap.

 (13) OVERRIDE-PARAMETERS

OVERRIDE-PARAMETERS is very powful, *all* the valid frame parameters
used by posframe's frame can be overridden by it.

NOTE: some `posframe-show' arguments are not frame parameters, so they
can not be overrided by this argument.

 (14) TIMEOUT

TIMEOUT can specify the number of seconds after which the posframe
will auto-hide.

 (15) REFRESH

If REFRESH is a number, posframe's frame-size will be re-adjusted
every REFRESH seconds.

 (16) ACCEPT-FOCUS

When ACCEPT-FOCUS is non-nil, posframe will accept focus.
be careful, you may face some bugs when set it to non-nil.

 (17) HIDEHANDLER

HIDEHANDLER is a function, when it return t, posframe will be
hide, this function has a plist argument:

  (:posframe-buffer xxx
   :posframe-parent-buffer xxx)

The builtin hidehandler functions are listed below:

1. `posframe-hidehandler-when-buffer-switch'

 (18) REFPOSHANDLER

REFPOSHANDLER is a function, a reference position (most is
top-left of current frame) will be returned when call this
function.

when it is nil or it return nil, child-frame feature will be used
and reference position will be deal with in Emacs.

The user case I know at the moment is let ivy-posframe work well
in EXWM environment (let posframe show on the other appliction
window).

         DO NOT USE UNLESS NECESSARY!!!

An example parent frame poshandler function is:

1. `posframe-refposhandler-xwininfo'

 (19) Others

You can use `posframe-delete-all' to delete all posframes.

(fn BUFFER-OR-NAME &key STRING POSITION POSHANDLER POSHANDLER-EXTRA-INFO WIDTH HEIGHT MAX-WIDTH MAX-HEIGHT MIN-WIDTH MIN-HEIGHT X-PIXEL-OFFSET Y-PIXEL-OFFSET LEFT-FRINGE RIGHT-FRINGE BORDER-WIDTH BORDER-COLOR INTERNAL-BORDER-WIDTH INTERNAL-BORDER-COLOR FONT FOREGROUND-COLOR BACKGROUND-COLOR RESPECT-HEADER-LINE RESPECT-MODE-LINE INITIALIZE NO-PROPERTIES KEEP-RATIO LINES-TRUNCATE OVERRIDE-PARAMETERS TIMEOUT REFRESH ACCEPT-FOCUS HIDEHANDLER REFPOSHANDLER &allow-other-keys)" nil nil) (autoload 'posframe-hide-all "posframe" "Hide all posframe frames." t nil) (autoload 'posframe-delete-all "posframe" "Delete all posframe frames and buffers." t nil) (register-definition-prefixes "posframe" '("posframe-")) (autoload 'posframe-benchmark "posframe-benchmark" "Benchmark tool for posframe." t nil) (register-definition-prefixes "posframe-benchmark" '("posframe-benchmark-alist")) (provide 'posframe-autoloads)) "cfrs" ((cfrs cfrs-autoloads) (autoload 'cfrs-read "cfrs" "Read a string using a pos-frame with given PROMPT and INITIAL-INPUT.

(fn PROMPT &optional INITIAL-INPUT)" nil nil) (register-definition-prefixes "cfrs" '("cfrs-")) (provide 'cfrs-autoloads)) "treemacs" ((treemacs-mode treemacs-fringe-indicator treemacs-interface treemacs-compatibility treemacs treemacs-persistence treemacs-core-utils treemacs-scope treemacs-tags treemacs-filewatch-mode treemacs-icons treemacs-macros treemacs-follow-mode treemacs-header-line treemacs-faces treemacs-rendering treemacs-file-management treemacs-visuals treemacs-workspaces treemacs-customization treemacs-autoloads treemacs-async treemacs-themes treemacs-dom treemacs-hydras treemacs-logging treemacs-extensions treemacs-bookmarks treemacs-mouse-interface treemacs-peek-mode treemacs-project-follow-mode treemacs-diagnostics treemacs-tag-follow-mode) (autoload 'treemacs-version "treemacs" "Return the `treemacs-version'." t nil) (autoload 'treemacs "treemacs" "Initialise or toggle treemacs.
* If the treemacs window is visible hide it.
* If a treemacs buffer exists, but is not visible show it.
* If no treemacs buffer exists for the current frame create and show it.
* If the workspace is empty additionally ask for the root path of the first
  project to add." t nil) (autoload 'treemacs-find-file "treemacs" "Find and focus the current file in the treemacs window.
If the current buffer has visits no file or with a prefix ARG ask for the
file instead.
Will show/create a treemacs buffers if it is not visible/does not exist.
For the most part only useful when `treemacs-follow-mode' is not active.

(fn &optional ARG)" t nil) (autoload 'treemacs-find-tag "treemacs" "Find and move point to the tag at point in the treemacs view.
Most likely to be useful when `treemacs-tag-follow-mode' is not active.

Will ask to change the treemacs root if the file to find is not under the
root.  If no treemacs buffer exists it will be created with the current file's
containing directory as root.  Will do nothing if the current buffer is not
visiting a file or Emacs cannot find any tags for the current file." t nil) (autoload 'treemacs-select-window "treemacs" "Select the treemacs window if it is visible.
Bring it to the foreground if it is not visible.
Initialise a new treemacs buffer as calling `treemacs' would if there is no
treemacs buffer for this frame.
Jump back to the previously used window if point is already in treemacs." t nil) (autoload 'treemacs-show-changelog "treemacs" "Show the changelog of treemacs." t nil) (autoload 'treemacs-edit-workspaces "treemacs" "Edit your treemacs workspaces and projects as an `org-mode' file." t nil) (autoload 'treemacs-display-current-project-exclusively "treemacs" "Display the current project, and *only* the current project.
Like `treemacs-add-and-display-current-project' this will add the current
project to treemacs based on either projectile, the built-in project.el, or the
current working directory.

However the 'exclusive' part means that it will make the current project the
only project, all other projects *will be removed* from the current workspace." t nil) (autoload 'treemacs-add-and-display-current-project "treemacs" "Open treemacs and add the current project root to the workspace.
The project is determined first by projectile (if treemacs-projectile is
installed), then by project.el, then by the current working directory.

If the project is already registered with treemacs just move point to its root.
An error message is displayed if the current buffer is not part of any project." t nil) (register-definition-prefixes "treemacs" '("treemacs-version")) (register-definition-prefixes "treemacs-async" '("treemacs-")) (autoload 'treemacs-bookmark "treemacs-bookmarks" "Find a bookmark in treemacs.
Only bookmarks marking either a file or a directory are offered for selection.
Treemacs will try to find and focus the given bookmark's location, in a similar
fashion to `treemacs-find-file'.

With a prefix argument ARG treemacs will also open the bookmarked location.

(fn &optional ARG)" t nil) (autoload 'treemacs--bookmark-handler "treemacs-bookmarks" "Open Treemacs into a bookmark RECORD.

(fn RECORD)" nil nil) (autoload 'treemacs-add-bookmark "treemacs-bookmarks" "Add the current node to Emacs' list of bookmarks.
For file and directory nodes their absolute path is saved.  Tag nodes
additionally also save the tag's position.  A tag can only be bookmarked if the
treemacs node is pointing to a valid buffer position." t nil) (register-definition-prefixes "treemacs-bookmarks" '("treemacs--")) (register-definition-prefixes "treemacs-compatibility" '("treemacs-")) (register-definition-prefixes "treemacs-core-utils" '("treemacs-")) (register-definition-prefixes "treemacs-customization" '("treemacs-")) (register-definition-prefixes "treemacs-diagnostics" '("treemacs-")) (register-definition-prefixes "treemacs-dom" '("treemacs-")) (register-definition-prefixes "treemacs-extensions" '("treemacs-")) (autoload 'treemacs-delete-file "treemacs-file-management" "Delete node at point.
A delete action must always be confirmed.  Directories are deleted recursively.
By default files are deleted by moving them to the trash.  With a prefix ARG
they will instead be wiped irreversibly.

(fn &optional ARG)" t nil) (autoload 'treemacs-move-file "treemacs-file-management" "Move file (or directory) at point.
Destination may also be a filename, in which case the moved file will also
be renamed." t nil) (autoload 'treemacs-copy-file "treemacs-file-management" "Copy file (or directory) at point.
Destination may also be a filename, in which case the copied file will also
be renamed." t nil) (autoload 'treemacs-rename-file "treemacs-file-management" "Rename the currently selected node.
Buffers visiting the renamed file or visiting a file inside a renamed directory
and windows showing them will be reloaded.  The list of recent files will
likewise be updated." t nil) (autoload 'treemacs-create-file "treemacs-file-management" "Create a new file.
Enter first the directory to create the new file in, then the new file's name.
The pre-selection for what directory to create in is based on the \"nearest\"
path to point - the containing directory for tags and files or the directory
itself, using $HOME when there is no path at or near point to grab." t nil) (autoload 'treemacs-create-dir "treemacs-file-management" "Create a new directory.
Enter first the directory to create the new dir in, then the new dir's name.
The pre-selection for what directory to create in is based on the \"nearest\"
path to point - the containing directory for tags and files or the directory
itself, using $HOME when there is no path at or near point to grab." t nil) (register-definition-prefixes "treemacs-file-management" '("treemacs-")) (register-definition-prefixes "treemacs-filewatch-mode" '("treemacs-")) (register-definition-prefixes "treemacs-follow-mode" '("treemacs-")) (register-definition-prefixes "treemacs-fringe-indicator" '("treemacs-")) (register-definition-prefixes "treemacs-header-line" '("treemacs-header-buttons-format")) (autoload 'treemacs-common-helpful-hydra "treemacs-hydras" "Summon a helpful hydra to show you the treemacs keymap.

This hydra will show the most commonly used keybinds for treemacs.  For the more
advanced (probably rarely used keybinds) see `treemacs-advanced-helpful-hydra'.

The keybinds shown in this hydra are not static, but reflect the actual
keybindings currently in use (including evil mode).  If the hydra is unable to
find the key a command is bound to it will show a blank instead." t nil) (autoload 'treemacs-advanced-helpful-hydra "treemacs-hydras" "Summon a helpful hydra to show you the treemacs keymap.

This hydra will show the more advanced (rarely used) keybinds for treemacs.  For
the more commonly used keybinds see `treemacs-common-helpful-hydra'.

The keybinds shown in this hydra are not static, but reflect the actual
keybindings currently in use (including evil mode).  If the hydra is unable to
find the key a command is bound to it will show a blank instead." t nil) (register-definition-prefixes "treemacs-hydras" '("treemacs-helpful-hydra")) (autoload 'treemacs-resize-icons "treemacs-icons" "Resize the current theme's icons to the given SIZE.

If SIZE is 'nil' the icons are not resized and will retain their default size of
22 pixels.

There is only one size, the icons are square and the aspect ratio will be
preserved when resizing them therefore width and height are the same.

Resizing the icons only works if Emacs was built with ImageMagick support, or if
using Emacs >= 27.1,which has native image resizing support.  If this is not the
case this function will not have any effect.

Custom icons are not taken into account, only the size of treemacs' own icons
png are changed.

(fn SIZE)" t nil) (autoload 'treemacs-define-custom-icon "treemacs-icons" "Define a custom ICON for the current theme to use for FILE-EXTENSIONS.

Note that treemacs has a very loose definition of what constitutes a file
extension - it's either everything past the last period, or just the file's full
name if there is no period.  This makes it possible to match file names like
'.gitignore' and 'Makefile'.

Additionally FILE-EXTENSIONS are also not case sensitive and will be stored in a
down-cased state.

(fn ICON &rest FILE-EXTENSIONS)" nil nil) (autoload 'treemacs-define-custom-image-icon "treemacs-icons" "Same as `treemacs-define-custom-icon' but for image icons instead of strings.
FILE is the path to an icon image (and not the actual icon string).
FILE-EXTENSIONS are all the (not case-sensitive) file extensions the icon
should be used for.

(fn FILE &rest FILE-EXTENSIONS)" nil nil) (autoload 'treemacs-map-icons-with-auto-mode-alist "treemacs-icons" "Remaps icons for EXTENSIONS according to `auto-mode-alist'.
EXTENSIONS should be a list of file extensions such that they match the regex
stored in `auto-mode-alist', for example '(\".cc\").
MODE-ICON-ALIST is an alist that maps which mode from `auto-mode-alist' should
be assigned which treemacs icon, for example
'((c-mode . treemacs-icon-c)
  (c++-mode . treemacs-icon-cpp))

(fn EXTENSIONS MODE-ICON-ALIST)" nil nil) (register-definition-prefixes "treemacs-icons" '("treemacs-")) (register-definition-prefixes "treemacs-interface" '("treemacs-")) (register-definition-prefixes "treemacs-logging" '("treemacs-")) (register-definition-prefixes "treemacs-macros" '("treemacs-")) (autoload 'treemacs-mode "treemacs-mode" "A major mode for displaying the file system in a tree layout.

(fn)" t nil) (register-definition-prefixes "treemacs-mode" '("treemacs-")) (autoload 'treemacs-leftclick-action "treemacs-mouse-interface" "Move focus to the clicked line.
Must be bound to a mouse click, or EVENT will not be supplied.

(fn EVENT)" t nil) (autoload 'treemacs-doubleclick-action "treemacs-mouse-interface" "Run the appropriate double-click action for the current node.
In the default configuration this means to do the same as `treemacs-RET-action'.

This function's exact configuration is stored in
`treemacs-doubleclick-actions-config'.

Must be bound to a mouse click, or EVENT will not be supplied.

(fn EVENT)" t nil) (autoload 'treemacs-single-click-expand-action "treemacs-mouse-interface" "A modified single-leftclick action that expands the clicked nodes.
Can be bound to <mouse1> if you prefer to expand nodes with a single click
instead of a double click.  Either way it must be bound to a mouse click, or
EVENT will not be supplied.

Clicking on icons will expand a file's tags, just like
`treemacs-leftclick-action'.

(fn EVENT)" t nil) (autoload 'treemacs-dragleftclick-action "treemacs-mouse-interface" "Drag a file/dir node to be opened in a window.
Must be bound to a mouse click, or EVENT will not be supplied.

(fn EVENT)" t nil) (autoload 'treemacs-define-doubleclick-action "treemacs-mouse-interface" "Define the behaviour of `treemacs-doubleclick-action'.
Determines that a button with a given STATE should lead to the execution of
ACTION.

The list of possible states can be found in `treemacs-valid-button-states'.
ACTION should be one of the `treemacs-visit-node-*' commands.

(fn STATE ACTION)" nil nil) (autoload 'treemacs-node-buffer-and-position "treemacs-mouse-interface" "Return source buffer or list of buffer and position for the current node.
This information can be used for future display.  Stay in the selected window
and ignore any prefix argument.

(fn &optional _)" t nil) (autoload 'treemacs-rightclick-menu "treemacs-mouse-interface" "Show a contextual right click menu based on click EVENT.

(fn EVENT)" t nil) (register-definition-prefixes "treemacs-mouse-interface" '("treemacs--")) (defvar treemacs-peek-mode nil "Non-nil if Treemacs-Peek mode is enabled.
See the `treemacs-peek-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-peek-mode'.") (custom-autoload 'treemacs-peek-mode "treemacs-peek-mode" nil) (autoload 'treemacs-peek-mode "treemacs-peek-mode" "Minor mode that allows you to peek at buffers before deciding to open them.

If called interactively, enable Treemacs-Peek mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

While the mode is active treemacs will automatically display the file at point,
without leving the treemacs window.

Peeking will stop when you leave the treemacs window, be it through a command
like `treemacs-RET-action' or some other window selection change.

Files' buffers that have been opened for peeking will be cleaned up if they did
not exist before peeking started.

The peeked window can be scrolled using
`treemacs-next/previous-line-other-window' and
`treemacs-next/previous-page-other-window'

(fn &optional ARG)" t nil) (register-definition-prefixes "treemacs-peek-mode" '("treemacs--")) (register-definition-prefixes "treemacs-persistence" '("treemacs-")) (defvar treemacs-project-follow-mode nil "Non-nil if Treemacs-Project-Follow mode is enabled.
See the `treemacs-project-follow-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-project-follow-mode'.") (custom-autoload 'treemacs-project-follow-mode "treemacs-project-follow-mode" nil) (autoload 'treemacs-project-follow-mode "treemacs-project-follow-mode" "Toggle `treemacs-only-current-project-mode'.

If called interactively, enable Treemacs-Project-Follow mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This is a minor mode meant for those who do not care about treemacs' workspace
features, or its preference to work with multiple projects simultaneously.  When
enabled it will function as an automated version of
`treemacs-display-current-project-exclusively', making sure that, after a small
idle delay, the current project, and *only* the current project, is displayed in
treemacs.

The project detection is based on the current buffer, and will try to determine
the project using the following methods, in the order they are listed:

- the current projectile.el project, if `treemacs-projectile' is installed
- the current project.el project
- the current `default-directory'

The update will only happen when treemacs is in the foreground, meaning a
treemacs window must exist in the current scope.

This mode requires at least Emacs version 27 since it relies on
`window-buffer-change-functions' and `window-selection-change-functions'.

(fn &optional ARG)" t nil) (register-definition-prefixes "treemacs-project-follow-mode" '("treemacs--")) (register-definition-prefixes "treemacs-rendering" '("treemacs-")) (register-definition-prefixes "treemacs-scope" '("treemacs-")) (autoload 'treemacs--flatten&sort-imenu-index "treemacs-tag-follow-mode" "Flatten current file's imenu index and sort it by tag position.
The tags are sorted into the order in which they appear, regardless of section
or nesting depth." nil nil) (defvar treemacs-tag-follow-mode nil "Non-nil if Treemacs-Tag-Follow mode is enabled.
See the `treemacs-tag-follow-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `treemacs-tag-follow-mode'.") (custom-autoload 'treemacs-tag-follow-mode "treemacs-tag-follow-mode" nil) (autoload 'treemacs-tag-follow-mode "treemacs-tag-follow-mode" "Toggle `treemacs-tag-follow-mode'.

If called interactively, enable Treemacs-Tag-Follow mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This acts as more fine-grained alternative to `treemacs-follow-mode' and will
thus disable `treemacs-follow-mode' on activation.  When enabled treemacs will
focus not only the file of the current buffer, but also the tag at point.

The follow action is attached to Emacs' idle timer and will run
`treemacs-tag-follow-delay' seconds of idle time.  The delay value is not an
integer, meaning it accepts floating point values like 1.5.

Every time a tag is followed a re--scan of the imenu index is forced by
temporarily setting `imenu-auto-rescan' to t (though a cache is applied as long
as the buffer is unmodified).  This is necessary to assure that creation or
deletion of tags does not lead to errors and guarantees an always up-to-date tag
view.

Note that in order to move to a tag in treemacs the treemacs buffer's window
needs to be temporarily selected, which will reset blink-cursor-mode's timer if
it is enabled.  This will result in the cursor blinking seemingly pausing for a
short time and giving the appearance of the tag follow action lasting much
longer than it really does.

(fn &optional ARG)" t nil) (register-definition-prefixes "treemacs-tag-follow-mode" '("treemacs--")) (autoload 'treemacs--expand-file-node "treemacs-tags" "Open tag items for file BTN.
Recursively open all tags below BTN when RECURSIVE is non-nil.

(fn BTN &optional RECURSIVE)" nil nil) (autoload 'treemacs--collapse-file-node "treemacs-tags" "Close node given by BTN.
Remove all open tag entries under BTN when RECURSIVE.

(fn BTN &optional RECURSIVE)" nil nil) (autoload 'treemacs--visit-or-expand/collapse-tag-node "treemacs-tags" "Visit tag section BTN if possible, expand or collapse it otherwise.
Pass prefix ARG on to either visit or toggle action.

FIND-WINDOW is a special provision depending on this function's invocation
context and decides whether to find the window to display in (if the tag is
visited instead of the node being expanded).

On the one hand it can be called based on `treemacs-RET-actions-config' (or
TAB).  The functions in these configs are expected to find the windows they need
to display in themselves, so FIND-WINDOW must be t. On the other hand this
function is also called from the top level vist-node functions like
`treemacs-visit-node-vertical-split' which delegates to the
`treemacs--execute-button-action' macro which includes the determination of
the display window.

(fn BTN ARG FIND-WINDOW)" nil nil) (autoload 'treemacs--expand-tag-node "treemacs-tags" "Open tags node items for BTN.
Open all tag section under BTN when call is RECURSIVE.

(fn BTN &optional RECURSIVE)" nil nil) (autoload 'treemacs--collapse-tag-node "treemacs-tags" "Close tags node at BTN.
Remove all open tag entries under BTN when RECURSIVE.

(fn BTN &optional RECURSIVE)" nil nil) (autoload 'treemacs--goto-tag "treemacs-tags" "Go to the tag at BTN.

(fn BTN)" nil nil) (autoload 'treemacs--create-imenu-index-function "treemacs-tags" "The `imenu-create-index-function' for treemacs buffers." nil nil) (function-put 'treemacs--create-imenu-index-function 'side-effect-free 't) (register-definition-prefixes "treemacs-tags" '("treemacs--")) (register-definition-prefixes "treemacs-themes" '("treemacs-")) (register-definition-prefixes "treemacs-visuals" '("treemacs-")) (register-definition-prefixes "treemacs-workspaces" '("treemacs-")) (provide 'treemacs-autoloads)) "lsp-treemacs" ((lsp-treemacs lsp-treemacs-autoloads lsp-treemacs-themes) (autoload 'lsp-treemacs-symbols "lsp-treemacs" "Show symbols view." t nil) (autoload 'lsp-treemacs-java-deps-list "lsp-treemacs" "Display java dependencies." t nil) (autoload 'lsp-treemacs-java-deps-follow "lsp-treemacs" nil t nil) (defvar lsp-treemacs-sync-mode nil "Non-nil if Lsp-Treemacs-Sync mode is enabled.
See the `lsp-treemacs-sync-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `lsp-treemacs-sync-mode'.") (custom-autoload 'lsp-treemacs-sync-mode "lsp-treemacs" nil) (autoload 'lsp-treemacs-sync-mode "lsp-treemacs" "Global minor mode for synchronizing lsp-mode workspace folders and treemacs projects.

If called interactively, enable Lsp-Treemacs-Sync mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'lsp-treemacs-references "lsp-treemacs" "Show the references for the symbol at point.
With a prefix argument, select the new window and expand the tree of references automatically.

(fn ARG)" t nil) (autoload 'lsp-treemacs-implementations "lsp-treemacs" "Show the implementations for the symbol at point.
With a prefix argument, select the new window expand the tree of implementations automatically.

(fn ARG)" t nil) (autoload 'lsp-treemacs-call-hierarchy "lsp-treemacs" "Show the incoming call hierarchy for the symbol at point.
With a prefix argument, show the outgoing call hierarchy.

(fn OUTGOING)" t nil) (autoload 'lsp-treemacs-type-hierarchy "lsp-treemacs" "Show the type hierarchy for the symbol at point.
With prefix 0 show sub-types.
With prefix 1 show super-types.
With prefix 2 show both.

(fn DIRECTION)" t nil) (autoload 'lsp-treemacs-errors-list "lsp-treemacs" nil t nil) (register-definition-prefixes "lsp-treemacs" '("lsp-tree")) (register-definition-prefixes "lsp-treemacs-themes" '("lsp-treemacs-theme")) (provide 'lsp-treemacs-autoloads)) "dap-mode" ((dap-elixir dap-ui dapui dap-go dap-mode dap-ruby dap-firefox dap-node dap-netcore dap-mode-autoloads dap-chrome dap-lldb dap-python dap-cpptools dap-launch dap-php dap-pwsh dap-utils dap-codelldb dap-overlays dap-edge dap-mouse dap-gdb-lldb dap-variables dap-swi-prolog dap-erlang dap-hydra) (register-definition-prefixes "dap-chrome" '("dap-chrome-")) (register-definition-prefixes "dap-codelldb" '("dap-codelldb-")) (register-definition-prefixes "dap-cpptools" '("dap-cpptools-")) (register-definition-prefixes "dap-edge" '("dap-edge-")) (register-definition-prefixes "dap-elixir" '("dap-elixir--populate-start-file-args")) (register-definition-prefixes "dap-erlang" '("dap-erlang--populate-start-file-args")) (register-definition-prefixes "dap-firefox" '("dap-firefox-")) (register-definition-prefixes "dap-gdb-lldb" '("dap-gdb-lldb-")) (register-definition-prefixes "dap-go" '("dap-go-")) (autoload 'dap-hydra "dap-hydra" "Run `dap-hydra/body'." t nil) (register-definition-prefixes "dap-hydra" '("dap-hydra")) (register-definition-prefixes "dap-launch" '("dap-launch-")) (register-definition-prefixes "dap-lldb" '("dap-lldb-")) (autoload 'dap-debug "dap-mode" "Run debug configuration DEBUG-ARGS.

If DEBUG-ARGS is not specified the configuration is generated
after selecting configuration template.

:dap-compilation specifies a shell command to be run using
`compilation-start' before starting the debug session. It could
be used to compile the project, spin up docker, ....

(fn DEBUG-ARGS)" t nil) (defvar dap-mode nil "Non-nil if Dap mode is enabled.
See the `dap-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-mode'.") (custom-autoload 'dap-mode "dap-mode" nil) (autoload 'dap-mode "dap-mode" "Global minor mode for DAP mode.

If called interactively, enable Dap mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (defvar dap-auto-configure-mode nil "Non-nil if Dap-Auto-Configure mode is enabled.
See the `dap-auto-configure-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-auto-configure-mode'.") (custom-autoload 'dap-auto-configure-mode "dap-mode" nil) (autoload 'dap-auto-configure-mode "dap-mode" "Auto configure dap minor mode.

If called interactively, enable Dap-Auto-Configure mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "dap-mode" '("dap-")) (defvar dap-tooltip-mode nil "Non-nil if Dap-Tooltip mode is enabled.
See the `dap-tooltip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-tooltip-mode'.") (custom-autoload 'dap-tooltip-mode "dap-mouse" nil) (autoload 'dap-tooltip-mode "dap-mouse" "Toggle the display of GUD tooltips.

If called interactively, enable Dap-Tooltip mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "dap-mouse" '("dap-")) (register-definition-prefixes "dap-netcore" '("dap-netcore-")) (register-definition-prefixes "dap-node" '("dap-node-")) (register-definition-prefixes "dap-overlays" '("dap-overlays-")) (register-definition-prefixes "dap-php" '("dap-php-")) (register-definition-prefixes "dap-pwsh" '("dap-pwsh-")) (register-definition-prefixes "dap-python" '("dap-python-")) (register-definition-prefixes "dap-ruby" '("dap-ruby-")) (register-definition-prefixes "dap-swi-prolog" '("dap-swi-prolog-")) (defvar dap-ui-mode nil "Non-nil if Dap-Ui mode is enabled.
See the `dap-ui-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-ui-mode'.") (custom-autoload 'dap-ui-mode "dap-ui" nil) (autoload 'dap-ui-mode "dap-ui" "Displaying DAP visuals.

If called interactively, enable Dap-Ui mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'dap-ui-breakpoints-list "dap-ui" "List breakpoints." t nil) (defvar dap-ui-controls-mode nil "Non-nil if Dap-Ui-Controls mode is enabled.
See the `dap-ui-controls-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dap-ui-controls-mode'.") (custom-autoload 'dap-ui-controls-mode "dap-ui" nil) (autoload 'dap-ui-controls-mode "dap-ui" "Displaying DAP visuals.

If called interactively, enable Dap-Ui-Controls mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'dap-ui-sessions "dap-ui" "Show currently active sessions." t nil) (autoload 'dap-ui-locals "dap-ui" nil t nil) (autoload 'dap-ui-show-many-windows "dap-ui" "Show auto configured feature windows." t nil) (autoload 'dap-ui-hide-many-windows "dap-ui" "Hide all debug windows when sessions are dead." t nil) (autoload 'dap-ui-repl "dap-ui" "Start an adapter-specific REPL.
This could be used to evaluate JavaScript in a browser, to
evaluate python in the context of the debugee, ...." t nil) (register-definition-prefixes "dap-ui" '("dap-")) (register-definition-prefixes "dap-utils" '("dap-utils-")) (register-definition-prefixes "dap-variables" '("dap-variables-")) (autoload 'dapui-loaded-sources "dapui" nil t nil) (register-definition-prefixes "dapui" '("dapui-")) (provide 'dap-mode-autoloads)) "srefactor" ((srefactor-autoloads srefactor-lisp srefactor-ui srefactor) (autoload 'srefactor-refactor-at-point "srefactor" "Offer contextual menu with actions based on current tag in scope.

Each menu item added returns a token for what type of refactoring
to perform." t nil) (register-definition-prefixes "srefactor" '("srefactor-")) (register-definition-prefixes "srefactor-lisp" '("comment-" "cur-buf" "first-token" "format-type" "ignore-num" "lexemes" "next-token" "orig-format-type" "recursive-p" "second-token" "srefactor-" "tok")) (register-definition-prefixes "srefactor-ui" '("srefactor-ui-")) (provide 'srefactor-autoloads)) "mozc" ((mozc mozc-autoloads) (defvar mozc-mode-map (let ((map (make-sparse-keymap))) (prog1 map (mapc (lambda (command) (mapc (lambda (key-sequence) (and (= (length key-sequence) 1) (integerp (aref key-sequence 0)) (define-key map key-sequence command))) (where-is-internal command global-map))) '(execute-extended-command toggle-input-method)) (mapc (lambda (event) (define-key map (vector event) nil)) '(delete-frame iconify-frame make-frame-visible select-window switch-frame)) (define-key map [t] #'mozc-handle-event))) "Keymap for function `mozc-mode'.") (defvar mozc-mode nil "Mode variable of function `mozc-mode'.
Non-nil means function `mozc-mode' is enabled.") (autoload 'mozc-mode "mozc" "Minor mode to input Japanese text with Mozc.
Toggle the mode if ARG is not given, or enable/disable the mode
according to ARG.

Hooks in `mozc-mode-hook' are run when the mode gets enabled.

Return non-nil when enabled, otherwise nil.


Tips for customizations

By the design policy, Mozc maintains most of user settings on
the server side.  Clients, including mozc.el, of Mozc do not
have many user settings on their side.

You can change a variety of user settings through a GUI command
line tool 'mozc_tool' which must be shipped with the mozc server.
The command line tool may be installed to /usr/lib/mozc or /usr/lib
directory.
You need a command line option '--mode=config_dialog' as the
following.

  $ /usr/lib/mozc/mozc_tool --mode=config_dialog

Then, it shows a GUI dialog to edit your user settings.

Note these settings are effective for all the clients of Mozc,
not limited to mozc.el.

Only the customizable item on mozc.el side is the key map for kana
input.  When you've chosen kana input rather than roman input,
a kana key map is effective, and you can customize it.

There are two built-in kana key maps, one for 106 JP keyboards and
one for 101 US keyboards.  You can choose one of them by setting
`mozc-keymap-kana' variable.

  ;; for 106 JP keyboards
  (setq mozc-keymap-kana mozc-keymap-kana-106jp)

  ;; for 101 US keyboards
  (setq mozc-keymap-kana mozc-keymap-kana-101us)

For advanced users, there are APIs for more detailed customization
or even creating your own key map.
See `mozc-keymap-get-entry', `mozc-keymap-put-entry',
`mozc-keymap-remove-entry', and `mozc-keymap-make-keymap' and
`mozc-keymap-make-keymap-from-flat-list'.

(fn &optional ARG)" t nil) (defun mozc-keymap-make-keymap nil "Create a new empty keymap and return it." (make-hash-table :size 128 :test #'eq)) (defun mozc-keymap-make-keymap-from-flat-list (list) "Create a new keymap and fill it with entries in LIST.
LIST must be a flat list which contains keys and mapped strings alternately." (mozc-keymap-fill-entries-from-flat-list (mozc-keymap-make-keymap) list)) (defun mozc-keymap-fill-entries-from-flat-list (keymap list) "Fill KEYMAP with entries in LIST and return KEYMAP.
KEYMAP must be a key table from keycodes to mapped strings.
LIST must be a flat list which contains keys and mapped strings alternately." (if (not (and (car list) (cadr list))) keymap (mozc-keymap-put-entry keymap (car list) (cadr list)) (mozc-keymap-fill-entries-from-flat-list keymap (cddr list)))) (defun mozc-keymap-get-entry (keymap keycode &optional default) "Return a mapped string if the entry for the keycode exists.
Otherwise, the default value, which must be a string.
KEYMAP must be a key table from keycodes to mapped strings.
KEYCODE must be an integer representing a key code to look up.
DEFAULT is returned if it's a string and no entry for KEYCODE is found." (let ((value (and (hash-table-p keymap) (gethash keycode keymap default)))) (and (stringp value) value))) (defun mozc-keymap-put-entry (keymap keycode mapped-string) "Add a new key mapping to a keymap.
KEYMAP must be a key table from keycodes to mapped strings.
KEYCODE must be an integer representing a key code.
MAPPED-STRING must be a string representing a preedit string to be inserted." (when (and (hash-table-p keymap) (integerp keycode) (stringp mapped-string)) (puthash keycode mapped-string keymap) (cons keycode mapped-string))) (defun mozc-keymap-remove-entry (keymap keycode) "Remove an entry from a keymap.  If no entry for keycode exists, do nothing.
KEYMAP must be a key table from keycodes to mapped strings.
KEYCODE must be an integer representing a key code to remove." (when (hash-table-p keymap) (remhash keycode keymap))) (defvar mozc-keymap-kana-106jp (mozc-keymap-make-keymap-from-flat-list '(49 "\343\201\254" 50 "\343\201\265" 51 "\343\201\202" 52 "\343\201\206" 53 "\343\201\210" 54 "\343\201\212" 55 "\343\202\204" 56 "\343\202\206" 57 "\343\202\210" 48 "\343\202\217" 45 "\343\201\273" 94 "\343\201\270" 124 "\343\203\274" 113 "\343\201\237" 119 "\343\201\246" 101 "\343\201\204" 114 "\343\201\231" 116 "\343\201\213" 121 "\343\202\223" 117 "\343\201\252" 105 "\343\201\253" 111 "\343\202\211" 112 "\343\201\233" 64 "\343\202\233" 91 "\343\202\234" 97 "\343\201\241" 115 "\343\201\250" 100 "\343\201\227" 102 "\343\201\257" 103 "\343\201\215" 104 "\343\201\217" 106 "\343\201\276" 107 "\343\201\256" 108 "\343\202\212" 59 "\343\202\214" 58 "\343\201\221" 93 "\343\202\200" 122 "\343\201\244" 120 "\343\201\225" 99 "\343\201\235" 118 "\343\201\262" 98 "\343\201\223" 110 "\343\201\277" 109 "\343\202\202" 44 "\343\201\255" 46 "\343\202\213" 47 "\343\202\201" 92 "\343\202\215" 35 "\343\201\201" 69 "\343\201\203" 36 "\343\201\205" 37 "\343\201\207" 38 "\343\201\211" 39 "\343\202\203" 40 "\343\202\205" 41 "\343\202\207" 126 "\343\202\222" 90 "\343\201\243" 60 "\343\200\201" 62 "\343\200\202" 63 "\343\203\273" 123 "\343\200\214" 125 "\343\200\215" 80 "\343\200\216" 43 "\343\200\217" 95 "\343\202\215" 70 "\343\202\216" 84 "\343\203\265" 42 "\343\203\266")) "Key mapping from key codes to Kana strings based on 106-JP keyboard.") (defvar mozc-keymap-kana-101us (mozc-keymap-make-keymap-from-flat-list '(49 "\343\201\254" 50 "\343\201\265" 51 "\343\201\202" 52 "\343\201\206" 53 "\343\201\210" 54 "\343\201\212" 55 "\343\202\204" 56 "\343\202\206" 57 "\343\202\210" 48 "\343\202\217" 45 "\343\201\273" 61 "\343\201\270" 96 "\343\202\215" 113 "\343\201\237" 119 "\343\201\246" 101 "\343\201\204" 114 "\343\201\231" 116 "\343\201\213" 121 "\343\202\223" 117 "\343\201\252" 105 "\343\201\253" 111 "\343\202\211" 112 "\343\201\233" 91 "\343\202\233" 93 "\343\202\234" 92 "\343\202\200" 97 "\343\201\241" 115 "\343\201\250" 100 "\343\201\227" 102 "\343\201\257" 103 "\343\201\215" 104 "\343\201\217" 106 "\343\201\276" 107 "\343\201\256" 108 "\343\202\212" 59 "\343\202\214" 39 "\343\201\221" 122 "\343\201\244" 120 "\343\201\225" 99 "\343\201\235" 118 "\343\201\262" 98 "\343\201\223" 110 "\343\201\277" 109 "\343\202\202" 44 "\343\201\255" 46 "\343\202\213" 47 "\343\202\201" 35 "\343\201\201" 69 "\343\201\203" 36 "\343\201\205" 37 "\343\201\207" 94 "\343\201\211" 38 "\343\202\203" 42 "\343\202\205" 40 "\343\202\207" 41 "\343\202\222" 90 "\343\201\243" 60 "\343\200\201" 62 "\343\200\202" 63 "\343\203\273" 123 "\343\200\214" 125 "\343\200\215" 80 "\343\200\216" 58 "\343\200\217" 95 "\343\203\274" 124 "\343\203\274" 70 "\343\202\216" 86 "\343\202\220" 43 "\343\202\221" 84 "\343\203\265" 34 "\343\203\266")) "Key mapping from key codes to Kana strings based on 101-US keyboard.") (defvar mozc-keymap-kana mozc-keymap-kana-106jp "The default key mapping for Kana input method.") (defcustom mozc-leim-title "[Mozc]" "Mode line string shown when function `mozc-mode' is enabled.
This indicator is not shown when you don't use LEIM." :type '(choice (const :tag "No indicator" nil) (string :tag "Show an indicator")) :group 'mozc) (defun mozc-leim-register-input-method nil "Register function `mozc-mode' as an input method of LEIM.
Do nothing if LEIM is not available." (if (fboundp #'register-input-method) (register-input-method "japanese-mozc" "Japanese" (lambda (input-method) (let ((new 'deactivate-current-input-method-function) (old 'inactivate-current-input-method-function)) (set (if (boundp new) new old) (lambda nil (mozc-mode nil)))) (mozc-mode t)) mozc-leim-title "Japanese input method with Mozc."))) (add-hook 'emacs-startup-hook #'mozc-leim-register-input-method) (mozc-leim-register-input-method) (register-definition-prefixes "mozc" '("mozc-")) (provide 'mozc-autoloads))))

#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data (org-elpa #s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 11 "melpa" nil "gnu-elpa-mirror" nil "el-get" nil "emacsmirror-mirror" nil "straight" nil "use-package" nil "bind-key" nil "yk-util" nil "dash" nil "init-loader" nil "cl-lib" nil "zenburn-theme" nil "col-highlight" nil "vline" nil "powerline" nil "total-lines" nil "cursor-chg" nil "magit" nil "git-commit" nil "transient" nil "with-editor" nil "magit-section" nil "helm-ls-git" nil "helm" nil "async" nil "popup" nil "helm-core" nil "git-gutter" nil "projectile" nil "helm-projectile" nil "org" (org :type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :local-repo "org" :depth full :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))) "open-junk-file" nil "lispxmp" nil "paredit" nil "auto-async-byte-compile" nil "powershell" nil "dockerfile-mode" nil "docker-compose-mode" nil "yaml-mode" nil "cmake-mode" nil "protobuf-mode" nil "csv-mode" nil "go-mode" nil "typescript-mode" nil "cuda-mode" nil "rust-mode" nil "cargo" nil "markdown-mode" nil "flycheck-rust" nil "flycheck" nil "pkg-info" nil "epl" nil "let-alist" nil "seq" nil "web-mode" nil "prettier-js" nil "glsl-mode" nil "clang-format+" nil "clang-format" nil "rainbow-delimiters" nil "flyspell" nil "ag" nil "s" nil "wgrep-ag" nil "wgrep" nil "undo-tree" nil "buffer-move" nil "fast-scroll" nil "edit-server" nil "multi-term" nil "gxref" nil "eldoc" nil "ein" nil "websocket" nil "anaphora" nil "request" nil "deferred" nil "polymode" nil "pdf-tools" nil "tablist" nil "yatex" nil "rainbow-mode" nil "migemo" nil "google-translate" nil "avy" nil "helm-descbinds" nil "helm-tramp" nil "helm-ag" nil "docker" nil "docker-tramp" nil "json-mode" nil "json-snatcher" nil "hydra" nil "lv" nil "yapfify" nil "py-isort" nil "ace-window" nil "which-key" nil "package-lint" nil "lsp-mode" nil "f" nil "ht" nil "spinner" nil "helm-lsp" nil "lsp-docker+" nil "lsp-docker" nil "lsp-ui" nil "lsp-pyright" nil "ccls" nil "yasnippet" nil "company" nil "company-tabnine" nil "unicode-escape" nil "names" nil "dap-mode" nil "bui" nil "lsp-treemacs" nil "treemacs" nil "pfuture" nil "cfrs" nil "posframe" nil "srefactor" nil "autoinsert" nil "mozc" nil)) melpa #s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "gnu-elpa-mirror" nil "el-get" (el-get :type git :flavor melpa :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :host github :repo "dimitri/el-get") "emacsmirror-mirror" nil "straight" nil "use-package" (use-package :type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package") "bind-key" (bind-key :type git :flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :host github :repo "jwiegley/use-package") "yk-util" nil "dash" (dash :type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el") "init-loader" (init-loader :type git :flavor melpa :files ("init-loader.el" "init-loader-pkg.el") :host github :repo "emacs-jp/init-loader") "cl-lib" nil "zenburn-theme" (zenburn-theme :type git :flavor melpa :host github :repo "bbatsov/zenburn-emacs") "col-highlight" nil "vline" (vline :type git :flavor melpa :host github :repo "buzztaiki/vline") "powerline" (powerline :type git :flavor melpa :host github :repo "milkypostman/powerline") "total-lines" (total-lines :type git :flavor melpa :host github :repo "hinrik/total-lines") "cursor-chg" nil "magit" (magit :type git :flavor melpa :files ("lisp/magit" "lisp/magit*.el" "lisp/git-rebase.el" "Documentation/magit.texi" "Documentation/AUTHORS.md" "LICENSE" (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el") "magit-pkg.el") :host github :repo "magit/magit") "git-commit" (git-commit :type git :flavor melpa :files ("lisp/git-commit.el" "lisp/git-commit-pkg.el" "git-commit-pkg.el") :host github :repo "magit/magit") "transient" (transient :type git :flavor melpa :files ("lisp/*.el" "docs/transient.texi" "transient-pkg.el") :host github :repo "magit/transient") "with-editor" (with-editor :type git :flavor melpa :host github :repo "magit/with-editor") "magit-section" (magit-section :type git :flavor melpa :files ("lisp/magit-section.el" "lisp/magit-section-pkg.el" "Documentation/magit-section.texi" "magit-section-pkg.el") :host github :repo "magit/magit") "helm-ls-git" (helm-ls-git :type git :flavor melpa :host github :repo "emacs-helm/helm-ls-git") "helm" (helm :type git :flavor melpa :files ("*.el" "emacs-helm.sh" (:exclude "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") "helm-pkg.el") :host github :repo "emacs-helm/helm") "async" (async :type git :flavor melpa :host github :repo "jwiegley/emacs-async") "popup" (popup :type git :flavor melpa :files ("popup.el" "popup-pkg.el") :host github :repo "auto-complete/popup-el") "helm-core" (helm-core :type git :flavor melpa :files ("helm-core-pkg.el" "helm.el" "helm-lib.el" "helm-source.el" "helm-multi-match.el" "helm-core-pkg.el") :host github :repo "emacs-helm/helm") "git-gutter" (git-gutter :type git :flavor melpa :host github :repo "emacsorphanage/git-gutter") "projectile" (projectile :type git :flavor melpa :files ("projectile.el" "projectile-pkg.el") :host github :repo "bbatsov/projectile") "helm-projectile" (helm-projectile :type git :flavor melpa :host github :repo "bbatsov/helm-projectile") "open-junk-file" (open-junk-file :type git :flavor melpa :host github :repo "rubikitch/open-junk-file") "lispxmp" (lispxmp :type git :flavor melpa :host github :repo "rubikitch/lispxmp") "paredit" (paredit :type git :flavor melpa :files ("paredit.el" "paredit-pkg.el") :repo "https://mumble.net/~campbell/git/paredit.git") "auto-async-byte-compile" (auto-async-byte-compile :type git :flavor melpa :host github :repo "rubikitch/auto-async-byte-compile") "powershell" (powershell :type git :flavor melpa :host github :repo "jschaf/powershell.el") "dockerfile-mode" (dockerfile-mode :type git :flavor melpa :host github :repo "spotify/dockerfile-mode") "docker-compose-mode" (docker-compose-mode :type git :flavor melpa :files (:defaults (:exclude "docker-compose-mode-helpers.el") "docker-compose-mode-pkg.el") :host github :repo "meqif/docker-compose-mode") "yaml-mode" (yaml-mode :type git :flavor melpa :host github :repo "yoshiki/yaml-mode") "cmake-mode" (cmake-mode :type git :flavor melpa :files ("Auxiliary/*.el" "cmake-mode-pkg.el") :repo "https://gitlab.kitware.com/cmake/cmake.git") "protobuf-mode" (protobuf-mode :type git :flavor melpa :files ("editors/protobuf-mode.el" "protobuf-mode-pkg.el") :host github :repo "google/protobuf") "csv-mode" nil "go-mode" (go-mode :type git :flavor melpa :files ("go-mode.el" "go-mode-pkg.el") :host github :repo "dominikh/go-mode.el") "typescript-mode" (typescript-mode :type git :flavor melpa :host github :repo "emacs-typescript/typescript.el") "cuda-mode" (cuda-mode :type git :flavor melpa :host github :repo "chachi/cuda-mode") "rust-mode" (rust-mode :type git :flavor melpa :host github :repo "rust-lang/rust-mode") "cargo" (cargo :type git :flavor melpa :host github :repo "kwrooijen/cargo.el") "markdown-mode" (markdown-mode :type git :flavor melpa :host github :repo "jrblevin/markdown-mode") "flycheck-rust" (flycheck-rust :type git :flavor melpa :host github :repo "flycheck/flycheck-rust") "flycheck" (flycheck :type git :flavor melpa :host github :repo "flycheck/flycheck") "pkg-info" (pkg-info :type git :flavor melpa :host github :repo "emacsorphanage/pkg-info") "epl" (epl :type git :flavor melpa :host github :repo "cask/epl") "let-alist" nil "seq" nil "web-mode" (web-mode :type git :flavor melpa :host github :repo "fxbois/web-mode") "prettier-js" (prettier-js :type git :flavor melpa :host github :repo "prettier/prettier-emacs") "glsl-mode" (glsl-mode :type git :flavor melpa :host github :repo "jimhourihan/glsl-mode") "clang-format+" (clang-format+ :type git :flavor melpa :host github :repo "SavchenkoValeriy/emacs-clang-format-plus") "clang-format" (clang-format :type git :flavor melpa :host github :repo "emacsmirror/clang-format") "rainbow-delimiters" (rainbow-delimiters :type git :flavor melpa :host github :repo "Fanael/rainbow-delimiters") "flyspell" nil "ag" (ag :type git :flavor melpa :host github :repo "Wilfred/ag.el") "s" (s :type git :flavor melpa :files ("s.el" "s-pkg.el") :host github :repo "magnars/s.el") "wgrep-ag" (wgrep-ag :type git :flavor melpa :files ("wgrep-ag.el" "wgrep-ag-pkg.el") :host github :repo "mhayashi1120/Emacs-wgrep") "wgrep" (wgrep :type git :flavor melpa :files ("wgrep.el" "wgrep-pkg.el") :host github :repo "mhayashi1120/Emacs-wgrep") "undo-tree" nil "buffer-move" (buffer-move :type git :flavor melpa :host github :repo "lukhas/buffer-move") "fast-scroll" (fast-scroll :type git :flavor melpa :host github :repo "ahungry/fast-scroll") "edit-server" (edit-server :type git :flavor melpa :files ("servers/edit-server.el" "edit-server-pkg.el") :host github :repo "stsquad/emacs_chrome") "multi-term" (multi-term :type git :flavor melpa :host github :repo "manateelazycat/multi-term") "gxref" (gxref :type git :flavor melpa :host github :repo "dedi/gxref") "eldoc" nil "ein" (ein :type git :flavor melpa :files ("lisp/*" (:exclude "lisp/zeroein.el") "ein-pkg.el") :host github :repo "millejoh/emacs-ipython-notebook") "websocket" (websocket :type git :flavor melpa :host github :repo "ahyatt/emacs-websocket") "anaphora" (anaphora :type git :flavor melpa :host github :repo "rolandwalker/anaphora") "request" (request :type git :flavor melpa :files ("request.el" "request-pkg.el") :host github :repo "tkf/emacs-request") "deferred" (deferred :type git :flavor melpa :files ("deferred.el" "deferred-pkg.el") :host github :repo "kiwanami/emacs-deferred") "polymode" (polymode :type git :flavor melpa :host github :repo "polymode/polymode") "pdf-tools" (pdf-tools :type git :flavor melpa :files ("lisp/*.el" "README" ("build" "Makefile") ("build" "server") (:exclude "lisp/tablist.el" "lisp/tablist-filter.el") "pdf-tools-pkg.el") :host github :repo "vedang/pdf-tools") "tablist" (tablist :type git :flavor melpa :host github :repo "politza/tablist") "yatex" nil "rainbow-mode" nil "migemo" (migemo :type git :flavor melpa :host github :repo "emacs-jp/migemo") "google-translate" (google-translate :type git :flavor melpa :host github :repo "atykhonov/google-translate") "avy" (avy :type git :flavor melpa :host github :repo "abo-abo/avy") "helm-descbinds" (helm-descbinds :type git :flavor melpa :host github :repo "emacs-helm/helm-descbinds") "helm-tramp" (helm-tramp :type git :flavor melpa :host github :repo "masasam/emacs-helm-tramp") "helm-ag" (helm-ag :type git :flavor melpa :host github :repo "emacsorphanage/helm-ag") "docker" (docker :type git :flavor melpa :host github :repo "Silex/docker.el") "docker-tramp" (docker-tramp :type git :flavor melpa :host github :repo "emacs-pe/docker-tramp.el") "json-mode" (json-mode :type git :flavor melpa :host github :repo "joshwnj/json-mode") "json-snatcher" (json-snatcher :type git :flavor melpa :host github :repo "Sterlingg/json-snatcher") "hydra" (hydra :type git :flavor melpa :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :host github :repo "abo-abo/hydra") "lv" (lv :type git :flavor melpa :files ("lv.el" "lv-pkg.el") :host github :repo "abo-abo/hydra") "yapfify" (yapfify :type git :flavor melpa :host github :repo "JorisE/yapfify") "py-isort" (py-isort :type git :flavor melpa :host github :repo "paetzke/py-isort.el") "ace-window" (ace-window :type git :flavor melpa :host github :repo "abo-abo/ace-window") "which-key" (which-key :type git :flavor melpa :host github :repo "justbur/emacs-which-key") "package-lint" (package-lint :type git :flavor melpa :files (:defaults "data" (:exclude "*flymake.el") "package-lint-pkg.el") :host github :repo "purcell/package-lint") "lsp-mode" (lsp-mode :type git :flavor melpa :files (:defaults "clients/*.el" "lsp-mode-pkg.el") :host github :repo "emacs-lsp/lsp-mode") "f" (f :type git :flavor melpa :files ("f.el" "f-pkg.el") :host github :repo "rejeep/f.el") "ht" (ht :type git :flavor melpa :files ("ht.el" "ht-pkg.el") :host github :repo "Wilfred/ht.el") "spinner" nil "helm-lsp" (helm-lsp :type git :flavor melpa :host github :repo "emacs-lsp/helm-lsp") "lsp-docker+" nil "lsp-docker" (lsp-docker :type git :flavor melpa :host github :repo "emacs-lsp/lsp-docker") "lsp-ui" (lsp-ui :type git :flavor melpa :files (:defaults "lsp-ui-doc.html" "resources" "lsp-ui-pkg.el") :host github :repo "emacs-lsp/lsp-ui") "lsp-pyright" (lsp-pyright :type git :flavor melpa :host github :repo "emacs-lsp/lsp-pyright") "ccls" (ccls :type git :flavor melpa :host github :repo "emacs-lsp/emacs-ccls") "yasnippet" (yasnippet :type git :flavor melpa :files ("yasnippet.el" "snippets" "yasnippet-pkg.el") :host github :repo "joaotavora/yasnippet") "company" (company :type git :flavor melpa :files (:defaults "icons" "company-pkg.el") :host github :repo "company-mode/company-mode") "company-tabnine" (company-tabnine :type git :flavor melpa :host github :repo "TommyX12/company-tabnine") "unicode-escape" (unicode-escape :type git :flavor melpa :host github :repo "kosh04/unicode-escape.el") "names" (names :type git :flavor melpa :host github :repo "Malabarba/names") "dap-mode" (dap-mode :type git :flavor melpa :files (:defaults "icons" "dap-mode-pkg.el") :host github :repo "emacs-lsp/dap-mode") "bui" (bui :type git :flavor melpa :host github :repo "alezost/bui.el") "lsp-treemacs" (lsp-treemacs :type git :flavor melpa :files (:defaults "icons" "lsp-treemacs-pkg.el") :host github :repo "emacs-lsp/lsp-treemacs") "treemacs" (treemacs :type git :flavor melpa :files (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el" "src/scripts/treemacs*.py" (:exclude "src/extra/*") "treemacs-pkg.el") :host github :repo "Alexander-Miller/treemacs") "pfuture" (pfuture :type git :flavor melpa :host github :repo "Alexander-Miller/pfuture") "cfrs" (cfrs :type git :flavor melpa :host github :repo "Alexander-Miller/cfrs") "posframe" (posframe :type git :flavor melpa :host github :repo "tumashu/posframe") "srefactor" (srefactor :type git :flavor melpa :host github :repo "tuhdo/semantic-refactor") "autoinsert" nil "mozc" (mozc :type git :flavor melpa :files ("src/unix/emacs/mozc.el" "mozc-pkg.el") :host github :repo "google/mozc"))) gnu-elpa-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 3 "emacsmirror-mirror" nil "straight" nil "yk-util" nil "cl-lib" nil "col-highlight" nil "cursor-chg" nil "csv-mode" (csv-mode :type git :host github :repo "emacs-straight/csv-mode" :files ("*" (:exclude ".git"))) "let-alist" (let-alist :type git :host github :repo "emacs-straight/let-alist" :files ("*" (:exclude ".git"))) "seq" nil "flyspell" nil "undo-tree" (undo-tree :type git :host github :repo "emacs-straight/undo-tree" :files ("*" (:exclude ".git"))) "eldoc" (eldoc :type git :host github :repo "emacs-straight/eldoc" :files ("*" (:exclude ".git"))) "yatex" nil "rainbow-mode" (rainbow-mode :type git :host github :repo "emacs-straight/rainbow-mode" :files ("*" (:exclude ".git"))) "spinner" (spinner :type git :host github :repo "emacs-straight/spinner" :files ("*" (:exclude ".git"))) "lsp-docker+" nil "autoinsert" nil)) el-get #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 1 "emacsmirror-mirror" nil "straight" nil "yk-util" nil "cl-lib" nil "col-highlight" nil "cursor-chg" nil "seq" nil "flyspell" nil "yatex" nil "lsp-docker+" nil "autoinsert" nil)) emacsmirror-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "straight" (straight :type git :host github :repo "emacsmirror/straight") "yk-util" nil "cl-lib" nil "col-highlight" (col-highlight :type git :host github :repo "emacsmirror/col-highlight") "cursor-chg" (cursor-chg :type git :host github :repo "emacsmirror/cursor-chg") "seq" nil "flyspell" nil "yatex" (yatex :type git :host github :repo "emacsmirror/yatex") "lsp-docker+" nil "autoinsert" nil))))

("org-elpa" "melpa" "gnu-elpa-mirror" "el-get" "emacsmirror-mirror" "straight" "emacs" "use-package" "bind-key" "yk-util" "dash" "init-loader" "cl-lib" "zenburn-theme" "col-highlight" "vline" "powerline" "total-lines" "cursor-chg" "magit" "git-commit" "transient" "with-editor" "magit-section" "helm-ls-git" "helm" "async" "popup" "helm-core" "git-gutter" "projectile" "helm-projectile" "org" "open-junk-file" "lispxmp" "paredit" "auto-async-byte-compile" "powershell" "dockerfile-mode" "docker-compose-mode" "yaml-mode" "cmake-mode" "protobuf-mode" "csv-mode" "go-mode" "typescript-mode" "cuda-mode" "rust-mode" "cargo" "markdown-mode" "flycheck-rust" "flycheck" "pkg-info" "epl" "let-alist" "seq" "web-mode" "prettier-js" "glsl-mode" "clang-format+" "clang-format" "rainbow-delimiters" "flyspell" "ag" "s" "wgrep-ag" "wgrep" "undo-tree" "buffer-move" "fast-scroll" "edit-server" "multi-term" "gxref" "eldoc" "ein" "websocket" "anaphora" "request" "deferred" "polymode" "pdf-tools" "tablist" "yatex" "rainbow-mode" "mozc" "migemo" "google-translate" "avy" "helm-descbinds" "helm-tramp" "helm-ag" "docker" "docker-tramp" "json-mode" "json-snatcher" "hydra" "lv" "yapfify" "py-isort" "ace-window" "which-key" "package-lint" "lsp-mode" "f" "ht" "spinner" "helm-lsp" "lsp-docker+" "lsp-docker" "lsp-ui" "lsp-pyright" "ccls" "yasnippet" "company" "company-tabnine" "unicode-escape" "names" "dap-mode" "bui" "lsp-treemacs" "treemacs" "pfuture" "cfrs" "posframe" "srefactor" "autoinsert")

t
