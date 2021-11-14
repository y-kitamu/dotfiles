;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((python-mode
  . (
     (yapfify-executable . "docker run -i --rm ml_gpu_jupyter yapf")
     (lsp-docker+-server-id . pyright)
     (lsp-docker+-docker-server-id . pyr-docker)
     (lsp-docker+-server-command . "pyright-langserver --stdio")
     ;; (lsp-docker+-server-cmd-fn . lsp-docker+-exec-in-container)
     (lsp-docker+-image-id . "ml_gpu_jupyter")
     (lsp-docker+-container-name . "py-lsp-docker")
     (lsp-docker+-path-mappings . (("/home/kitamura/work/" . "/home/kitamura/work/")))
     ))
 (c++-mode
  . (
     (lsp-docker+-server-id . ccls)
     (lsp-docker+-docker-server-id . ccls-docker)
     (lsp-docker+-server-command . "ccls")
     (lsp-docker+-image-id . "cpp_engine")
     (lsp-docker+-container-name . "cpp-lsp-docker")
     (lsp-docker+-path-mappings . (("/home/kitamura/work/" . "/home/kitamura/work/")))
     ;; (lsp-docker+-server-cmd-fn . lsp-docker+-exec-in-container)
     )))
