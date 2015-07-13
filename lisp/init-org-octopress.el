(require-package 'org-octopress)
(require 'org-octopress)
(setq org-octopress-directory-top       "~/Programing/octopress/source")
(setq org-octopress-directory-posts     "~/Programing/octopress/source/_posts")
(setq org-octopress-directory-org-top   "~/Programing/octopress/source")
(setq org-octopress-directory-org-posts "~/Programing/octopress/source/blog")
;; copy setupfile-sample.ort in https:////github.com/yoshinari-nomura/org-octopress to setupfile.org
(setq org-octopress-setup-file          "~/org_sty/setupfile.org")

(provide 'init-org-octopress)