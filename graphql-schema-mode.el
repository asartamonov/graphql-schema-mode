;;; graphql-schema-mode.el --- is a major mode to highlight Graphql files. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright ©asartamonov 2023

;;; Commentary:
;;; functionality: syntax highlighting (keywords, types, directives)
;;;                imenu integration: entities
;;;                comments collapsing for better readability
;;;                xref integration (auto tagfile build, rebuild)
;;;                templating: docstring, type, interface
;;;                flycheck integration: type implements interface, naming conventions
;;;        

;;; Code:
(require 'etags)
(require 'semantic/symref/grep)
(require 'vc)
(require 's)

(defgroup graphql-schema nil
  "Graphql schema settings."
  :group 'tools)

(defcustom imenu-groups '((enum . "*Misc*")
                          (scalar . "*Misc*")
                          (directive . "*Misc*")
                          (fragment . "*Misc*"))
  "Key-value pairs for imenu grouping.
The GraphQL type system supports the following kind of entities and keywords:

| GraphQL type entity    | keyword in schema |
|------------------------+-------------------|
| Scalar                 | scalar            |
| Object                 | type              |
| Interface              | interface         |
| Union                  | union             |
| InputObject            | input             |
| Enum                   | enum              |
|------------------------+-------------------|
| other GraphQL entities |                   |
|------------------------+-------------------|
| Directive              | directive         |
| Fragment               | fragment          |

Keys in this customization group are keywords and values
are desired names for items groups displayed by the imenu.
Multiple keys corresponding to the same group name supported."
  :type '(alist
          :key-type symbol
          :value-type string))

(defcustom schema-file-extensions '(".graphqls")
  "Filetypes when the mode is automatically activated, example: .graphqls."
  :type '(repeat string))

(defcustom schema-tags-filename ".graphql-schema-tags"
  "Tags file name, conventionally named as TAGS."
  :type 'string)

(defcustom schema-locations '(
                              ;;("~/Projects/graphq" . (list "~/Projects/emacs/graphql"))
                              )
  "Schema files root locations.
Useful when your schema files located in several locations.
Set per project:
- key is project VC system root dir
- value is list of schema files root dirs
Default is nil, callers should consider root as dir containing opened file.
Зачем это нужно? Чтобы строить теги: при построении etags учитывает файлы
схем из всех локаций."
  :type '(alist
          :key-type (directory :tag "project-vc-root")
          :value-type (repeat (directory :tag "schema location dir"))))

(defvar-local gql--imenu-expr (let ((entity-match-regexp
                                    "^[[:blank:]]*\\(?:%s\\)[[:blank:]]+\\([[:alnum:]_@]+\\)"))
                               (list (list (alist-get 'type imenu-groups)
                                           (format entity-match-regexp "type") 1)
                                     (list (alist-get 'input imenu-groups)
                                           (format entity-match-regexp "input") 1)
                                     (list (alist-get 'union imenu-groups)
                                           (format entity-match-regexp "union") 1)
                                     (list (alist-get 'scalar imenu-groups)
                                           (format entity-match-regexp "scalar") 1)
                                     (list (alist-get 'directive imenu-groups)
                                           (format entity-match-regexp "directive") 1)
                                     (list (alist-get 'interface imenu-groups)
                                           (format entity-match-regexp "interface") 1))))

(defvar-local gql--keywords '("query" "mutation" "subscription"
                           "input" "type" "extend" "interface" "implements"
                           "enum" "fragment" "scalar" "union" "directive"))

(defvar-local gql--constants
  '("true" "false" "null"))

(defvar-local gql--special-signs
  '("&" "[" "]" "!"))

(defvar-local gql--fontification
  (let ((kword-regexp "\\(^\\|[[:blank:]]+\\)\\(%s\\)[[:blank:]]+[[:alpha:]]")
        (constant-regexp "^[[:blank:]]*[[:upper:]_]\\{2,\\}[[:blank:]]*")
        (type-regexp "[[:upper:]][[:alpha:]]+")
        (or-delimit-regexp "\\|"))
    (append
     (mapcar (lambda (k) (cons k '(2 font-lock-keyword-face)))
             (mapcar (lambda (s) (format kword-regexp s)) gql--keywords))
     (list
      (cons (mapconcat (lambda (s) (format "\\%s" s)) gql--special-signs or-delimit-regexp)
            font-lock-builtin-face)
      (cons (concat (regexp-opt gql--constants 'words) or-delimit-regexp constant-regexp)
            font-lock-constant-face)
      (cons type-regexp font-lock-type-face))))
  "Fontification settings for graphql mode.")

(define-derived-mode graphql-schema-mode prog-mode
  "Graphql schema mode: syntax highlighting and tools for work with schemas files."
  (setq font-lock-defaults '(gql--fontification))
  (setq-local comment-start "# ")
  (setq-local comment-skip-start "#+[ \t]*")
  (setq-local comment-end "")
  ; imenu integration
  (setq imenu-generic-expression gql--imenu-expr))

(defun gql--build-tags ()
  "Rebuild TAG file, use schema destination settings."
  (if (gql--tags-file-name)
      (let* ((etags-regexp "'/\\(^type\\|^enum\\|^interface\\|^scalar\\) \\([a-zA-Z]+\\)[ \\t\\n]*/\\2/'")
             (exts-regexp (mapconcat 'identity (or schema-file-extensions '(".graphqls")) "\\|"))
             (search-roots (mapconcat 'identity (gql--schema-search-path) " "))
             (find-template (format "find %s -type f -regex '.*\\(%s\\)'"  search-roots exts-regexp))
             (build-tags-cmd (format "%s | xargs etags -o %s --regex=%s" find-template (gql--tags-file-name) etags-regexp)))
        (setq-local gql--disp-buff-alist display-buffer-alist)
        (add-to-list 'display-buffer-alist '("*Async Shell Command*" . (display-buffer-no-window . nil)) )
        (async-shell-command build-tags-cmd)
        (setq display-buffer-alist gql--disp-buff-alist)
        )))

;(gql--build-tags)

(defun gql--curr-buffer-vc-root ()
  "Find vc root or nil if not tracked by vc."
  (if (and (vc-deduce-backend) buffer-file-name)
      (vc-call-backend (vc-responsible-backend (buffer-file-name)) 'root (buffer-file-name))
    nil))

(defun gql--schema-search-path ()
  "Return dir, root for schemas search.
If set via customize group: if one dir, return it,
if more and vc root is present, return vc root,
else - return current buffer file dir,
otherwise, return current user home dir."
  (let* ((vc-root (gql--curr-buffer-vc-root))
         (custom-locations (alist-get schema-locations vc-root nil nil 'equal))
         (curr-buf-dir (file-name-directory (buffer-file-name))))
    (cond (custom-locations custom-locations)
          (vc-root (list vc-root))
          (curr-buf-dir (list curr-buf-dir)))))
;(gql--schema-search-path)

(add-hook 'graphql-schema-mode-hook
          (lambda ()
            "Enable hs-minor-mode and customize for gql schemas"
            (let ((hs-start "\"")
                  (hs-end "\""))
              (add-to-list 'hs-special-modes-alist
                           (list 'graphql-schema-mode hs-start hs-end "#" 'forward-sexp nil))
              (hs-minor-mode)
              (add-hook 'after-save-hook 'gql--build-tags nil 'local)
              (add-to-list 'semantic-symref-filepattern-alist  (append '(graphql-schema-mode)
                                                                       (mapcar
                                                                        (lambda (ext)
                                                                          (if (not (s-starts-with? "*" ext))
                                                                              (concat "*" ext)))
                                                                        schema-file-extensions)))
              ;(add-to-list 'semantic-symref-filepattern-alist  '(graphql-schema-mode "*.graphqls"))
              (if (not (file-exists-p (gql--tags-file-name)))
                  (gql--build-tags))
              (setq tags-revert-without-query 1
                    tags-table-list (list (gql--tags-file-name))))))

(defun gql--tags-file-name()
  "Return file name for tags file.
If current buffer belongs to vc, return file in vc root
if no vc, return filename in current buffer dir, return nil otherwise."
  (if (gql--curr-buffer-vc-root)
      (concat (gql--curr-buffer-vc-root) schema-tags-filename)
    (if (buffer-file-name)
        (concat (file-name-directory (buffer-file-name)) schema-tags-filename))))

;;;###autoload
(dolist (file-ext schema-file-extensions)
  (add-to-list 'auto-mode-alist (cons file-ext 'graphql-schema-mode)))


(provide 'graphql-schema-mode)
;;; graphql-schema-mode.el ends here
