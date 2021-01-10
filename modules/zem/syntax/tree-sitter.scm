(define-module (zem syntax tree-sitter)
  #:use-module ((zem api tree-sitter) #:prefix tsapi:)
  #:use-module (zem core buffer)
  #:use-module (zem core text-prop)
  #:use-module (ice-9 match)
  #:use-module (emacsy emacsy))

(define* (position->ts-point #:optional (pos (point)))
  (save-excursion
      (goto-char pos)
    (cons (line-number-at-pos) (current-column))))

(define (ts-before-change start end)
  (let ((start-byte (position-bytes start))
        (end-byte (position-bytes end))
        (start-point (position->ts-point start))
        (end-point (position->ts-point end)))
    (set! (local-var 'tree-sitter:before-change-point) (vector start-byte end-byte start-point end-point))))

(define (ts-after-change start end len)
  (match-let ((#(start-byte old-end-byte start-point old-end-point)
               (local-var 'tree-sitter:before-change-point)))
             (tsapi:tree-edit (local-var 'tree-sitter:tree)
                              start-byte
                              old-end-byte
                              (position-bytes end)
                              start-point
                              old-end-point
                              (position->ts-point end))))

(define (highlight-query-capture-mapper name)
  (cond
   ((string= name "keyword") 'keyword)
   ((string= name "number") 'number)
   ((string= name "string") 'string)
   ((string= name "operator") 'operator)
   ((or
     (string= name "type")
     (string= name "type.builtin")) 'type)
   ((string= name "function") 'function)
   (else 'text)))

(define (ensure-highlight-query)
  (unless (local-var 'tree-sitter:highlight-query)
    (set! (local-var 'tree-sitter:highlight-query)
          (tsapi:query-new (local-var 'tree-sitter:highlight-patterns)
                           highlight-query-capture-mapper)))
  (local-var 'tree-sitter:highlight-query))

(define-public (setup-buffer buffer)
  (with-buffer buffer
    (let* ((parser (tsapi:parser-new))
           (tree (tsapi:parser-parse-string parser (buffer-string))))
      (set! (local-var 'tree-sitter:parser) parser)
      (set! (local-var 'tree-sitter:tree) tree)
      (set! (local-var 'tree-sitter:query-cursor) (tsapi:query-cursor-new))
      (set! (local-var 'tree-sitter:highlight-patterns) tree-sitter-highlight-patterns-cpp)
      (set! (local-var 'tree-sitter:highlight-query) #f)
      (ensure-highlight-query)
      (highlight-region buffer (point-min) (point-max))
      (set! (local-var 'tree-sitter:before-change-point) #(0 0 (0 . 0) (0 . 0)))
      (add-hook! (local-var 'before-change-functions) ts-before-change)
      (add-hook! (local-var 'after-change-functions) ts-after-change))))

(define-public (update-buffer buffer)
  (with-buffer buffer
    (set! (local-var 'tree-sitter:tree)
          (tsapi:parser-parse-string (local-var 'tree-sitter:parser)
                                     (buffer-string)
                                     (local-var 'tree-sitter:tree)))))

(define (highlight-capture capture)
  (match capture
         (((start . end) . tag)
          (when (not (= start end))
            (put-text-property start end 'syntax tag)))))

(define-public (highlight-region buffer beg end)
  (update-buffer buffer)
  (with-buffer buffer
    (let ((cursor (local-var 'tree-sitter:query-cursor))
          (root (tsapi:tree-root-node (local-var 'tree-sitter:tree))))
      (tsapi:query-cursor-set-byte-range cursor
                                         (position-bytes beg)
                                         (position-bytes end))
      (let ((captures (tsapi:query-cursor-captures
                       cursor
                       (local-var 'tree-sitter:highlight-query)
                       root)))
        (for-each highlight-capture captures)))))

(define tree-sitter-highlight-patterns-cpp
"[\"break\"
 \"case\"
 \"const\"
 \"continue\"
 \"default\"
 \"do\"
 \"else\"
 \"enum\"
 \"extern\"
 \"for\"
 \"if\"
 \"inline\"
 \"return\"
 \"sizeof\"
 \"static\"
 \"struct\"
 \"switch\"
 \"typedef\"
 \"union\"
 \"volatile\"
 \"while\"
 \"...\"] @keyword

[(storage_class_specifier)
 (type_qualifier)] @keyword

[\"#define\"
 \"#else\"
 \"#endif\"
 \"#if\"
 \"#ifdef\"
 \"#ifndef\"
 \"#include\"
 (preproc_directive)] @function.macro

(([\"#ifdef\" \"#ifndef\"] (identifier) @constant))

[\"+\" \"-\" \"*\" \"/\" \"%\"
 \"~\" \"|\" \"&\" \"<<\" \">>\"
 \"!\" \"||\" \"&&\"
 \"->\"
 \"==\" \"!=\" \"<\" \">\" \"<=\" \">=\"
 \"=\" \"+=\" \"-=\" \"*=\" \"/=\" \"%=\" \"|=\" \"&=\"
 \"++\" \"--\"
] @operator

(conditional_expression [\"?\" \":\"] @operator)

[\"(\" \")\" \"[\" \"]\" \"{\" \"}\"] @punctuation.bracket

[\".\" \",\" \";\"] @punctuation.delimiter

;;; ----------------------------------------------------------------------------
;;; Functions.

(call_expression
 function: [(identifier) @function.call
            (field_expression field: (_) @method.call)])

(function_declarator
 declarator: [(identifier) @function
              (parenthesized_declarator
               (pointer_declarator (field_identifier) @function))])

(preproc_function_def
 name: (identifier) @function)

;;; ----------------------------------------------------------------------------
;;; Types.

[(primitive_type)
 (sized_type_specifier)] @type.builtin

(type_identifier) @type

;;; ----------------------------------------------------------------------------
;;; Variables.

(declaration declarator: [(identifier) @variable
                          (_ (identifier) @variable)])

(parameter_declaration declarator: [(identifier) @variable.parameter
                                    (_ (identifier) @variable.parameter)])

(init_declarator declarator: [(identifier) @variable
                              (_ (identifier) @variable)])

(assignment_expression
 left: [(identifier) @variable
        (field_expression field: (_) @variable)
        (subscript_expression argument: (identifier) @variable)
        (pointer_expression (identifier) @variable)])

(update_expression
 argument: (identifier) @variable)

(preproc_def name: (identifier) @variable.special)

(preproc_params
 (identifier) @variable.parameter)

;;; ----------------------------------------------------------------------------
;;; Properties.

(field_declaration
 declarator: [(field_identifier) @property.definition
              (pointer_declarator (field_identifier) @property.definition)
              (pointer_declarator (pointer_declarator (field_identifier) @property.definition))])

(enumerator name: (identifier) @property.definition)

(field_identifier) @property

;;; ----------------------------------------------------------------------------
;;; Misc.

((identifier) @constant
 (.match? @constant \"^[A-Z_][A-Z_\\d]*$\"))

[(null) (true) (false)] @constant.builtin

[(number_literal)
 (char_literal)] @number

(statement_identifier) @label

;;; ----------------------------------------------------------------------------
;;; Strings and comments.

(comment) @comment

[(string_literal)
 (system_lib_string)] @string")


