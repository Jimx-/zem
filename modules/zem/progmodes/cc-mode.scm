(define-module (zem progmodes cc-mode)
  #:use-module (zem core mode)
  #:use-module (zem progmodes prog-mode)
  #:use-module (zem syntax tree-sitter)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 regex)
  #:export (enter-c++-mode))

(load-extension "libguile-tree-sitter-cc" "init_tree_sitter_cc")

(define-derived-mode c++-mode prog-mode "C++")

(set! auto-mode-alist (cons `(,(make-regexp "\\.cpp\\'") . ,c++-mode) auto-mode-alist))

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

(add-hook! c++-mode-hook
           (colambda ()
                     (set! (local-var 'tree-sitter:language)
                           (tree-sitter-language-cpp))
                     (set! (local-var 'tree-sitter:highlight-patterns)
                           tree-sitter-highlight-patterns-cpp)
                     (tree-sitter-highlight-mode)))
