;; Tempel templates
;; See https://github.com/minad/tempel

fundamental-mode

(today (format-time-string "%Y-%m-%d"))

prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK ")

c-mode c++-mode :when (re-search-backward "^\\S-*$" (line-beginning-position) 'noerror)

;; Main function
(main "int main(int argc, char *argv[]) {" n> r> n> "return 0;" n "}" >)
(mainn "int main(void) {" n> r> n> "return 0;" n "}" >)

;; Include headers
(inc "#include <" (p "stdio.h" header) ">")
(inch "#include \"" (p "header.h" header) "\"")

;; Standard library includes
(incstdio "#include <stdio.h>")
(incstdlib "#include <stdlib.h>")
(incstring "#include <string.h>")
(incmath "#include <math.h>")
(inctime "#include <time.h>")
(incassert "#include <assert.h>")
(incstdbool "#include <stdbool.h>")
(incstdint "#include <stdint.h>")

;; Control structures
(if "if (" (p "condition") ") {" n> r> n "}" >)
(ife "if (" (p "condition") ") {" n> r> n "} else {" n> p n "}" >)
(elif "else if (" (p "condition") ") {" n> r> n "}" >)
(else "else {" n> r> n "}" >)

(for "for (" (p "int i = 0") "; " (p "i < n") "; " (p "i++") ") {" n> r> n "}" >)
(fori "for (int " (p "i") " = 0; " (s i) " < " (p "count") "; " (s i) "++) {" n> r> n "}" >)
(forr "for (int " (p "i") " = " (p "n") "; " (s i) " >= 0; " (s i) "--) {" n> r> n "}" >)

(while "while (" (p "condition") ") {" n> r> n "}" >)
(do "do {" n> r> n "} while (" (p "condition") ");" >)

(switch "switch (" (p "expression") ") {" n> "case " (p "value") ":" n> r> n> "break;" n "default:" n> "break;" n "}" >)
(case "case " (p "value") ":" n> r> n "break;" >)

;; Functions
(fn (p "void" return-type) " " (p "function_name") "(" (p "void") ") {" n> r> n "}" >)
(fns "static " (p "void" return-type) " " (p "function_name") "(" (p "void") ") {" n> r> n "}" >)

;; Struct and typedef
(st "struct " (p "name") " {" n> r> n "};" >)
(td "typedef " (p "type") " " (p "alias") ";")
(tds "typedef struct " (p "name") " {" n> r> n "} " (s name) ";" >)

;; Enum
(enum "enum " (p "name") " {" n> r> n "};" >)
(tenum "typedef enum " (p "name") " {" n> r> n "} " (s name) ";" >)

;; Memory management
(malloc (p "type") " *" (p "ptr") " = malloc(sizeof(" (s type) ") * " (p "count") ");")
(calloc (p "type") " *" (p "ptr") " = calloc(" (p "count") ", sizeof(" (s type) "));")
(realloc (p "ptr") " = realloc(" (s ptr) ", sizeof(" (p "type") ") * " (p "new_count") ");")
(free "free(" (p "ptr") ");")

;; Printf variants
(printf "printf(\"" (p "%d\\n") "\", " (p "var") ");")
(printfs "printf(\"%s\\n\", " (p "str") ");")
(fprintf "fprintf(stderr, \"" (p "Error: %s\\n") "\", " (p "msg") ");")

;; File I/O
(fopen "FILE *" (p "fp") " = fopen(\"" (p "filename") "\", \"" (p "r" mode) "\");")
(fclose "fclose(" (p "fp") ");")
(fread "fread(" (p "buffer") ", sizeof(" (p "type") "), " (p "count") ", " (p "fp") ");")
(fwrite "fwrite(" (p "buffer") ", sizeof(" (p "type") "), " (p "count") ", " (p "fp") ");")
(fprintf "fprintf(" (p "fp") ", \"" (p "%d\\n") "\", " (p "var") ");")
(fscanf "fscanf(" (p "fp") ", \"" (p "%d") "\", &" (p "var") ");")

;; Error handling
(perror "perror(\"" (p "error message") "\");")
(check "if (" (p "ptr") " == NULL) {" n> "perror(\"" (s ptr) "\");" n> "exit(EXIT_FAILURE);" n "}" >)
(assert "assert(" (p "condition") ");")

;; Comments and documentation
(comment "/*" n> "* " (p "description") n> "*/" >)
(commentl "// " (p "comment"))

;; Header guard
(guard "#ifndef " (p (upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))) "HEADER") "_H" n "#define " (s "HEADER") "_H" n n r n n "#endif /* " (s "HEADER") "_H */" n)

;; Pragma once (modern alternative)
(pragma "#pragma once" n n)

lisp-mode emacs-lisp-mode ;; Specify multiple modes

(lambda "(lambda (" p ")" n> r> ")")
(var "(defvar " p "\n  \"" p "\")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(group "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(alias "(defalias '" p " '" p ")")
(fun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(iflet "(if-let (" p ")" n> r> ")")
(whenlet "(when-let (" p ")" n> r> ")")
(iflet* "(if-let* (" p ")" n> r> ")")
(whenlet* "(when-let* (" p ")" n> r> ")")
(andlet* "(and-let* (" p ")" n> r> ")")
(cond "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(let "(let (" p ")" n> r> ")")
(let* "(let* (" p ")" n> r> ")")
(rec "(letrec (" p ")" n> r> ")")
(dotimes "(dotimes (" p ")" n> r> ")")
(dolist "(dolist (" p ")" n> r> ")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")
(while "(while " p n> r> ")")
(format "(format \"" p "\"" q ")")
(message "(message \"" p "\"" q ")")
(error "(error \"" p "\"" q ")")
(user-error "(user-error \"" p "\"" q ")")

text-mode

(box "┌─" (make-string (length str) ?─) "─┐" n
     "│ " (s str)                       " │" n
     "└─" (make-string (length str) ?─) "─┘" n)
(abox "+-" (make-string (length str) ?-) "-+" n
      "| " (s str)                       " |" n
      "+-" (make-string (length str) ?-) "-+" n)
(cut "--8<---------------cut here---------------start------------->8---" n r n
     "--8<---------------cut here---------------end--------------->8---" n)
(rot13 (p "plain text" text) n "----" n (rot13 text))
(calc (p "taylor(sin(x),x=0,3)" formula) n "----" n (format "%s" (calc-eval formula)))
(today (format-time-string "%Y-%m-%d"))

latex-mode

(begin "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")
(frac "\\frac{" p "}{" q "}")
(enumerate "\\begin{enumerate}\n\\item " r> n> "\\end{enumerate}")
(itemize "\\begin{itemize}\n\\item " r> n> "\\end{itemize}")

java-mode

(class "public class " (p (file-name-base (or (buffer-file-name) (buffer-name)))) " {" n> r> n "}")
(main "public static void main(String[] args) {" n> r> n "}")
(println "System.out.println(" q ");")

python-mode python-ts-mode

(class "class " (p "ClassName") ":" n> r>)
(def "def " (p "function_name") "(" p "):" n> "\"\"\"" p "\"\"\"" n> r>)
(for "for " (p "item") " in " (p "items") ":" n> r>)
(if "if " (p "condition") ":" n> r>)
(main "if __name__ == '__main__':" n> r>)

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
