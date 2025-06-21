# Compiler and flags
OCAMLC = ocamlc
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
RM = rm -f

# Source files
LEXER_SRC = lexer.mll
PARSER_SRC = parser.mly
AST_SRC = ast.ml
TYPE_CHECKER_SRC = type_checker.ml
INTERPRETER_SRC = interpreter.ml
MAIN_SRC = main.ml

# Generated files
LEXER_GEN = lexer.ml
PARSER_GEN = parser.mli parser.ml

# Object files (order matters!)
OBJS = ast.cmo type_checker.cmo interpreter.cmo parser.cmo lexer.cmo main.cmo

# Final executable
EXEC = interpreter

.PHONY: all clean test

all: $(EXEC)

$(EXEC): $(OBJS)
	$(OCAMLC) -o $@ -I +str str.cma $(OBJS)

# Dependency rules
ast.cmo: ast.ml
	$(OCAMLC) -c $<

type_checker.cmo: type_checker.ml ast.cmo
	$(OCAMLC) -c $<

interpreter.cmo: interpreter.ml ast.cmo type_checker.cmo
	$(OCAMLC) -c $<

parser.cmi: parser.mli
	$(OCAMLC) -c $<

parser.cmo: parser.ml parser.cmi
	$(OCAMLC) -c $<

lexer.cmo: lexer.ml parser.cmi
	$(OCAMLC) -c $<

main.cmo: main.ml parser.cmi ast.cmo interpreter.cmo
	$(OCAMLC) -c $<

# Code generation rules
lexer.ml: $(LEXER_SRC)
	$(OCAMLLEX) $<

parser.mli parser.ml: $(PARSER_SRC) ast.cmo type_checker.cmo
	$(OCAMLYACC) $<
	sed -i.bak 's/(Lexing\.lexbuf -> token) -> Lexing\.lexbuf -> Ast\.command/(Lexing\.lexbuf -> token) -> Lexing\.lexbuf -> Ast\.command/' parser.mli

clean:
	$(RM) $(EXEC) *.cmo *.cmi $(LEXER_GEN) $(PARSER_GEN) *.bak

# Run with input file if it exists
test: $(EXEC)
	@if [ -f input.txt ]; then \
		./$(EXEC) input.txt; \
    else \
        ./$(EXEC); \
    fi