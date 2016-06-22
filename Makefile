SOURCES = opal.ml
RESULT = opal
LIBINSTALL_FILES = META opal.cma opal.cmxa opal.cmi opal.a

all: ncl bcl

-include OCamlMakefile
