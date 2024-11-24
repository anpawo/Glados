##
## EPITECH PROJECT, 2024
## glados
## File description:
## Makefile
##

# -------------------------------------------------------------------------- #


DIR		=	$(shell stack path --local-install-root)/bin/
NAME	=	glados

# -------------------------------------------------------------------------- #


.PHONY: all
all: $(NAME)


$(NAME):
	@ stack build
	@ cp $(DIR)$(NAME)-exe $(NAME)


.PHONY: clean
clean:


.PHONY: fclean
fclean:	clean
	@ $(RM) $(NAME)


.PHONY: re
re: fclean
	$(MAKE) all


.PHONY: unit-tests
unit-tests:
	stack test


.PHONY: functionnal-tests
functionnal-tests:
	@ echo "not implemented yet"


.PHONY: coverage
coverage:
	stack test --coverage


# -------------------------------------------------------------------------- #
