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
	stack clean
	stack test


.PHONY: functionnal-tests
functionnal-tests: re
	vangelis ./test/tests.toml --diff


.PHONY: coverage
coverage:
	stack test --coverage


# -------------------------------------------------------------------------- #
