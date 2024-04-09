##
## EPITECH PROJECT, 2024
## Wolfram
## File description:
## This is the makefile of
## the wolfram project
##

NAME 			= mypandoc
BUILD_PATH 		= $(shell stack path --local-install-root)

.PHONY: all clean fclean re tests_run

all: $(NAME)

$(NAME):
	@stack build
	@cp $(BUILD_PATH)/bin/$(NAME)-exe $(NAME)

clean:
	@stack clean

fclean: clean
	@rm -f $(NAME)

re: fclean all

tests_run: fclean
	@stack test --coverage
