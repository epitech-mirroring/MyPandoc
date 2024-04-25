##
## EPITECH PROJECT, 2024
## MicroTekSpice
## File description:
## Makefile
##

## Config
NAME 			= 	mypandoc

STACK_WORK		= 	.stack-work/*
STACK_BUILD		= 	.stack-work/dist/*
STACK_PATH		= 	$(shell stack path --local-install-root)
STACK_BIN		= 	$(STACK_PATH)/bin/${NAME}-exe

# Colors and formatting
GREEN =		\033[1;32m
YELLOW =	\033[1;33m
RED =		\033[1;31m
BLUE =		\033[1;36m
GOLD =		\033[1;33m
MAGENTA =	\033[1;35m
RESET =		\033[0m

RUNNING = [$(YELLOW)~$(RESET)]
SUCCESS = [$(GREEN)✔$(RESET)]
FAILURE = [$(RED)✘$(RESET)]
SKIPPED = [$(MAGENTA)@$(RESET)]

## Rules
all:
	@printf "$(RUNNING)$(BLUE) 🚧  Building mypandoc$(RESET)\n";
	@stack build 2> /tmp/mypandocbuild.log \
	&& printf \
	"$(SUCCESS)$(GREEN) 🚀  Build successfully mypandoc$(RESET)\n" \
	|| (printf "$(FAILURE)$(RED) 🚨  Build failed!\
	$(RESET)\n" && cat /tmp/mypandocbuild.log && false);
	@printf "$(RUNNING)$(BLUE) 📦  Moving built binary (${NAME})$(RESET)\n";
	@cp $(STACK_BIN) $(NAME)
	@printf "$(SUCCESS)$(GREEN) 📦  Moved binary successfully$(RESET)\n";

clean:
	@stack clean
	@printf "$(RUNNING)$(RED) 🗑️   Cleaning all stack files$(RESET)\n";

fclean: clean
	@rm -fr $(NAME)
	@printf "$(RUNNING)$(RED) 🗑️   Fcleaning mypandoc$(RESET)\n";


log:
	@cat /tmp/mypandocbuild.log

test_run:
	@stack test --coverage
	@printf "$(SUCCESS)$(GREEN) 🎉   Tests passed successfully$(RESET)\n";

re: fclean all
