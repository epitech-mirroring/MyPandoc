##
## EPITECH PROJECT, 2024
## MyPandoc
## File description:
## Makefile
##

## Config
NAME 			= 	mypandoc_bonus

STACK_NAME		= 	mypandoc
STACK_PATH		= 	$(shell stack path --local-install-root)
STACK_BIN		= 	$(STACK_PATH)/bin/${STACK_NAME}-exe

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
	@printf "$(RUNNING)$(BLUE) 🚧  Building mypandoc$(RESET)\r";
	@stack build 2> /tmp/mypandocbuild.log \
	&& printf \
	"$(SUCCESS)$(GREEN) 🚀  Build successfully mypandoc$(RESET)\n" \
	|| (printf "$(FAILURE)$(RED) 🚨  Build failed!\
	$(RESET)\n" && cat /tmp/mypandocbuild.log && false);
	@printf \
	"$(RUNNING)$(BLUE) 📦  Moving built binary (${NAME})$(RESET)    \r";
	@cp $(STACK_BIN) $(NAME) &&  \
	printf \
	"$(SUCCESS)$(GREEN) 📦  Moved binary successfully$(RESET)       \n" || \
	printf \
	"$(FAILURE)$(RED) 🚨  \Failed to move binary$(RESET)            \n";

clean:
	@stack clean
	@printf "$(RUNNING)$(RED) 🗑️   Cleaning all stack files$(RESET)\n";

fclean: clean
	@rm -fr $(NAME)
	@printf "$(RUNNING)$(RED) 🗑️   Fcleaning mypandoc$(RESET)\n";

log:
	@cat /tmp/mypandocbuild.log

tests_run:
	@stack test --coverage
	@printf "$(SUCCESS)$(GREEN) 🎉   Tests passed successfully$(RESET)\n";
	@stack hpc report --all --destdir=test/coverage
	@printf "$(RUNNING)$(BLUE) 📊  Generating coverage report$(RESET)\n";

re: fclean all
