##
## EPITECH PROJECT, 2020
## Doop
## File description:
## Makefile for the doop
##

NAME		=	imageCompressor

WORK		=	.stack-work

PATH_EXEC	=	.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/imageCompressor/imageCompressor

STACK		=	stack.yaml.lock

CABAL		=	imageCompressor.cabal

RM		=	rm -fr

all:		$(NAME)

$(NAME):
		@stack build
		@printf "[\033[0;33mBuild\033[0m] % 32s\n" $(NAME) | tr ' ' '.'
		@cp $(PATH_EXEC) .

clean:
		@printf "[\033[0;31mDeleted\033[0m] % 30s\n" $(WORK) | tr ' ' '.'
		@printf "[\033[0;31mDeleted\033[0m] % 30s\n" $(STACK) | tr ' ' '.'
		@printf "[\033[0;31mDeleted\033[0m] % 30s\n" $(CABAL) | tr ' ' '.'
		@$(RM) $(CABAL)
		@$(RM) $(STACK)
		@$(RM) $(WORK)

tests_run:
		@stack test --coverage

fclean: 	clean
		@$(RM) $(NAME)
		@printf "[\033[0;31mDeleted\033[0m] % 30s\n" $(NAME) | tr ' ' '.'

re:		fclean all

.PHONY:	all clean fclean re
