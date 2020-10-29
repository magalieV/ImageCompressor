##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile of the project
##

NAME	=	imageCompressor

RM	=	rm -rf

all: $(NAME)

$(NAME):
	stack build
	cp `stack path --local-install-root`/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)
	$(RM) imageCompressor.cabal
	$(RM) .stack-work

re: fclean all

.PHONY	:	all clean fclean re
