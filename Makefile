stack-all:
	stack --resolver nightly build $(FLAGS)
	@echo
	stack --resolver lts build $(FLAGS)
	@echo
	stack --resolver lts-14 build $(FLAGS)
	@echo
	stack --resolver lts-13 build $(FLAGS)
	@echo
	stack --resolver lts-12 build $(FLAGS)
	@echo
	stack --resolver lts-11 build $(FLAGS)
	@echo
	stack --resolver lts-10 build $(FLAGS)
	@echo
	stack --resolver lts-9 build $(FLAGS)
	@echo
	stack --resolver lts-8 build $(FLAGS)
	@echo
	stack --resolver lts-6 build $(FLAGS)
