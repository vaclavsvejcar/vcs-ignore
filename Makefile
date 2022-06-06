.PHONY: clean
clean:
	rm -rf ./.stack-work/
	rm -rf ./dist-newstyle

.PHONY: pretty
pretty: 
	find ./app -name '*.hs' | xargs fourmolu -i
	find ./src -name '*.hs' | xargs fourmolu -i
	find ./test -name '*.hs' | xargs fourmolu -i

.PHONY: fresh
fresh: clean build

.PHONY: headroom
headroom:
	headroom run -c

.PHONY: hlint
hlint:
	hlint ./app
	hlint ./src
	hlint ./test

.PHONY: build
build: hlint headroom pretty
	stack test
	stack haddock