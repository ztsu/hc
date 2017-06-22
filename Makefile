.PHONY: build

build:
	elm make ./src/Main.elm --output ./index.js
