elm: $(shell find elm -type f | grep elm) elm/Main.elm 
	elm make --output=index.js elm/Main.elm

org:
	emacs assets/content/Interests.org --batch -f org-md-export-to-md --kill
