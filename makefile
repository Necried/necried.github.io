ORG_ODIR=assets/content
ORG_INDIR=assets/org
ORGFILES=$(patsubst $(ORG_INDIR)/%.org,$(ORG_ODIR)/%.md,$(wildcard $(ORG_INDIR)/*.org))

.PHONY: clean all

all: setup org hask index.js launch

setup: ~/TODO/resources.org
	cp ~/TODO/resources.org $(ORG_INDIR)
	cp assets/templates/Reads.elm src

org: $(ORG_ODIR) $(ORGFILES)

hask:
	ghc -o assets/templates/Process assets/templates/Process.hs
	./assets/templates/Process

$(ORG_ODIR):
	mkdir -v -p $(ORG_ODIR)

$(ORG_INDIR)/%.md: $(ORG_INDIR)/%.org
	emacs $< --batch -f org-md-export-to-markdown --kill

$(ORG_ODIR)/%.md: $(ORG_INDIR)/%.md
	install -v -m 644 $< $(ORG_ODIR)
	rm $<

index.js: $(shell find src -type f | grep elm)  
	elm make src/Main.elm --output=index.js

launch:
	open http://localhost:8000/index.html

stop:
	killall elm

# clean all generated files
clean: elmclean
	rm index.js
	(cd assets/templates; rm Process *.o *.hi)

# clean the elm-stuff directory	
elmclean:
	rm -r elm-stuff/
	rm src/Reads.elm
# debug
print-%  : ; @echo $* = $($*)
