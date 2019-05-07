ORG_ODIR=assets/content
ORG_INDIR=assets/org
ORGFILES=$(patsubst $(ORG_INDIR)/%.org,$(ORG_ODIR)/%.md,$(wildcard $(ORG_INDIR)/*.org))

.PHONY: clean all

all: setup org index.js

setup: ~/TODO/resources.org
	cp ~/TODO/resources.org $(ORG_INDIR)

org: $(ORG_ODIR) $(ORGFILES)

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
	cd build && elm reactor &

sa:
	killall elm || true
	open http://localhost:8000/src/${n}/Static/Standalone/${m}.elm && elm reactor
	#elm make src/Static/Standalone/${m}.elm --output=build/Standalone${m}.html
	#open build/Standalone${m}.html

stop:
	killall elm

# clean all generated files
clean:
	rm index.js
	rm $(ORG_ODIR)/*.md

# clean the elm-stuff directory	
elmclean:
	rm -r elm-stuff/

# debug
print-%  : ; @echo $* = $($*)
