ORG_ODIR=assets/content
ORG_INDIR=assets/org
ORGFILES=$(patsubst $(ORG_INDIR)/%.org,$(ORG_ODIR)/%.md,$(wildcard $(ORG_INDIR)/*.org))

js="index.js"
min="index.min.js"

.PHONY: clean all

all: setup org hask index.js

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
	set -e

	js=$(js)
	min=$(min)
	elm make --optimize src/Main.elm --output=$(js)
	uglifyjs $(js) --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output=$(min)

	echo "Initial size: $(cat $js | wc -c) bytes  ($js)"
	echo "Minified size:$(cat $min | wc -c) bytes  ($min)"
	echo "Gzipped size: $(cat $min | gzip -c | wc -c) bytes"

	rm $(js)
	mv $(min) $(js)


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
