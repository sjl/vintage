.PHONY: vendor clean binary-sbcl binary-ccl binary

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# Clean -----------------------------------------------------------------------
clean:
	rm -rf bin

# Build -----------------------------------------------------------------------
lisps := $(shell ffind '\.(asd|lisp)$$')
assets := $(shell find assets/ -type f)

bin:
	mkdir -p bin

binary-sbcl: bin
	sbcl --load "src/build.lisp"
	rm -f bin/vintage-sbcl
	mv vintage bin/vintage-sbcl

binary-ccl: bin
	ccl --load "src/build.lisp"
	rm -f bin/vintage-ccl
	mv vintage bin/vintage-ccl

bin/vintage-sbcl: $(lisps) $(assets) Makefile
	make binary-sbcl

bin/vintage-ccl: $(lisps) $(assets) Makefile
	make binary-ccl

bin/vintage: $(lisps) $(assets) Makefile
	make binary-sbcl
	mv bin/vintage-sbcl bin/vintage

binary: binary-sbcl
	mv bin/vintage-sbcl bin/vintage

all: binary-sbcl binary-ccl
	cp bin/vintage-sbcl bin/vintage

# Deploy -----------------------------------------------------------------------
server-update-deps:
	hg  -R /home/sjl/lib/cl-losh   pull -v -u
	hg  -R /home/sjl/lib/cl-pcg    pull -v -u
	git -C /home/sjl/lib/cl-charms pull --ff-only origin dev

server-binary: binary
	rm -f /opt/vintage/vintage
	cp bin/vintage /opt/vintage/vintage

deploy:
	rsync --exclude=bin --exclude=.hg --exclude '*.fasl' -avz . jam:/home/sjl/src/vintage
	ssh jam make -C /home/sjl/src/vintage server-update-deps server-binary
