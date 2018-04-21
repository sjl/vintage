.PHONY: vendor clean binary-sbcl binary-ccl

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

