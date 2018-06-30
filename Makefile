.PHONY: default build install uninstall clean

default: build

CONFIGS := src/server/config.ml src/key-gen/config.ml src/kknotify/config.ml

$(CONFIGS): config.ml
	@echo "set $@"
	@echo "(* generated by Makefile *)" > $@
	@cat config.ml >> $@

build: $(CONFIGS)
	jbuilder build

install: build
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean
	git clean -dfXq
