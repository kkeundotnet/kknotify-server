.PHONY: default build install uninstall clean

default: build

build:
	jbuilder build

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean
	git clean -dfXq

# TODO: Remove: this is just for testing
run: build
	_build/install/default/bin/kknotify_server

key_gen: build
	_build/install/default/bin/kknotify_key_gen > .key
