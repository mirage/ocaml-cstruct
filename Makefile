OS ?= unix

ifneq "$(MIRAGE_OS)" ""
OS := $(MIRAGE_OS)
endif

.PHONY: all clean install test
.DEFAULT: all

all:
	cd $(OS) && $(MAKE) all
clean:
	cd $(OS) && $(MAKE) clean
install:
	cd $(OS) && $(MAKE) install
reinstall:
	cd $(OS) && $(MAKE) reinstall
test:
	cd $(OS) && $(MAKE) test
