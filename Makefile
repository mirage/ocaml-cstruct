.PHONY: all clean

all:
	jbuilder build

clean:
	rm -rf _build *.install
