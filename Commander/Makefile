LZMAs = $(shell ls */*.bmp.lzma)
BMPs = $(patsubst %.bmp.lzma,%.bmp,$(LZMAs))

all: $(BMPs)
	md5sum -c commander-spaceships.md5

%.bmp: %.bmp.lzma
	lzcat $< > $@

clean:
	$(RM) */*.bmp
