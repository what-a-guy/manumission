PROG=manumission
VERSION=0.4
OBJS=${PROG}.o
SRCS=${OBJS:%.o=%.scm}
BGFLAGS=-g2
SAMPLES:=$(shell dir sample)
SAMPLE_FILES:=${SAMPLES:%=sample/%}
all: .afile ${PROG}
	./${PROG} -i sample -o sample.chm -d index.html

${PROG}: ${OBJS}
	bigloo ${BGFLAGS} -o $@ $^

%.o: %.scm
	bigloo ${BGFLAGS} -c -o $@ $^

.afile: ${SRCS}
	bglafile $^ > $@

tgz: $(PROG)-$(VERSION).tgz

$(PROG)-$(VERSION).tgz: COPYING INSTALL Makefile NEWS $(SRCS)
	rm -fr tmp
	mkdir -p tmp/manumission/sample
	cp $^ tmp/manumission
	cp $(SAMPLE_FILES) tmp/manumission/sample
	tar czpf $@ -C tmp ${^:%=manumission/%} ${SAMPLE_FILES:%=manumission/%}

clean:
	rm -f $(PROG) $(OBJS) .afile sample.chm
