.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

#MOdules
MODS = autocomplete_clt message test

all: compile

compile: ${MODS:%=%.beam} subdirs

#Subdirs

subdirs:
	cd json-eep; make

clean:
	rm -f *.beam
	cd json-eep; make clean 
