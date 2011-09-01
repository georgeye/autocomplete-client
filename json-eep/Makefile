.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

#MOdules
MODS = json_eep json_grammar json_lex2 

all: compile

compile: ${MODS:%=%.beam} 

#Subdirs


clean:
	rm -f *.beam

