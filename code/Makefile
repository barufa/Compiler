# Unix makefile for tigermain example

HOME=$(shell pwd)
MOSMLHOME=${HOME}/../mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac -v

GCC=gcc
CFLAGS= -g
MOSMLC=mosmlc -c -liberal
MOSMLL=mosmlc

# Unix
REMOVE=rm -f
MOVE=mv
EXEFILE=

.SUFFIXES : .sig .sml .ui .uo

GRALOBJS= tigerabs.uo tigergrm.uo tigerlex.uo tigernlin.uo tigerinterp.uo tigercanon.uo\
	  tigerpp.uo tigerescap.uo tigertab.uo tigerseman.uo tigertemp.uo tigertopsort.uo tigertrans.uo\
	  tigerassem.uo tigercodegen.uo tigermain.uo tigertree.uo tigerframe.uo tigerit.uo tigerpila.uo

all: depend tiger

tiger: $(GRALOBJS) $(OBJSGEN)
	$(MOSMLL) -o tiger $(EXEFILE) tigermain.uo

tigergrm.sml tigergrm.sig: tigergrm.y
	$(MOSMLYACC) tigergrm.y

tigerlex.sml: tigerlex.lex
	$(MOSMLLEX) tigerlex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) tigergrm.output
	$(REMOVE) tigergrm.sig
	$(REMOVE) tigergrm.sml
	$(REMOVE) tigerlex.sml
	$(REMOVE) tigermain
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) errlist
	$(REMOVE) *.o
	$(REMOVE) tiger

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: tigerabs.sml tigergrm.sml tigerlex.sml tigermain.sml \
	tigerassem.sml tigernlin.sml tigerpp.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigermakegraph.uo: tigermakegraph.ui 
tigerassem.uo: tigertemp.ui 
tigermain.uo: tigerseman.ui tigercodegen.ui tigerescap.ui tigergrm.ui \
    tigerframe.ui tigerit.uo tigercanon.ui tigerassem.uo tigerinterp.uo \
    tigerlex.uo tigertrans.ui tigerpp.uo 
tigermakegraph.ui: tigerassem.uo tigerflow.ui 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigertemp.ui 
tigersimpleregalloc.uo: tigersimpleregalloc.ui tigerassem.uo 
tigerliveness.uo: tigerliveness.ui 
tigerflow.uo: tigerflow.ui 
tigermuestratipos.ui: tigertips.uo 
tigertree.uo: tigertemp.ui 
tigersimpleregalloc.ui: tigerframe.ui tigerassem.uo 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigerseman.ui: tigerabs.uo 
tigertemp.uo: tigertemp.ui 
tigerit.uo: tigertree.uo tigertab.ui 
tigercanon.ui: tigertree.uo tigertemp.ui 
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo 
tigerframe.uo: tigerframe.ui tigertree.uo tigerassem.uo tigertemp.ui 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigercodegen.ui: tigertree.uo tigerframe.ui tigerassem.uo 
tigerflow.ui: tigergraph.ui tigertemp.ui 
tigercodegen.uo: tigercodegen.ui tigertree.uo tigerframe.ui tigerassem.uo \
    tigertemp.ui 
tigergrm.ui: tigerabs.uo 
tigergraph.uo: tigergraph.ui 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigerliveness.ui: tigergraph.ui tigertemp.ui tigerflow.ui 
tigertab.uo: tigertab.ui 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigertopsort.ui tigerabs.uo tigertrans.ui 
tigerpp.uo: tigerabs.uo 
tigerinterp.uo: tigertree.uo tigertab.ui tigerframe.ui tigerit.uo \
    tigertemp.ui 
tigerpila.uo: tigerpila.ui 
tigermuestratipos.uo: tigermuestratipos.ui tigertips.uo 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigerescap.ui: tigerabs.uo 
tigerframe.ui: tigertree.uo tigerassem.uo tigertemp.ui 
