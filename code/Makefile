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
	  tigerassem.uo tigercodegen.uo tigermain.uo tigertree.uo tigerframe.uo tigerit.uo tigerpila.uo\
	  tigergraph.uo tigerflow.uo tigerliveness.uo

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
tigergrm.ui: tigerabs.uo 
tigermain.uo: tigerseman.ui tigercodegen.ui tigerescap.ui tigergrm.ui \
    tigerframe.ui tigerit.uo tigercanon.ui tigerassem.uo tigerinterp.uo \
    tigerlex.uo tigertrans.ui tigerpp.uo 
tigercolor.uo: tigercolor.ui tigergraph.ui tigerpila.ui tigerframe.ui \
    tigertemp.ui tigerliveness.ui tigerflow.ui 
tigerliveness.ui: tigergraph.ui tigertemp.ui tigerflow.ui 
tigergraph.uo: tigergraph.ui tigerassem.uo 
tigerflow.uo: tigerflow.ui tigergraph.ui tigerassem.uo tigertemp.ui 
tigerassem.uo: tigertemp.ui 
tigercanon.ui: tigertree.uo tigertemp.ui 
tigerregalloc.uo: tigerregalloc.ui tigerframe.ui tigertemp.ui 
tigertab.uo: tigertab.ui 
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo 
tigerpila.uo: tigerpila.ui 
tigertemp.uo: tigertemp.ui 
tigerflow.ui: tigergraph.ui tigerassem.uo tigertemp.ui 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigermuestratipos.ui: tigertips.uo 
tigersimpleregalloc.uo: tigersimpleregalloc.ui tigerassem.uo 
tigerliveness.uo: tigerliveness.ui tigergraph.ui tigertemp.ui tigerflow.ui 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigerit.uo: tigertree.uo tigertab.ui 
tigerinterp.uo: tigertree.uo tigertab.ui tigerframe.ui tigerit.uo \
    tigertemp.ui 
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigersimpleregalloc.ui: tigerframe.ui tigerassem.uo 
tigerpp.uo: tigerabs.uo 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigertopsort.ui tigerabs.uo tigertrans.ui 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigerregalloc.ui: tigerframe.ui tigerassem.uo tigertemp.ui 
tigerframe.ui: tigertree.uo tigerassem.uo tigertemp.ui 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigercodegen.uo: tigercodegen.ui tigertree.uo tigerframe.ui tigerassem.uo \
    tigertemp.ui 
tigertree.uo: tigertemp.ui 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigergraph.ui: tigerassem.uo 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigertemp.ui 
tigerescap.ui: tigerabs.uo 
tigercolor.ui: tigerframe.ui tigerassem.uo tigertemp.ui 
tigercodegen.ui: tigertree.uo tigerframe.ui tigerassem.uo 
tigerseman.ui: tigerabs.uo 
tigermuestratipos.uo: tigermuestratipos.ui tigertips.uo 
tigerframe.uo: tigerframe.ui tigertree.uo tigerassem.uo tigertemp.ui 
