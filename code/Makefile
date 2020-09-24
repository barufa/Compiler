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
	  tigergraph.uo tigerflow.uo tigerliveness.uo tigercolor.uo

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
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigersimpleregalloc.ui: tigerframe.ui tigerassem.uo 
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo 
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigerpp.uo: tigerabs.uo 
tigerflow.uo: tigerflow.ui tigergraph.ui tigerassem.uo tigertemp.ui 
tigerflow.ui: tigergraph.ui tigerassem.uo tigertemp.ui 
tigerliveness.uo: tigerliveness.ui tigergraph.ui tigertemp.ui tigerflow.ui 
tigertree.uo: tigertemp.ui 
tigermuestratipos.uo: tigermuestratipos.ui tigertips.uo 
tigergrm.ui: tigerabs.uo 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigercodegen.uo: tigercodegen.ui tigertree.uo tigerframe.ui tigerassem.uo \
    tigertemp.ui 
tigerframe.uo: tigerframe.ui tigertree.uo tigerassem.uo tigertemp.ui 
tigercanon.ui: tigertree.uo tigertemp.ui 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigercodegen.ui: tigertree.uo tigerframe.ui tigerassem.uo 
tigerregalloc.ui: tigerframe.ui tigerassem.uo tigertemp.ui 
tigerpila.uo: tigerpila.ui 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigertemp.ui 
tigergraph.uo: tigergraph.ui tigerassem.uo 
tigerit.uo: tigertree.uo tigertab.ui 
tigerliveness.ui: tigergraph.ui tigertemp.ui tigerflow.ui 
tigercolor.uo: tigercolor.ui tigergraph.ui tigerpila.ui tigerframe.ui \
    tigerassem.uo tigertemp.ui tigerliveness.ui tigerflow.ui 
tigergraph.ui: tigerassem.uo 
tigerseman.ui: tigerabs.uo 
tigerregalloc.uo: tigerregalloc.ui tigerframe.ui tigertemp.ui 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigerinterp.uo: tigertree.uo tigertab.ui tigerframe.ui tigerit.uo \
    tigertemp.ui 
tigerescap.ui: tigerabs.uo 
tigermain.uo: tigerseman.ui tigercodegen.ui tigerescap.ui tigergrm.ui \
    tigerframe.ui tigerit.uo tigercanon.ui tigerassem.uo tigerinterp.uo \
    tigerlex.uo tigertrans.ui tigerpp.uo 
tigerassem.uo: tigertemp.ui 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigertopsort.ui tigerabs.uo tigertrans.ui 
tigercolor.ui: tigerframe.ui tigerassem.uo tigertemp.ui 
tigertab.uo: tigertab.ui 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigermuestratipos.ui: tigertips.uo 
tigertemp.uo: tigertemp.ui 
tigersimpleregalloc.uo: tigersimpleregalloc.ui tigerassem.uo 
tigerframe.ui: tigertree.uo tigerassem.uo tigertemp.ui 
