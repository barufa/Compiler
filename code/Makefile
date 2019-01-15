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

# DOS
#REMOVE=del
#MOVE=move
#EXEFILE=.exe

.SUFFIXES : .sig .sml .ui .uo

GRALOBJS= tigerabs.uo tigergrm.uo tigerlex.uo tigernlin.uo tigerinterp.uo tigercanon.uo\
	  tigerpp.uo tigerescap.uo tigertab.uo tigerseman.uo tigertemp.uo tigertopsort.uo tigertrans.uo\
	  tigerassem.uo tigermain.uo tigertree.uo tigerframe.uo tigerit.uo tigerpila.uo

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
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigersimpleregalloc.ui: tigerframe.ui tigerassem.uo 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigercodegen.uo: tigercodegen.ui tigertree.uo tigerframe.ui tigerassem.uo \
    tigertemp.ui 
tigerpila.uo: tigerpila.ui 
tigercodegen.ui: tigertree.uo tigerframe.ui tigerassem.uo 
tigerframe.ui: tigertree.uo tigerassem.uo tigertemp.ui 
tigermain.uo: tigerseman.ui tigerescap.ui tigergrm.ui tigerframe.ui \
    tigerit.uo tigercanon.ui tigerinterp.uo tigerlex.uo tigertrans.ui \
    tigerpp.uo 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigerpp.uo: tigerabs.uo 
tigertab.uo: tigertab.ui 
tigertree.uo: tigertemp.ui 
tigerassem.uo: tigertemp.ui 
tigerinterp.uo: tigertree.uo tigertab.ui tigerframe.ui tigerit.uo \
    tigertemp.ui 
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigersimpleregalloc.uo: tigersimpleregalloc.ui tigerassem.uo 
tigermuestratipos.uo: tigermuestratipos.ui tigertips.uo 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigertemp.ui 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigertemp.uo: tigertemp.ui 
tigercanon.ui: tigertree.uo tigertemp.ui 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigerseman.ui: tigerabs.uo 
tigerescap.ui: tigerabs.uo 
tigergrm.ui: tigerabs.uo 
tigerframe.uo: tigerframe.ui tigertree.uo tigerassem.uo tigertemp.ui 
tigerit.uo: tigertree.uo tigertab.ui 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigertopsort.ui tigerabs.uo tigertrans.ui 
tigermuestratipos.ui: tigertips.uo 
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo
 
