FILES= *.txi */*.txi figures/*

MANNAME=ecl-manual
HTMLDIR=$(MANNAME)
PDFNAME=$(MANNAME).pdf
# left this as "ecl" for historic reasons
INFONAME=ecl.info

CWD=$(shell pwd)

.PHONY: all pdf info html

all: pdf info html

pdf: $(PDFNAME)
info: $(INFONAME).gz
html: $(HTMLDIR)/index.html

$(PDFNAME): $(FILES)
	texi2pdf manual.txi \
	&& cp -pf manual.pdf $(PDFNAME)

$(INFONAME).gz: $(FILES)
	$(MAKEINFO) -o $(INFONAME) --no-split manual.txi
	gzip < $(INFONAME) > $(INFONAME).gz

$(HTMLDIR)/index.html: $(FILES)
	-rm -rf $(HTMLDIR)/
	$(MAKEINFO) -o $(HTMLDIR) --html --css-include=ecl.css --split=section manual.txi
	cp -r figures $(HTMLDIR)/

clean:
	-rm -rf *.{aux,cf,cfs,cp,cpp,cpps,cps,ex,exs,fn,fns,ft,fts,log,lsp,lsps,toc,tp,tps,vr,vrs,pdf,info,info-1,info-2,info.gz,html} html/ \
		manual.[ceflvpa]* manual.toc manual.tp manual.tps \
		$(PDFNAME) $(HTMLDIR)/ \
		$(INFONAME) $(INFONAME).gz
