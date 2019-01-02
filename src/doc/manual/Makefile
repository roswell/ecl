FILES= *.txi */*.txi figures/*

.PHONY: all pdf info html

all: pdf info html

pdf: manual.pdf
info: ecl.info
html: html/index.html

manual.pdf: $(FILES)
	texi2pdf manual.txi

ecl.info: $(FILES)
	makeinfo --no-split manual.txi

html/index.html: $(FILES)
	makeinfo --html --css-include=ecl.css --split=chapter manual.txi
	rm -rf html
	mv ecl html
	cp -r figures html

clean:
	rm -rf *.{aux,cf,cfs,cp,cpp,cpps,cps,ex,exs,fn,fns,ft,fts,log,lsp,lsps,toc,tp,tps,vr,vrs,pdf,info,info-1,info-2,html} html
