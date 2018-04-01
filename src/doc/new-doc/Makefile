FILES= *.txi */*.txi figures/*

.PHONY: all pdf info html

all: pdf info html

pdf: new-doc.pdf
info: ecldoc.info
html: ecldoc

new-doc.pdf: $(FILES)
	texi2pdf new-doc.txi

ecldoc.info: $(FILES)
	makeinfo new-doc.txi

ecldoc: $(FILES)
	makeinfo --html --css-include=ecl.css --split=chapter new-doc.txi
	cp -r figures ecldoc

clean:
	rm -rf *.{aux,cf,cfs,cp,cpp,cpps,cps,ex,exs,fn,fns,ft,fts,log,lsp,lsps,toc,tp,tps,vr,vrs,pdf,info,html}
