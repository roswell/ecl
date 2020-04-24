FILES= *.txi */*.txi figures/*

.PHONY: all pdf info html

all: pdf info html

pdf: manual.pdf
info: ecl.info.gz
html: html/index.html

manual.pdf: $(FILES)
	texi2pdf manual.txi

ecl.info.gz: $(FILES)
	$(MAKEINFO) --no-split manual.txi
	gzip < ecl.info > ecl.info.gz

html/index.html: $(FILES)
	$(MAKEINFO) --html --css-include=ecl.css --split=section manual.txi
	rm -rf html
	mv ecl html
	cp -r figures html

clean:
	rm -rf *.{aux,cf,cfs,cp,cpp,cpps,cps,ex,exs,fn,fns,ft,fts,log,lsp,lsps,toc,tp,tps,vr,vrs,pdf,info,info-1,info-2,info.gz,html} html
