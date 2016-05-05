new-doc.pdf: *.txi */*.txi figures/*
	texi2pdf new-doc.txi

ecldoc.info: *.txi */*.txi figures/*
	makeinfo new-doc.txi

clean:
	rm -rf *.{aux,cf,cfs,cp,cpp,cpps,cps,ex,exs,fn,fns,ft,fts,log,lsp,lsps,toc,tp,tps,vr,vrs,pdf,info}
