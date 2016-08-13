all:lisp-envi.a hello-lisp-system--all-systems.fasb

#lisp environment.
lisp-envi.a: lisp-envi.asd lisp-envi.lisp build_static.lisp
	ecl -load build_static.lisp

#your lisp system.
hello-lisp-system--all-systems.fasb: hello-lisp-system.asd hello-lisp.lisp \
build_fasl.lisp
	ecl -load build_fasl.lisp

clean:
	-rm -f hello-lisp-system--all-systems.fasb lisp-envi.a
