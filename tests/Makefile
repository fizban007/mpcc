all :
	for f in *.rkt ; do racket $$f ; done
	for f in *.C ; do g++ $$f ; ./a.out ; done

clean :
	rm a.out *.C
