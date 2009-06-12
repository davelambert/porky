all: porky.pdf porky.html

porky.pdf: porky.texi
	texi2pdf $<

porky.html: porky.texi
	texi2html $<

clean:
	rm -f *.cp *.cps *.fn *.fns *.fo *.ky *.log *.pg *.toc *.tp *.vr
	rm -f porky.html
	rm -f porky.pdf
