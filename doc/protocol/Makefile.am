EXTRA_DIST = cmd protocol.tex

.dvi.ps:
	dvips -o $@ $<

protocol.dvi: protocol.tex commands.tex protocol.1st
	latex protocol

protocol.1st: protocol.tex commands.tex
	latex protocol
	touch protocol.1st

commands.tex: cmd/*.tex
	rm -f commands.tex; \
	for f in cmd/*.tex; do \
		echo "\\begin{reqspec}{${f%.tex}}" >>commands.tex; \
		cat "$$f" >>commands.tex; \
		echo "\\end{reqspec}" >>commands.tex; \
	done

clean-local:
	rm -f protocol.aux protocol.log protocol.dvi commands.tex \
	protocol.1st protocol.ps
