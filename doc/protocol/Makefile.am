EXTRA_DIST = cmd protocol.tex reqlist.sty

.dvi.ps:
	dvips -o $@ $<

protocol.dvi: protocol.tex commands.tex protocol.1st reqlist.sty
	latex protocol

protocol.1st: protocol.tex commands.tex reqlist.sty
	latex protocol
	touch protocol.1st

commands.tex: cmd/*.tex
	rm -f commands.tex; \
	for f in cmd/*.tex; do \
		cmd="$${f%.tex}"; \
		cmd="$${cmd##*/}"; \
		echo "\\begin{reqspec}{$$cmd}" >>commands.tex; \
		cat "$$f" >>commands.tex; \
		echo "\\end{reqspec}" >>commands.tex; \
	done

clean-local:
	rm -f protocol.aux protocol.log protocol.dvi commands.tex \
	protocol.1st protocol.ps protocol.out protocol.toc
