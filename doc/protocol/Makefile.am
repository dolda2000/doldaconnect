EXTRA_DIST = cmd protocol.tex

.dvi.ps:
	dvips -o $@ $<

protocol.dvi: protocol.tex commands.tex protocol.1st
	latex protocol

protocol.1st: protocol.tex commands.tex
	latex protocol
	touch protocol.1st

commands.tex: cmds/*.tex
	rm -f commands.tex \
	for f in cmds/*.tex; do \
		echo "\\begin{reqspec}{${f%.tex}}" >>commands.tex; \
		cat "$f" >>commands.tex; \
		echo "\\end{reqspec}" >>commands.tex; \
	done
