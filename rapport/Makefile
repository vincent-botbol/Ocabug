TARGET=rapport.pdf

all: $(TARGET)

$(TARGET): rapport.tex
	pdflatex $<

run: $(TARGET)
	evince $<