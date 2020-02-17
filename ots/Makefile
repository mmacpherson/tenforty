IMAGE=ots-convert
OTS_DKR=docker run --rm -v $(PWD):/work $(IMAGE) python /work/convert_ots_library.py

libraries: build _ots_2018.c

build:
	docker build -t $(IMAGE) .

_ots_2018.c: OpenTaxSolver2018_16.06_linux64.tgz
	$(OTS_DKR) 2018 $^ > $@

_ots_2019.c: OpenTaxSolver2019_17.03_linux64.tgz
	$(OTS_DKR) 2019 $^ > $@

clean:
	rm -f *.c

nuke: clean
	docker rmi $(IMAGE)

.PHONY: libraries build clean nuke
