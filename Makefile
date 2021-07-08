.PHONY: dev
venv: venv/touchfile

venv/touchfile: requirements.txt
	test -d venv || python3 -m venv venv
	. venv/bin/activate; pip install -U pip; pip install -Ur requirements.txt
	touch venv/touchfile

.PHONY: dev
test: venv
	. venv/bin/activate; nosetests tests

.PHONY: dev
dev: venv
		. venv/bin/activate; python setup.py build_ext --inplace

.PHONY: notebook
notebook: venv
		. venv/bin/activate; jupyter notebook --no-browser --ip='0.0.0.0' --NotebookApp.token='' --NotebookApp.password=''

.PHONY: clean
clean:
	rm -rf venv
	find -iname "*.pyc" -delete
