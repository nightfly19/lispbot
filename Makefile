.PHONY: all docker

all: docker

docker: lispbot
	docker build -t lispbot .

lispbot: lispbot.lisp settings.lisp
	./lispbot.lisp
