all: bigloo chibi chez gauche guile

base:
	podman build -f base.Dockerfile -t integrationtest-base ..

bigloo: base
	-podman run --rm -it $(shell podman build -q -f bigloo.Dockerfile ..)

chibi: base
	-podman run --rm -it $(shell podman build -q -f chibi.Dockerfile ..)

chez: base
	-podman run --rm -it $(shell podman build -q -f chez.Dockerfile ..)

gauche: base
	-podman run --rm -it $(shell podman build -q -f gauche.Dockerfile ..)

guile: base
	-podman run --rm -it $(shell podman build -q -f guile.Dockerfile ..)
