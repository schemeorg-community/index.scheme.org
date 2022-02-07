create-dev-certs:
	mkdir -p cert
	openssl req -newkey rsa:4096 -x509 -sha256 -days 3650 -nodes -out cert/r7rsindex.crt -keyout cert/r7rsindex.key
