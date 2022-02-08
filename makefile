create-dev-certs:
	mkdir -p cert
	openssl req -new -x509 -days 999 -nodes -out cert/fullchain.pem -keyout cert/privkey.pem
