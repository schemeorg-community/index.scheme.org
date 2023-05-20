# Scheme index dockerless distribution.

This file only concerns launching specifics, general documentation available at nginx/www/data/README.html.

## Running Java app

Install JDK version 11+
Change directory to app folder
Run `java -jar scheme-index.jar` to start

## Running Nginx server

Install nginx
Run `nginx -p <nginx-folder>` to start
Run `nginx -p <nginx-folder> -s stop` to stop
where <nginx-folder> is the "nginx" folder that was extracted from scheme index zip, not some folder of nginx installation.
Make sure nginx has read permissions to the static files; in particular, it won't work out of the box under user home directory.
