# The development environment as a container

Using this method, you can set up a development environment separate from your
main machine.
 
Start:

    docker-compose run devenv bash

This with open a terminal with the voikko environment set up. In this
environment you can for example run `make` with all dependencies available to
you.

If you need to rebuild the container (if you changed the Dockerfile), issue this
command:
    
    docker-compose build devenv
