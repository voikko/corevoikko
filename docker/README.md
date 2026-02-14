# Voikko Docker container

This Docker container is a Ubuntu 24.04 image with a Voikko installation. For new developers, it offers an easy access to the latest code (and latest vocabulary).

Launch by running:

    docker compose up --build

Analyze a word:

    docker exec -ti docker-voikkocontainer-1 python3 -c 'from libvoikko import Voikko; print(Voikko("fi").analyze("alusta"))'
