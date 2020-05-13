FROM ubuntu:18.04

RUN apt-get -y update && \
  apt-get -y dist-upgrade

# fetch dependencies
RUN apt-get -y install \
  wget \
  build-essential \
  libtinfo-dev \
  zlib1g-dev \
  libreadline-dev \
  python3 \
  python3-pip \
  autoconf \
  gettext \
  pkg-config \
  libtool

# install foma
RUN cd /usr/src && \
  wget -q -O - https://bitbucket.org/mhulden/foma/downloads/foma-0.9.18.tar.gz | tar xz && \
  cd foma-0.9.18 && \
  make && \
  make install

# fetch voikko source
RUN cd /usr/src && \
  wget -q -O - https://api.github.com/repos/voikko/corevoikko/tarball/master | tar xz

# install libvoikko (NB: `ldconfig` flushes cache so that libvoikko.so.1 can be found)
RUN cd /usr/src/voikko-corevoikko-*/libvoikko && \
  ./autogen.sh && \
  ./configure --disable-hfst --with-dictionary-path=/usr/share/voikko:/usr/lib/voikko && \
  make && \
  make install && \
  ldconfig

# build and install vocabulary
# NB: see `tools/bin/voikko-build-dicts` to choose options.
RUN cd /usr/src/voikko-corevoikko-*/voikko-fi && \
  make update-vocabulary && \
  make vvfst && \
  make vvfst-install \
  DESTDIR=/usr/lib/voikko \
  GENLEX_OPTS="--extra-usage=it,medicine,science,nature,education" \
  VOIKKO_VARIANT=science \
  VOIKKO_DESCRIPTION="Finnish (scientific vocabulary)" \
  SM_PATCHINFO="Development snapshot"

# install python module
RUN cd /usr/src/voikko-corevoikko-*/libvoikko/python && \
  python3 setup.py install

# Now you should be able to:
# python3 -c "from libvoikko import Voikko; print(Voikko('fi').analyze('alusta'))"
