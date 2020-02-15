FROM python:3.8

RUN pip install -U \
  click \
  pycparser==2.19 \
  sh

# -- grab pycparser repo to get its "fake" include files
RUN mkdir /pycparser
RUN curl -L https://github.com/eliben/pycparser/archive/release_v2.19.tar.gz -o /tmp/pycparser.tgz
RUN tar xf /tmp/pycparser.tgz -C /pycparser --strip 1

# -- premake working directory
RUN mkdir /ots
WORKDIR /ots
