# syntax=docker/dockerfile:1
FROM cimg/python:3.8.12
#FROM cimg/postgres:9.6.22

WORKDIR /container
# RUN echo "export PATH=$HOME/.roswell/bin:$PATH" >> $BASH_ENV
USER root
RUN apt update && \
    apt install -y postgresql-client || true && \
    apt install libpq-dev

# Install dependencies from Debian repositories
RUN apt update && apt install -y make wget bzip2 && apt-get clean

# Install SBCL from the tarball binaries.
RUN wget http://prdownloads.sourceforge.net/sbcl/sbcl-2.1.8-x86-64-linux-binary.tar.bz2 -O /tmp/sbcl.tar.bz2 && \
    mkdir /tmp/sbcl && \
    tar jxvf /tmp/sbcl.tar.bz2 --strip-components=1 -C /tmp/sbcl/ && \
    cd /tmp/sbcl && \
    sh install.sh && \
    cd /tmp && \
    rm -rf /tmp/sbcl/ && \
    rm /tmp/sbcl.tar.bz2 && \
    curl -O https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp && \
    echo "(require \"asdf\")" > /etc/sbclrc

USER circleci
COPY . .
# CMD ["node", "src/index.js"]
