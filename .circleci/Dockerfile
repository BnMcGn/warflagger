# syntax=docker/dockerfile:1
FROM cimg/postgres:9.6.22
# RUN apk add --no-cache python g++ make
WORKDIR /container
# RUN echo "export PATH=$HOME/.roswell/bin:$PATH" >> $BASH_ENV
RUN apt update
RUN apt install -y python3 python3-pip || true
RUN apt install -y postgresql-client || true
RUN apt install libpq-dev
RUN apt install -y curl || true

#RUN apt install -y git tar gzip ssh ca-certificates || true

RUN curl -L https://raw.githubusercontent.com/roswell/roswell/release/scripts/install-for-ci.sh | bash
RUN curl https://dist.ipfs.io/go-ipfs/v0.9.1/go-ipfs_v0.9.1_linux-amd64.tar.gz > go-ipfs.tar.gz
RUN tar xzvf go-ipfs.tar.gz
RUN go-ipfs/install.sh

COPY . .
# RUN yarn install --production
# CMD ["node", "src/index.js"]
