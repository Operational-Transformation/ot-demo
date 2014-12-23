# https://github.com/darinmorrison/docker-haskell/tree/docker-library
FROM haskell:7.8

RUN cabal update
RUN apt-get install -y wget
RUN cd /opt && \
  wget --no-check-certificate http://github.com/ocharles/engine.io/archive/variadic-on.tar.gz && \
  tar xvzf variadic-on.tar.gz && mv engine.io-variadic-on engine.io
ADD ./backends/haskell/ot-demo.cabal /opt/ot-demo/backends/haskell/ot-demo.cabal
RUN cd /opt/ot-demo/backends/haskell && \
  cabal sandbox init && \
  cabal sandbox add-source /opt/engine.io/socket-io && \
  cabal install --only-dependencies
ADD ./backends/haskell /opt/ot-demo/backends/haskell
RUN cd /opt/ot-demo/backends/haskell && cabal build
ADD ./public /opt/ot-demo/public
WORKDIR /opt/ot-demo/backends/haskell
CMD ./dist/build/ot-demo/ot-demo
