# https://github.com/darinmorrison/docker-haskell/tree/docker-library
FROM haskell:7.8

RUN cabal update
ADD ./backends/haskell/ot-demo.cabal /opt/ot-demo/backends/haskell/ot-demo.cabal
RUN cd /opt/ot-demo/backends/haskell && \
  cabal sandbox init && \
  cabal install --only-dependencies
ADD ./backends/haskell /opt/ot-demo/backends/haskell
RUN cd /opt/ot-demo/backends/haskell && cabal build
ADD ./public /opt/ot-demo/public
WORKDIR /opt/ot-demo/backends/haskell
CMD ./dist/build/ot-demo/ot-demo
