FROM haskell:8
RUN cabal update
RUN cabal install Parsec
RUN mkdir /app
WORKDIR /app
