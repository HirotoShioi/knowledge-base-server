FROM heroku/heroku:16

ENV LANG C.UTF-8

# Install required packages.
RUN apt-get update \
    && apt-get upgrade -y --assume-yes \
    && ln -s /usr/lib/x86_64-linux-gnu/libstdc++.so.6 /usr/lib/libstdc++.so \
    && apt-get install -y --assume-yes xz-utils gcc libgmp-dev zlib1g-dev g++ tree iputils-ping vim-nox \
    && rm -rf /var/lib/apt/lists/*

# Install stack to /opt/stack/bin
RUN mkdir -p /opt/stack/bin \
    && mkdir -p /opt/cardano-knowledgebase-server/src \
    && mkdir -p /opt/cardano-knowledgebase-server/bin \
    && curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

# Set /src as an working directory
WORKDIR /opt/cardano-knowledgebase-server/src

# Set the PATH for the root user so they can use stack.
ENV PATH "$PATH:/opt/stack/bin:/opt/cardano-knowledgebase-server/bin"

# Install GHC using stack, based on your app's stack.yaml file.
COPY ./stack.yaml /opt/cardano-knowledgebase-server/src/stack.yaml
RUN stack --no-terminal setup

# Install all dependencies in .cabal file
COPY ./cardano-knowledgebase-server.cabal /opt/cardano-knowledgebase-server/src/cardano-knowledgebase-server.cabal
RUN stack --no-terminal build --only-dependencies

# Build application
COPY . /opt/cardano-knowledgebase-server/src
RUN stack --no-terminal --local-bin-path /opt/cardano-knowledgebase-server/bin/ install

# Move doc file to the bin folder
RUN mkdir -p /opt/cardano-knowledgebase-server/bin/doc
COPY ./doc /opt/cardano-knowledgebase-server/bin/doc

# Remove source code.
RUN rm -rf /opt/cardano-knowledgebase-server/src

# Add the apiuser and setup their PATH
RUN useradd -ms /bin/bash apiuser \
    && chown -R apiuser:apiuser /opt/cardano-knowledgebase-server
USER apiuser

# Set working directory
WORKDIR /opt/cardano-knowledgebase-server/bin

CMD /opt/cardano-knowledgebase-server/bin/cardano-knowledgebase-server-api -- run-server