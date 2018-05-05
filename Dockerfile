FROM heroku/heroku:16

ENV LANG C.UTF-8

# Install required packages.
RUN apt-get update
RUN apt-get upgrade -y --assume-yes
# Install packages for stack and ghc.
RUN apt-get install -y --assume-yes xz-utils gcc libgmp-dev zlib1g-dev
# Install convenience utilities, like tree, ping, and vim.
RUN apt-get install -y --assume-yes tree iputils-ping vim-nox

# Remove apt caches to reduce the size of our container.
RUN rm -rf /var/lib/apt/lists/*

# Install stack to /opt/stack/bin
RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

# Create directories.
RUN mkdir -p /opt/cardano-knowledgebase-server/src
RUN mkdir -p /opt/cardano-knowledgebase-server/bin

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
RUN useradd -ms /bin/bash apiuser
RUN chown -R apiuser:apiuser /opt/cardano-knowledgebase-server
USER apiuser

# Set working directory
WORKDIR /opt/cardano-knowledgebase-server/bin

CMD /opt/cardano-knowledgebase-server/bin/cardano-knowledgebase-server-api