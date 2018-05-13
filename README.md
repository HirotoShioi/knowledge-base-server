# Knowledgebase-server

[![Build Status](https://travis-ci.org/HirotoShioi/knowledge-base-server.svg?branch=master)](https://travis-ci.org/HirotoShioi/knowledge-base-server)

This is Haskell application which parses the markdown file then make it available as an Web API using Servant.

## How it works

Parser will parse through all the files in the docs.
`MetaData.md` will be used as metadata.

example:
https://cardano-knowledgebase-server.herokuapp.com/faq
