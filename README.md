# Knowledge-base-server

[![Build Status](https://travis-ci.org/HirotoShioi/knowledge-base-server.svg?branch=master)](https://travis-ci.org/HirotoShioi/knowledge-base-server)

This is Haskell application which parses the markdown file then make it available as an Web API using Servant.

```terminal
Cardano knowledge base server

Usage: <interactive> COMMAND [--version]
  Haskell servant server for Cardano related information

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  run-server               Run server
  new-knowledge            Create new knowledge
  new-faq                  Create new FAQ
  verify-docs              Check if all the documents are valid
```

## How it works

Parser will parse through all the files in the docs.
`MetaData.md` will be used as metadata.

example:
https://cardano-knowledgebase-server.herokuapp.com/faq