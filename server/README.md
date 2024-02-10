# Server

Web request handling in Haskell.

### Run the server

Make sure you have [stack](https://docs.haskellstack.org/en/stable/) installed.

```shell
$ stack test # run tests
$ stack run  # run the web server (default config file is "./config.yaml")
$ stack run -- <Path to your config file> # run the web server
```

### Configuration

Change the config file according to your need. Check out `config-example.yaml` for example.

**DO NOT COMMIT YOUR TOKEN!**

### Before your commit

Please format your code with [fourmolu](https://hackage.haskell.org/package/fourmolu).
