# ghc-eventlog-chrome

A command line tool for converting GHC's [event logs](https://www.haskell.org/ghc/blog/20190924-eventful-ghc.html) to
Chrome ["Trace Event" format](https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview).

### Usage

```
ghc-eventlog-chrome path/to/ghc.eventlog > output.time-trace
```
