# SHRDLU Server

This project is the server side of the [SHRDLU](https://en.wikipedia.org/wiki/SHRDLU) demonstration used 
[at ELS 2015](https://www.european-lisp-symposium.org/static/proceedings/2015.pdf). 

## Installation

You'll need [Leiningen](https://leiningen.org/) to build this project. Start up a REPL in the project root folder:

```
lein repl
```

From there on you're good to go.

## Usage

Start up the server by running the following from the REPL:

```clojure
(startup 2222)
```

Then, open up [the NetLogo client](https://github.com/cognesence/shrdlu-client) and click the "Connect" button. You
should see an acceptance message printed on the REPL. Now in the NetLogo client click the button to start the REPL.
From here you're ready to start issuing commands. Try entering this into the REPL:

```clojure
(shrep-1 '(make a blue box and put it on s1))
```

You should see the winch drop a blue box called `b1` onto stack 1. Now make a red box and put it on top of the blue box:

```clojure
(shrep-1 '(make a red box and put it on b1))
```

A new red box will be created called `b2` will be created and placed on top of the blue box `b1`. Now let's try to move
`b1`:

```clojure
(shrep-1 '(put b1 on s6))
```

Now the winch can't move `b1` right away because `b2` is sitting on top of it. Our operator search mechanism will figure
this out and move `b2` first so we can proceed.

## Options

FIXME: listing of options this app accepts.

## Examples

...

### Bugs

...

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2017 Simon Lynch

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
