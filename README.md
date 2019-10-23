# KSF demo

My implementation of
https://gist.github.com/f-f/d09d10d0e1b2a81cc035eb35bb00d958

## Build and installation instructions

Download and install elm.  The demo has been tested and developed with
elm 0.19.0.  See https://guide.elm-lang.org/install/elm.html

Build the JavaScript file with
```
elm make src/Main.elm --output=main.js
```

Copy `main.js`, `index.html` and `index.css` to a web server's static
directory.

## About the demo

The demo has a number of limitations.

* Logging out is not implemented.
* Reloading the page discards user login and returns the user to the
  login dialog.
* Address edit validation errors are handled gracefully, all other
  errors lead the app to an error state with no recovery options other
  than reloading the page.
* There's no explicit success message for successful address edits.
* Some fields are read only on the server side. They are editable in
  the demo but any changes are discarded.
* User name and address are shown. There's more user data available on
  the server but I limited the demo to just those.

The transitions between states where the app is loading or updating
data are pretty clumsy. If I was doing something user facing and not
just a demo I'd most likely use an overlay to show that the app is
busy and not flash content by removing and displaying forms.

I feel that there's a bit too much repetition with the field names in
the demo.
