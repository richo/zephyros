# Zephyros

> As Odysseus climbed onto the shore, he opened up his MBP and began to write an email to a mailing list about how terrible his adventure had been, and how he'd almost been drowned by the sea, and how beautiful yet deadly was the sound of the Sirens.
>
> But, noticing how his windows were all in disarray, overlapping one another and terribly sized, he opened Zephyros. Using a little black-CoffeeScript-magic, he tweaked his configs. Then he swiftly began arranging his windows using only his keyboard.
>
> The subjects of the local kingdom, noticing the elegant gracefulness with which he resized and repositioned his Mac windows, set out to make him their king. Even the mighty king of that land, two hundred years old yet stronger than any other, had decided to abdicate his throne for the sake of Odysseus. For he had learned of this foreigner's magnificent configs.
>
> But before the people could approach him, he sensed in himself their plan, so he set out for his home country to reclaim his dear wife Penelope. Yet even so, not without first pushing his config changes to his github repo for all to benefit from. And, being of such noble blood, he even contributed some of his ideas and configs to the Zephyros wiki for all to benefit from.
> - The Odyssey

### About Zephyros

*The OS X window manager for hackers*

* Current version: **2.7**
* Requires: OS X 10.7 and up
* Download: [latest .zip file](https://raw.github.com/sdegutis/zephyros/master/Builds/Zephyros-LATEST.app.tar.gz), unzip, right-click app, choose "Open"

Table of contents:

* [Overview](#overview)
    * [Basics](#basics)
    * [Modular Configs](#modular-configs)
    * [Auto-Reload Configs](#auto-reload-configs)
    * [Using Other Languages](#using-other-languages)
* [Config Example](#config-example)
    * [More Config Tricks/Examples](#more-config-tricksexamples)
* [JS API doc](#ruby-api)
* [Ruby API doc](#ruby-api)
* [Change log](#change-log)
* [Todo](#todo)
* [License](#license)

### Overview

#### Basics

At it's core, Zephyros is just a program that runs quietly in your menu bar, and loads a config file in your home directory.

You can write your config file using:

- JavaScript as `~/.zephyros.js`
- [CoffeeScript 1.6.2](http://coffeescript.org/) as `~/.zephyros.coffee`
- more, see [using other languages](#using-other-languages) below

In your config file, `bind()` some global hot keys to your own JavaScript functions which do window-managery type things.

Here are some things you can do with Zephyros's simple API ([actual API docs are below](#api)):

- find the focused window
- determine window sizes and positions
- move and resize windows
- change focus to a given window
- listen to global events (window created, app launched/killed, etc)
- transfer focus to the closest window in a given direction
- run shell scripts
- open apps, links, or files
- get free pizza (okay not really)
- and more!

Is the API missing something you need? File an issue and let me know!

For your convenience, [underscore.js](http://underscorejs.org/) 1.4.4 is loaded beforehand.

#### Modular Configs

You can `require()` other files.

#### Auto-Reload Configs

When you enable this feature via the menu, Zephyros will reload your config file it changes.

#### Using Other Languages

Optionally, you can tell Zephyros to preprocess your config file with any command line utility. This lets you use any [altjs.org](http://altjs.org/) languages, such as from [this guy's list](https://github.com/jashkenas/coffee-script/wiki/List-of-languages-that-compile-to-JS)).

#### Config Caveats

- If reloading your config file fails, your key bindings will be un-bound as a precaution, presuming that your config file is in an unpredictable state. They will be re-bound again next time your config file is successfully loaded. Same with events you're registered to.

### Config Example

Put the following in `~/.zephyros.coffee`

```coffeescript
# useful for testing
bind "R", ["cmd", "alt", "ctrl"], -> reloadConfig()

# maximize window
bind "M", ["cmd", "alt", "ctrl"], ->
  win = api.focusedWindow()
  win.setFrame win.screen().frameWithoutDockOrMenu()

# push to top half of screen
bind "K", ["cmd", "alt", "ctrl"], ->
  win = api.focusedWindow()
  frame = win.screen().frameWithoutDockOrMenu()
  frame.size.height /= 2
  win.setFrame frame

# push to bottom half of screen
bind "J", ["cmd", "alt", "ctrl"], ->
  win = api.focusedWindow()
  frame = win.screen().frameWithoutDockOrMenu()
  frame.origin.y += frame.size.height / 2
  frame.size.height /= 2
  win.setFrame frame
```

#### More Config Tricks/Examples

The [wiki home page](https://github.com/sdegutis/zephyros/wiki) has a list of configs from users, and configs that replicate other apps (like SizeUp and Divvy).

### JS API

[Here.](Docs/JS_API.md)

### Ruby API

[Here.](Docs/RUBY_API.md)

### Change log

- HEAD
  - Added a barely-working, mostly-broken Ruby API
  - Broke the Log window
  - Possibly broke some of the JS stuff? maybe not though...
- 2.7
  - Less dumb way of choosing configs (uses UI this time)
  - Added a non-functioning Ruby option
- 2.6.1
  - Added 'screens_changed' event
- 2.6
  - First version anyone should care about

### Todo

#### Want to help?

* Are you some kind of designer? Want to help? Great! We need these 3 things:
    1. better CSS styling in [the Log Window](Zephyros/logwindow.html)
    2. a better app icon (current one is literally a ripoff of [AppGrid's](https://dxezhqhj7t42i.cloudfront.net/image/1e0daca8-3855-4135-a2a1-8569d28e8648))
    3. a better menu bar icon (current one is literally a ripoff of [AppGrid's](http://giantrobotsoftware.com/appgrid/screenshot1-thumb.png))
* Are you a JS programmer? There's lots of low-hanging fruit!
    * Better error handling when passing wrong stuff into API functions
    * Convert more stuff in `api.coffee` to JS types before giving them to people (ugh so tedious though)
* Are you an ObjC programmer? There's lots of low-hanging fruit!
    * Get rid of the `NSPasteboard` category and the use of `objc_[g,s]etAssociatedObject`
    * Check for syntax errors (in raw JS) before evaluating code, and show them in the log window if there are any
    * Show evaluated (raw JS) code when there are runtime errors
    * Give a better error message if your config file *actually turns out to be a directory* (sigh)
    * Add `mouseMoved` event, but coalesce notifications to a reasonable amount (default every 0.5 sec, make it configurable)
    * Add `api.windowUnderMouse()`
    * Add `api.screenUnderMouse()`

### License

> Released under MIT license.
>
> Copyright (c) 2013 Steven Degutis
>
> Permission is hereby granted, free of charge, to any person obtaining a copy
> of this software and associated documentation files (the "Software"), to deal
> in the Software without restriction, including without limitation the rights
> to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
> copies of the Software, and to permit persons to whom the Software is
> furnished to do so, subject to the following conditions:
>
> The above copyright notice and this permission notice shall be included in
> all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
> IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
> FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
> AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
> LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
> OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
> THE SOFTWARE.
