# Zephyros

> As Odysseus climbed onto the shore, he opened up his MBP and began to write an email to a mailing list about how terrible his adventure had been, and how he'd almost been drowned by the sea, and how beautiful yet deadly was the sound of the Sirens.
>
> But, noticing how his windows were all in disarray, overlapping one another and terribly sized, he opened Zephyros. Using a little black-Ruby-magic, he tweaked his configs. Then he swiftly began arranging his windows using only his keyboard.
>
> The subjects of the local kingdom, noticing the elegant gracefulness with which he resized and repositioned his Mac windows, set out to make him their king. Even the mighty king of that land, two hundred years old yet stronger than any other, had decided to abdicate his throne for the sake of Odysseus. For he had learned of this foreigner's magnificent configs.
>
> But before the people could approach him, he sensed in himself their plan, so he set out for his home country to reclaim his dear wife Penelope. Yet even so, not without first pushing his config changes to his github repo for all to benefit from. And, being of such noble blood, he even contributed some of his ideas and configs to the Zephyros wiki for the sake of all.
> - The Odyssey

### About Zephyros

*The OS X window manager for hackers*

* Current version: **3.1**
* Requires: OS X 10.7 and up
* Download: get [.zip file](https://raw.github.com/sdegutis/zephyros/master/Builds/Zephyros-LATEST.app.tar.gz), unzip, right-click app, choose "Open"

#### Basics

At it's core, Zephyros just runs quietly in your menu bar, and runs your config script. You typically use this script to bind global hot keys to do stuff, often window-manager-type things.

#### Supported languages

- Ruby 2.0
- JavaScript
- [CoffeeScript 1.6.2](http://coffeescript.org/)
- anything that compiles to JS (see [altjs.org](http://altjs.org/) and [this guy's list](https://github.com/jashkenas/coffee-script/wiki/List-of-languages-that-compile-to-JS))

#### Stuff you can do

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

Note: If reloading your config file fails, your key bindings will be un-bound as a precaution, presuming that your config file is in an unpredictable state. They will be re-bound again next time your config file is successfully loaded. Same with events you're registered to.

#### Auto-reloading configs

You can tell Zephyros to automatically reload your config file whenver it changes. You can also bind a global hot key to reloading it. I'm also considering having it always watch `~/.zephyros/` (recursively), and reload when it sees a change. But I need your feedback on if that would be useful. Open an issue about it if you want this feature.

### Example Configs

Here's a sample Ruby config to get you started:

```ruby
mash = ["cmd", "alt", "ctrl"]

# useful for testing
API.bind("R", mash) { API. reload_config }

# maximize window (the hard way)
API.bind "M", mash do
  win = API.focused_window
  f = win.screen.frame_without_dock_or_menu
  f.inset! 10, 10 # give the window-shadows a little breathing room
  win.frame = f
  # note: we could have just done win.maximize, but this is more flexible
end

# push to top half of screen
API.bind "K", mash do
  win = API.focused_window
  frame = win.screen.frame_without_dock_or_menu
  frame.h /= 2
  win.frame = frame
end

# push to bottom half of screen
API.bind "J", mash do
  win = API.focused_window
  frame = win.screen.frame_without_dock_or_menu
  frame.y += frame.h / 2
  frame.h /= 2
  win.frame = frame
end
```

#### More configs

* [The author's config](https://github.com/sdegutis/dotfiles/blob/master/home/.zephyros.rb)
* Look in the [wiki home page](https://github.com/sdegutis/zephyros/wiki) for other people's configs (including a SizeUp emulator)

### API Docs

- [Ruby](Docs/RUBY_API.md)
- [JavaScript or CoffeeScript](Docs/JS_API.md)
- Python (coming soon)

### Community

- #zephyros on irc.freenode.org
- [wiki home page](https://github.com/sdegutis/zephyros/wiki), a place to share configs and stuff
- [Zephyros mailing list (Google Groups)](https://groups.google.com/forum/#!forum/zephyros-app) for sharing ideas and stuff

### Change log

- HEAD
    - Adds preliminary [Nu (lisp)](http://programming.nu/) support
- 3.1
    - Added a fuzzy-matching `choose_from` method to the Ruby API (it's pretty sweet)
- 3.0
    - Added a Ruby 2.0 API
    - Changed JS API:
        - any function that returned a CGRect now returns a hash, keys `'x', 'y', 'w', 'h'`
        - any function that returned a CGPoint now returns a hash, keys `'x', 'y'`
        - any function that returned a CGSize now returns a hash, keys `'w', 'h'`
        - adds functions `SDMinX`, `SDMinY`, `SDMaxX`, `SDMaxY`, `SDRectMake`, `SDInsetRect`, `SDIntegralRect`
- 2.7
    - Broke the Log window's REPL (fixed in 3.0)
    - Less dumb way of choosing configs (uses UI this time)
    - Added a non-functioning Ruby option
- 2.6.1
    - Added 'screens_changed' event
- 2.6
    - First version anyone should care about

### Todo

* Better CSS styling in [the Log Window](Zephyros/logwindow.html)
* Better app icon
* Better menu bar icon
* Stop using JSCocoa and just use JavaScriptCore directly
* Better error handling in JS API
* Check for syntax errors (in raw JS) before evaluating code, and show them in the log window if there are any
* Show evaluated (raw JS) code when there are runtime errors
* Give a better error message if your config file *actually turns out to be a directory* (sigh)
* Add `mouseMoved` event, but coalesce notifications to a reasonable amount (default every 0.5 sec, make it configurable)
* Add `api.screenUnderMouse()`

### Special Thanks

- Kyle
- jkj, mikeash, whitequark, charliesome, Psy, dirkg, drbrain
- Matz, Apple
- and many others

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
