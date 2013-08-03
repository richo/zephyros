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

* Current version: **4.0**
* Requires: OS X 10.7 and up
* Download: get [.zip file](https://raw.github.com/sdegutis/zephyros/master/Builds/Zephyros-LATEST.app.tar.gz), unzip, right-click app, choose "Open"

#### Basics

At it's core, Zephyros just runs quietly in your menu bar, and listens for your script. You typically write a script that binds global hot keys to do stuff, often window-manager-type things.

#### Some languages you can use

- [Using Clojure](Docs/Clojure.md)
- [Using Ruby](Docs/Ruby.md)
- [Using Go](Docs/Go.md)
- Using Python [(coming soon)](libs/not-ready/zeph.py)
- Using node.js [(coming soon)](libs/not-ready/zeph.js)

#### Other languages

You can script Zephyros from **any language** that can talk JSON over TCP.

Don't see your favorite language here? Why not [write a Zephyros lib](Docs/TCP.md) for it!

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

#### Auto-reloading configs

* Automatically reload your config file whenever it (or anything you choose) changes.
* Bind a global hot key to reloading it.

### Example Configs

#### Ruby

```ruby
require '/Applications/Zephyros.app/Contents/Resources/libs/zephyros.rb'

# push to top half of screen
API.bind "K", ["cmd", "alt", "ctrl"] do
  win = API.focused_window
  frame = win.screen.frame_without_dock_or_menu
  frame.h /= 2
  win.frame = frame
end
```

#### Clojure

```clojure
(use '[leiningen.exec :only (deps)])
(deps '[[org.clojure/data.json "0.2.2"]])

(load-file "/Applications/Zephyros.app/Contents/Resources/libs/zephyros.clj")

(bind "D" ["Cmd" "Shift"]
      (fn []
        (alert "hello world" 1)))

@listen-for-callbacks ;; necessary when you use (bind) or (listen)
```

#### Go

```go
package main

import (
	. "../../Applications/Zephyros.app/Contents/Resources/libs/zephyros_go"
)

func main() {
	API.Bind("D", []string{"Cmd", "Shift"}, func() {
		API.Alert("hello world", 1)
	})

	ListenForCallbacks()
}
```

#### More configs

* [The author's config](https://github.com/sdegutis/dotfiles/blob/master/home/.zephyros.rb)
* Look in the [wiki home page](https://github.com/sdegutis/zephyros/wiki) for other people's configs (including a SizeUp emulator)

### Community

- #zephyros on irc.freenode.org
- [wiki home page](https://github.com/sdegutis/zephyros/wiki), a place to share configs and stuff

### Change log

- 4.0
    - **Adds support for any language that speaks TCP**
    - Temporarily removes JS support
    - Includes Ruby lib
    - Includes Clojure lib
    - Includes Go lib (partially done)
    - Ruby API now requires `require '~/Applications/Zephyros.app/Contents/Resources/libs/zephyros.rb'`
    - Slight UI upgrade
    - Internal REPL is broken again
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

### Help Needed

Are you good at stuff? We could use these things:

* Better CSS styling in [the Log Window](Zephyros/logwindow.html)
* Better app icon
* Better menu bar icon

### Special Thanks

- To everyone.
    - You're great!

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
