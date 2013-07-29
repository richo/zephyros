## Zephyros JS API

* [Top Level](#top-level)
* [Type "API"](#type-api)
* [Type "Settings"](#type-settings)
* [Type "Window"](#type-window)
* [Type "Screen"](#type-screen)
* [Type "App"](#type-app)
* [Other Type](#other-types)
* [Events](#events)

### Top Level

```coffeescript
property (API) api

- (void) log(String str)                   # shows up in the log window
- (void) alert(String str[, Float delay])  # shows in a fancy alert; optional delay is seconds

- (void) bind(String key,              # case-insensitive single-character string; see link below
              Array<String> modifiers, # may contain any number of: "cmd", "ctrl", "alt", "shift"
              Function fn)             # javascript fn that takes no args; return val is ignored

- (void) listen(String eventName, Function callback) # see Events section below

- (void) reloadConfig()

- (void) require(String path) # looks at extension to know which language to use
                              # if relative path, looks in `~/.zephyros/`

- (Hash) shell(String path, Array<String> args[, String stdin]) # returns {"stdout": string,
                                                                #          "stderr": string,
                                                                #          "status": int}

- (void) open(String thing) # can be path or URL

- (void) doAfter(Float sec, Function fn)
```

The function `bind()` uses [this list](https://github.com/sdegutis/zephyros/blob/master/Zephyros/SDKeyBindingTranslator.m#L148) of key strings.

### Type: `API`

```coffeescript
- (Settings) settings()

- (Array<Window>) allWindows()
- (Array<Window>) visibleWindows()
- (Window) focusedWindow()

- (Screen) mainScreen()
- (Array<Screen>) allScreens()

- (Array<App>) runningApps()

- (String) clipboardContents()
```

### Type: `Settings`

```coffeescript
property (Float) alertDisappearDelay # in seconds.
property (Boolean) alertAnimates     # when opening.

- (NSBox) alertBox()
- (NSTextField) alertTextField()
```

### Type: `Window`

```coffeescript
- (Grid) getGrid()
- (void) setGrid(Grid g[, Screen optionalScreen])
# grids are just JS objects with keys {x,y,w,h} as numbers, 0-based index

class-property (number) Window.gridWidth # default: 4
class-property (number) Window.gridMarginX # default: 5
class-property (number) Window.gridMarginY # default: 5
# these margins are for giving window-shadows some breathing room

- (CGPoint) topLeft()
- (CGSize) size()
- (CGRect) frame()

- (void) setTopLeft(CGPoint thePoint)
- (void) setSize(CGSize theSize)
- (void) setFrame(CGRect frame)
- (void) maximize()

- (Screen) screen()
- (Array<Window>) otherWindowsOnSameScreen()
- (Array<Window>) otherWindowsOnAllScreens()

- (String) title()
- (Boolean) isWindowMinimized()

- (Boolean) isNormalWindow() # you probably want to avoid resizing/moving ones that aren't

- (App) app()

- (Boolean) focusWindow()
- (void) focusWindowLeft()
- (void) focusWindowRight()
- (void) focusWindowUp()
- (void) focusWindowDown()
```

### Type: `Screen`

```coffeescript
- (CGRect) frameIncludingDockAndMenu()
- (CGRect) frameWithoutDockOrMenu()

- (Screen) nextScreen()
- (Screen) previousScreen()
```

### Type: `App`

```coffeescript
- (Array<Window>) allWindows()
- (Array<Window>) visibleWindows()

- (String) title()
- (Boolean) isHidden()

- (void) kill()
- (void) kill9()
```

### Other Types

The rest of the types here are classes from ObjC, bridged to JS. Here's a few for reference:

```coffeescript
# CGRect
property (CGPoint) origin # top-left
property (CGSize) size

# CGSize
property (Float) width
property (Float) height

# CGPoint
property (Float) x
property (Float) y
```

The rest you'll have to look up for yourself.

### Events

```coffeescript
'window_created', callback args: (win)
'window_minimized', callback args: (win)
'window_unminimized', callback args: (win)
'window_moved', callback args: (win)
'window_resized', callback args: (win)
'app_launched', callback args: (app)
'app_died', callback args: (app)
'app_hidden', callback args: (app)
'app_shown', callback args: (app)
'screens_changed', callback args: ()
```
