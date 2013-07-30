mapToJS = (list, fn) -> _.map __jsc__.toJS(list), fn
objToJS = (obj) -> __jsc__.toJS obj

SDMinX = (r) -> r.x
SDMinY = (r) -> r.y
SDMaxX = (r) -> r.x + r.w
SDMaxY = (r) -> r.y + r.h

SDRectMake = (x, y, w, h) ->
  r = {}
  r.x = x
  r.y = y
  r.w = w
  r.h = h
  r

SDInsetRect = (r, byX, byY) ->
  r.x += byY
  r.y += byY
  r.w -= (byX * 2)
  r.h -= (byY * 2)
  r

SDIntegralRect = (r) -> r

class Screen
  @fromNS: (proxy) -> new Screen proxy
  constructor: (@proxy) ->
  frameIncludingDockAndMenu: -> @proxy.frameIncludingDockAndMenu()
  frameWithoutDockOrMenu: -> @proxy.frameWithoutDockOrMenu()
  nextScreen: -> Screen.fromNS @proxy.nextScreen()
  previousScreen: -> Screen.fromNS @proxy.previousScreen()

class App
  @fromNS: (proxy) -> new App proxy
  constructor: (@proxy) ->
  isHidden: -> @proxy.isHidden()
  show: -> @proxy.show()
  hide: -> @proxy.hide()
  allWindows: -> mapToJS @proxy.allWindows(), Window.fromNS
  visibleWindows: -> mapToJS @proxy.visibleWindows(), Window.fromNS
  title: -> @proxy.title()
  kill: -> @proxy.kill()
  kill9: -> @proxy.kill9()

class Window
  @fromNS: (proxy) -> new Window proxy
  constructor: (@proxy) ->
  topLeft: -> @proxy.topLeft()
  size: -> @proxy.size()
  frame: -> @proxy.frame()
  setTopLeft: (x) -> @proxy.setTopLeft(x)
  setSize: (x) -> @proxy.setSize(x)
  setFrame: (x) -> @proxy.setFrame(x)
  maximize: -> @proxy.maximize()
  minimize: -> @proxy.minimize()
  unMinimize: -> @proxy.unMinimize()
  app: -> App.fromNS @proxy.app()
  isNormalWindow: -> @proxy.isNormalWindow()
  screen: -> Screen.fromNS @proxy.screen()
  otherWindowsOnSameScreen: -> mapToJS @proxy.otherWindowsOnSameScreen(), Screen.fromNS
  otherWindowsOnAllScreens: -> mapToJS @proxy.otherWindowsOnAllScreens(), Screen.fromNS
  title: -> objToJS @proxy.title()
  isWindowMinimized: -> @proxy.isWindowMinimized()
  focusWindow: -> @proxy.focusWindow()
  focusWindowLeft: -> @proxy.focusWindowLeft()
  focusWindowRight: -> @proxy.focusWindowRight()
  focusWindowUp: -> @proxy.focusWindowUp()
  focusWindowDown: -> @proxy.focusWindowDown()
  getGrid: ->
    winFrame = @frame()
    screenRect = @screen().frameWithoutDockOrMenu()
    thirdScrenWidth = screenRect.w / Window.gridWidth
    halfScreenHeight = screenRect.h / 2.0
    {
      x: Math.round((winFrame.x - SDMinX(screenRect)) / thirdScrenWidth),
      y: Math.round((winFrame.y - SDMinY(screenRect)) / halfScreenHeight),
      w: Math.max(Math.round(winFrame.w / thirdScrenWidth), 1),
      h: Math.max(Math.round(winFrame.h / halfScreenHeight), 1)
    }
  setGrid: (grid, screen) ->
    screen ?= @screen()
    screenRect = screen.frameWithoutDockOrMenu()
    thirdScrenWidth = screenRect.w / Window.gridWidth
    halfScreenHeight = screenRect.h / 2.0
    newFrame = SDRectMake((grid.x * thirdScrenWidth) + SDMinX(screenRect),
                          (grid.y * halfScreenHeight) + SDMinY(screenRect),
                          grid.w * thirdScrenWidth,
                          grid.h * halfScreenHeight)
    newFrame = SDInsetRect(newFrame, Window.gridMarginX, Window.gridMarginY)
    newFrame = SDIntegralRect(newFrame)
    @setFrame newFrame

Window.gridWidth ?= 3
Window.gridMarginX ?= 5
Window.gridMarginY ?= 5

api =
  settings: -> SDAPI.settings()
  runningApps: -> mapToJS SDAppProxy.runningApps(), App.fromNS
  allWindows: -> mapToJS SDWindowProxy.allWindows(), Window.fromNS
  visibleWindows: -> mapToJS SDWindowProxy.visibleWindows(), Window.fromNS
  focusedWindow: -> Window.fromNS SDWindowProxy.focusedWindow()
  mainScreen: -> SDScreenProxy.mainScreen()
  allScreens: -> mapToJS SDScreenProxy.allScreens(), Screen.fromNS
  clipboardContents: ->
    body = NSPasteboard.generalPasteboard().stringForType(NSPasteboardTypeString)
    if body
      body.toString()
    else
      null

shell = (path, args, options) -> SDAPI.shell_args_options_ path, args, options
open = (thing) -> SDAPI.shell_args_options_ "/usr/bin/open", [thing], {}
bind = (key, modifiers, fn) -> SDKeyBinder.sharedKeyBinder().bind_modifiers_fn_ key, modifiers, SDJSCallback.alloc().initWithJavaScriptFn(fn).autorelease()
log = (str) -> SDLogWindowController.sharedLogWindowController().show_type_ str, "SDLogMessageTypeUser"
require = (file) -> SDConfigLoader.sharedConfigLoader().js().require(file)
alert = (str, delay) -> SDAlertWindowController.sharedAlertWindowController().show_delay_ str, delay
reloadConfig = -> SDConfigLoader.sharedConfigLoader().reloadConfig()
doAfter = (sec, fn) -> SDAPI.doFn_after_ SDJSCallback.alloc().initWithJavaScriptFn(fn).autorelease(), sec

listen = (event, fn) ->
  trampolineFn = (thing) ->
    if thing?
      switch thing.className().toString()
        when 'SDWindowProxy'
          fn Window.fromNS(thing)
        when 'SDAppProxy'
          fn App.fromNS(thing)
    else
      fn()
  SDEventListener.sharedEventListener().listenForEvent_fn_(event, trampolineFn)
