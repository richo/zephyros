## Zephyros Ruby API

#### Setup

Add this to the top of your ruby script:
```ruby
require '~/Applications/Zephyros.app/Contents/Resources/libs/zephyros.rb'
```

#### Auto-running

Change your auto-launch-command inside Zephyros to:
```shell
ruby path/to/zephyros.rb
```

#### API

The function `API.bind` uses [this list](https://github.com/sdegutis/zephyros/blob/master/Zephyros/SDKeyBindingTranslator.m#L148) of key strings.

```ruby
class API

  class << self

    def reload_config; end

    def after(sec, &blk); end
    def log(str); end
    def bind(key, mods, &blk); end
    def alert(msg); end

    def shell(path, args, opts={}); end # TODO: document opts
    def open(thing); end
    def clipboard_contents; end

    def choose_from(list, title, &blk); end
        # takes list of strings
        # blk gets index of chosen string, or nil of canceled

    def focused_window; end
    def visible_windows; end
    def all_windows; end

    def main_screen; end
    def all_screens; end

    def running_apps; end

    def listen(event, &blk); end
        # 'window_created', callback args: (win)
        # 'window_minimized', callback args: (win)
        # 'window_unminimized', callback args: (win)
        # 'window_moved', callback args: (win)
        # 'window_resized', callback args: (win)
        # 'app_launched', callback args: (app)
        # 'app_died', callback args: (app)
        # 'app_hidden', callback args: (app)
        # 'app_shown', callback args: (app)
        # 'screens_changed', callback args: ()

  end

end

class Screen

  def frame_including_dock_and_menu; end
  def frame_without_dock_or_menu; end
  def next_screen; end
  def previous_screen; end

end

class App

  def all_windows; end
  def visible_windows; end

  def title; end
  def hidden?; end

  def show; end
  def hide; end

  def kill; end
  def kill9; end

end

$window_grid_width = 3
$window_grid_height = 2
$window_grid_margin_x = 5
$window_grid_margin_y = 5

class Window

  def other_windows_on_same_screen; end

  def frame; end # => returns a Rect
  def top_left; end # => returns a Point
  def size; end # => returns a Size

  def frame=(x); end # => takes a Rect
  def top_left=(x); end # => takes a Point
  def size=(x); end # => takes a Size

  def maximize; end
  def minimize; end
  def un_minimize; end

  def screen; end
  def app; end

  def title; end

  def focus_window; end
  def focus_window_left; end
  def focus_window_right; end
  def focus_window_up; end
  def focus_window_down; end

  def normal_window?; end
  def minimized?; end

  def get_grid; end
  def set_grid(g, screen); end

end

class Point < Struct.new(:x, :y); end

class Size < Struct.new(:w, :h); end

class Rect < Struct.new(:x, :y, :w, :h)

    def integral!; end
    def inset!; end

end

```
