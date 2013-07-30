## Zephyros Ruby API

Ruby Version: 2.0

```ruby
class API

  class << self

    def after(sec, &blk); end
    def log(str); end
    def shell(path, args, opts={}); end # TODO: document opts
    def open(thing); end
    def bind(key, mods, &blk); end
    def alert(msg); end

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

Point = Struct.new(:x, :y)
Size = Struct.new(:w, :h)
Rect = Struct.new(:x, :y, :w, :h)
```
