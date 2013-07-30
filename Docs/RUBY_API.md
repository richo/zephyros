## Zephyros Ruby API

```ruby
class API

  class << self

    def after(sec, &blk); end
    def listen(event, &blk); end
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

  def frame; end
  def top_left; end
  def size; end

  def frame=(x); end
  def top_left=(x); end
  def size=(x); end

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
class Rect < Struct.new(:x, :y, :w, :h); end
```
