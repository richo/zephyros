class API

  class << self

    def after(sec, &blk)
      $api.doFn_after_ blk, sec
    end

    def bind(key, mods, &blk)
      $keybinder.bind_modifiers_fn_ key, mods, blk
    end

    def alert(msg)
      $alert.show_delay_ msg, nil
    end

    def focused_window; $windowproxy.focusedWindow; end
    def visible_windows; $windowproxy.visibleWindows; end
    def all_windows; $windowproxy.allWindows; end

    def main_screen; $screenproxy.mainScreen; end
    def all_screens; $screenproxy.allScreens; end

    def running_apps; $appproxy.runningApps; end

  end

end

class ScreenProxy

  def frame_including_dock_and_menu; Rect.from_hash frameIncludingDockAndMenu; end
  def frame_without_dock_or_menu; Rect.from_hash frameWithoutDockOrMenu; end
  def next_screen; nextScreen; end
  def previous_screen; previousScreen; end

end

class AppProxy

  def all_windows; allWindows; end
  def visible_windows; visibleWindows; end

  def title; method_missing :title; end
  def hidden?; isHidden; end

  def show; method_missing :show; end
  def hide; method_missing :hide; end

  def kill; method_missing :kill; end
  def kill9; method_missing :kill9; end

end

$window_grid_width = 3
$window_grid_margin_x = 5
$window_grid_margin_y = 5

class WindowProxy

  def other_windows_on_same_screen; otherWindowsOnSameScreen; end

  def frame; Rect.from_hash method_missing(:frame); end
  def top_left; Point.from_hash method_missing(:topLeft); end
  def size; Size.from_hash method_missing(:size); end

  def frame=(x); method_missing(:setFrame_, x.to_hash); end
  def top_left=(x); method_missing(:setTopLeft_, x.to_hash); end
  def size=(x); method_missing(:setSize_, x.to_hash); end

  def maximize; method_missing(:maximize); end
  def minimize; method_missing(:minimize); end
  def un_minimize; unMinimize; end

  def screen; method_missing(:screen); end
  def app; method_missing(:app); end

  def focus_window; focusWindow; end
  def focus_window_left; focusWindowLeft; end
  def focus_window_right; focusWindowRight; end
  def focus_window_up; focusWindowUp; end
  def focus_window_down; focusWindowDown; end

  def normal_window?; isNormalWindow; end
  def minimized?; isWindowMinimized; end

  def title; method_missing(:title); end

  def get_grid
    win_frame = self.frame
    screen_rect = self.screen.frame_without_dock_or_menu
    third_screen_width = screen_rect.w / $window_grid_width
    half_screen_height = screen_rect.h / 2.0
    Rect.make(((win_frame.x - screen_rect.min_x) / third_screen_width).round,
              ((win_frame.y - screen_rect.min_y) / half_screen_height).round,
              [(win_frame.w.round / third_screen_width).round, 1].max,
              [(win_frame.h.round / half_screen_height).round, 1].max)
  end

  def set_grid(g, screen)
    screen = screen || self.screen
    screen_rect = screen.frame_without_dock_or_menu
    third_screen_width = screen_rect.w / $window_grid_width
    half_screen_height = screen_rect.h / 2.0
    new_frame = Rect.make((g.x * third_screen_width) + screen_rect.min_x,
                          (g.y * half_screen_height) + screen_rect.min_y,
                          g.w * third_screen_width,
                          g.h * half_screen_height)
    new_frame.inset!($window_grid_margin_x, $window_grid_margin_y)
    new_frame.integral!
    self.frame = new_frame
  end

end

class Point < Struct.new(:x, :y)

  def self.from_hash(d)
    r = new
    r.x = d['x']
    r.y = d['y']
    r
  end

  def to_hash
    {
      'x' => x,
      'y' => y,
    }
  end

  def initialize
    self.x = 0
    self.y = 0
  end

end

class Size < Struct.new(:w, :h)

  def self.from_hash(d)
    r = new
    r.w = d['w']
    r.h = d['h']
    r
  end

  def to_hash
    {
      'w' => w,
      'h' => h,
    }
  end

  def initialize
    self.w = 0
    self.h = 0
  end

end

class Rect < Struct.new(:x, :y, :w, :h)

  def self.from_hash(d)
    r = new
    r.x = d['x']
    r.y = d['y']
    r.w = d['w']
    r.h = d['h']
    r
  end

  def to_hash
    {
      'x' => x,
      'y' => y,
      'w' => w,
      'h' => h,
    }
  end

  def initialize
    self.x = 0
    self.y = 0
    self.w = 0
    self.h = 0
  end

  def self.make(x, y, w, h)
    r = Rect.new
    r.x = x
    r.y = y
    r.w = w
    r.h = h
    r
  end

  def inset!(x, y)
    self.x += x
    self.y += y
    self.w -= (x * 2)
    self.h -= (y * 2)
    self
  end

  def min_x; x; end
  def min_y; y; end
  def max_x; x + w; end
  def max_y; y + h; end

end
