class API

  class << self

    def after(sec, &blk)
      $api.doFn_after_ blk, sec
    end

    def bind(key, mods, &blk)
      $keybinder.bind_modifiers_fn_ key, mods, blk
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

  def frame_including_dock_and_menu; frameIncludingDockAndMenu; end
  def frame_without_dock_or_menu; frameWithoutDockOrMenu; end
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

class WindowProxy

  def other_windows_on_same_screen; otherWindowsOnSameScreen; end

  def frame; method_missing(:frame); end
  def top_left; method_missing(:topLeft); end
  def size; method_missing(:size); end

  def frame=(x); method_missing(:setFrame_, x); end
  def top_left=(x); method_missing(:setTopLeft_, x); end
  def size=(x); method_missing(:setSize_, x); end

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

end

class Point

  def x; method_missing(:x); end
  def y; method_missing(:y); end

  def x=(n); method_missing(:setX_, n); end
  def y=(n); method_missing(:setY_, n); end

end

class Size

  def w; method_missing(:w); end
  def h; method_missing(:h); end

  def w=(n); method_missing(:setW_, n); end
  def h=(n); method_missing(:setH_, n); end

end

class Rect

  def x; method_missing(:x); end
  def y; method_missing(:y); end
  def w; method_missing(:w); end
  def h; method_missing(:h); end

  def x=(n); method_missing(:setX_, n); end
  def y=(n); method_missing(:setY_, n); end
  def w=(n); method_missing(:setW_, n); end
  def h=(n); method_missing(:setH_, n); end

end
