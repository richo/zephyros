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

  end

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
