class API
  class << self

    def after(sec, &blk)
      $api.doFn_after_ blk, sec
    end

    def bind(key, mods, &blk)
      $keybinder.bind_modifiers_fn_ key, mods, blk
    end

    def focused_window
      $windowproxy.focusedWindow
    end

    def visible_windows
      $windowproxy.visibleWindows
    end

  end
end
