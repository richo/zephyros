class API
    def self.after(sec, &blk)
        $api.doFn_after_ blk, sec
    end
    
    def self.bind(key, mods, &blk)
        $keybinder.bind_modifiers_fn_ key, mods, blk
    end
end
