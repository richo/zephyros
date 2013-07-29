#Window = Class.new(WrappedObject)
#App = Class.new(WrappedObject)
#Screen = Class.new(WrappedObject)
#API = Class.new(WrappedObject)


#class << API
#    def foobartest2
#        'sup'
#    end
#end

def after(sec, &blk)
    $api.doFn_after_ blk, sec
end

#p keybinder

$keybinder.bind_modifiers_fn_ "D", ["cmd", "alt", "ctrl"], lambda {puts 'woot'}

p $keybinder.cuz
after 0.5 do puts 'ok' end
