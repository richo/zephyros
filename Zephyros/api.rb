#class << API
#    def foobartest2
#        'sup'
#    end
#end

def after(sec, &blk)
    $api.doFn_after_ blk, sec
end

p $keybinder
p $api

$keybinder.bind_modifiers_fn_ "D", ["cmd", "alt", "ctrl"], lambda {puts 'woot'}

after 0.5 do puts 'ok' end
