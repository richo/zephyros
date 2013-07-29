SDWindow = Class.new(WrappedObject)
SDApp = Class.new(WrappedObject)
SDScreen = Class.new(WrappedObject)
SDAPI = Class.new(WrappedObject)

def after(sec, &blk)
    $internal.doFn_after_ blk, sec
end
