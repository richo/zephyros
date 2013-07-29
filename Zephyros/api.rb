puts "fine"

class Window < ObjcWrapper
end

class App < ObjcWrapper
end

class Screen < ObjcWrapper
end


def doit
    puts @something.class
end