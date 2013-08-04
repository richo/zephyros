require 'socket'
require 'json'
require 'thread'

def wait_on_callbacks
  $zeph.wait_on_callbacks
end

class Zeph

  def initialize
    @sock = TCPSocket.new 'localhost', 1235
    @id = 0
    @queues = {}

    trap("SIGINT") { exit }
    @listen_thread = listen_forever
  end

  def wait_on_callbacks
    @listen_thread.join
  end

  def send_message(data, &blk)
    id = @id += 1
    @queues[id] = Queue.new
    json = [id].concat(data).to_json
    @sock.write "#{json.size}\n#{json}"

    if blk.nil?
      o = @queues[id].pop
      @queues.delete(id)
      return o
    else
      Thread.new do
        num_future_calls = @queues[id].pop

        loopblk = lambda do
          event = @queues[id].pop
          blk.call event
        end

        if num_future_calls > 0
          num_future_calls.times { loopblk.call }
        else
          loop { loopblk.call }
        end

        @queues.delete(id)
      end
      return nil
    end
  end

  private

  def listen_forever
    Thread.new do
      loop do
        size = @sock.gets
        msg = @sock.read(size.to_i)
        j = JSON.load(msg)
        id = j[0]
        obj = j[1]
        @queues[id].push obj
      end
    end
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

  def integral!
    self.x = self.x.floor
    self.y = self.y.floor
    self.w = self.w.ceil
    self.h = self.h.ceil
    self
  end

  def min_x; x; end
  def min_y; y; end
  def max_x; x + w; end
  def max_y; y + h; end

end

module ZephProxy

  def method_missing(*args, &blk)
    $zeph.send_message [id, *args], &blk
  end

end

module PatchAdams

  def patch_return(name, interceptor, &blk)
    define_method(name) do |*args|
      super(*args, &blk).instance_exec(&interceptor)
    end
  end

  def patch_args(name, interceptor, &blk)
    define_method(name) do |*args|
      super(*args.map{|arg| arg.instance_exec(&interceptor)}, &blk)
    end
  end

end








class API

  class << self

    define_method(:id) { 0 }

    include ZephProxy
    extend PatchAdams

    patch_return(:focused_window, -> { Window.new self })
    patch_return(:visible_windows, -> { map { |o| Window.new o } })
    patch_return(:all_windows, -> { map { |o| Window.new o } })

    patch_return(:main_screen, -> { Screen.new self })
    patch_return(:all_screens, -> { map { |o| Screen.new o } })

    patch_return(:running_apps, -> { map { |o| App.new o } })

    def alert(msg, sec=2)
      super(msg, sec)
    end

    # ### Just realized we never coerce the arg into a Ruby class. Oops.
    # def listen(event, &blk)
    # end

  end

end

class App < Struct.new(:id)

  include ZephProxy
  extend PatchAdams

  patch_return(:all_windows, -> { map { |o| Window.new o } })
  patch_return(:visible_windows, -> { map { |o| Window.new o } })

end

class Screen < Struct.new(:id)

  include ZephProxy
  extend PatchAdams

  patch_return(:next_screen, -> { Screen.new self })
  patch_return(:previous_screen, -> { Screen.new self })

  patch_return(:frame_including_dock_and_menu, -> { Rect.from_hash self })
  patch_return(:frame_without_dock_or_menu, -> { Rect.from_hash self })

end

$window_grid_width = 3
$window_grid_height = 2
$window_grid_margin_x = 5
$window_grid_margin_y = 5

class Window < Struct.new(:id)

  include ZephProxy
  extend PatchAdams

  patch_return(:frame, -> { Rect.from_hash self })
  patch_return(:top_left, -> { Point.from_hash self })
  patch_return(:size, -> { Size.from_hash self })

  patch_return(:screen, -> { Screen.new self })
  patch_return(:app, -> { App.new self })

  patch_return(:other_windows_on_same_screen, -> { map { |o| Window.new o } })

  define_method(:frame=) { |f| set_frame f.to_hash }
  define_method(:top_left=) { |f| set_top_left f.to_hash }
  define_method(:size=) { |f| set_size f.to_hash }

  def get_grid
    win_frame = self.frame
    screen_rect = self.screen.frame_without_dock_or_menu
    third_screen_width = screen_rect.w / $window_grid_width.to_f
    half_screen_height = screen_rect.h / $window_grid_height.to_f
    Rect.make(((win_frame.x - screen_rect.min_x) / third_screen_width).round,
              ((win_frame.y - screen_rect.min_y) / half_screen_height).round,
              [(win_frame.w.round / third_screen_width).round, 1].max,
              [(win_frame.h.round / half_screen_height).round, 1].max)
  end

  def set_grid(g, screen)
    screen = screen || self.screen
    screen_rect = screen.frame_without_dock_or_menu
    third_screen_width = screen_rect.w / $window_grid_width.to_f
    half_screen_height = screen_rect.h / $window_grid_height.to_f
    new_frame = Rect.make((g.x * third_screen_width) + screen_rect.min_x,
                          (g.y * half_screen_height) + screen_rect.min_y,
                          g.w * third_screen_width,
                          g.h * half_screen_height)
    new_frame.inset!($window_grid_margin_x.to_f, $window_grid_margin_y.to_f)
    new_frame.integral!
    self.frame = new_frame
  end

end










$zeph = Zeph.new
