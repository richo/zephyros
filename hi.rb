require 'socket'
require 'json'
require 'thread'

Window = Struct.new(:id)
Screen = Struct.new(:id)
App = Struct.new(:id)

class Object
  def converted
    if is_a?(Array)
      map(&:converted)
    elsif is_a?(Hash) && has_key?('_type')
      klass = Kernel.const_get(self['_type'])
      klass.new(self['_id'])
    else
      self
    end
  end
end

class Zeph

  def initialize
    @sock = TCPSocket.new 'localhost', 1235
    @id = 0
    @queues = {}

    thread = listen_forever
    at_exit { thread.join }
  end

  def request(data)
    id = send_raw data

    val = @queues[id].pop
    @queues.delete id
    return val[1].converted
  end

  def register(data, blk)
    id = send_raw data

    Thread.new do
      loop do
        event = @queues[id].pop
        blk.call event[1].converted
      end
    end
  end

  private

  def send_raw(data)
    id = @id += 1
    @queues[id] = Queue.new
    json = [id].concat(data).to_json
    @sock.write "#{json.size}\n#{json}"
    return id
  end

  def listen_forever
    Thread.new do
      loop do
        size = @sock.gets
        msg = @sock.read(size.to_i)
        val = JSON.load(msg)
        id = val[0]
        @queues[id] << val
      end
    end
  end

end

$zeph = Zeph.new



class API

  class << self

    def all_windows
      $zeph.request [0, 'all_windows']
    end

    def bind(key, mods, &blk)
      $zeph.register [0, 'bind', key, mods], blk
    end

  end

end



10.times do |i|

  if i == 5
    API.bind 'd', ['cmd', 'opt'] do |args|
      p args
    end
  end

  p API.all_windows

end
