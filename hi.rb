require 'socket'
require 'json'
require 'thread'

class Zeph

  def initialize
    @sock = TCPSocket.new 'localhost', 1235
    @id = 0
    @queues = {}

    thread = listen_forever
    at_exit { thread.join }
  end

  def request(*data)
    id = send_raw 'request', data

    val = @queues[id].pop
    @queues.delete id
    return val
  end

  def register(*data, &blk)
    id = send_raw 'register', data

    Thread.new do
      loop do
        event = @queues[id].pop
        blk.call event
      end
    end
  end

  private

  def send_raw(type, data)
    id = @id += 1
    @queues[id] = Queue.new
    json = [type, id].concat(data).to_json
    @sock.write "#{json.size}\n#{json}"
    return id
  end

  def listen_forever
    Thread.new do
      loop do
        size = @sock.gets
        msg = @sock.read(size.to_i)
        val = JSON.load(msg)
        id = val[1]
        @queues[id] << val
      end
    end
  end

end


$zeph = Zeph.new

10.times do |i|

  if i == 5
    $zeph.register 'bind', 'mash+d' do |args|
      p args
    end
  end

  val = $zeph.request 'set_title', 'woot'
  p val
end
