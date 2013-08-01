require 'socket'
require 'json'
require 'thread'

class Zeph

  def initialize
    @sock = TCPSocket.new 'localhost', 1235
    @id = 0
    @queues = {}

    thread = listen_forever
    at_exit do
      thread.join
      @sock.close
    end
  end

  def send(*data)
    id = write 'request', data

    val = @queues[id].pop
    @queues.delete id
    return val
  end

  def register(*data, &blk)
    id = write 'register', data

    thread = Thread.new do
      loop do
        event = @queues[id].pop
        blk.call event
      end
    end
    at_exit { thread.join }
  end

  private

  def write(type, data)
    id = @id += 1
    @queues[id] = Queue.new
    json = ['request', id].concat(data).to_json
    @sock.write "#{json.size}\n#{json}"
    return id
  end

  def listen_forever
    Thread.new do
      loop do
        val = get
        id = val[1]
        @queues[id] << val
      end
    end
  end

  def get
    size = @sock.gets
    puts "size is #{size.inspect}"
    msg = @sock.read(size.to_i)
    puts "msg is #{msg.inspect}"
    JSON.load(msg)
  end

end


$zeph = Zeph.new

10.times do |i|

  if i == 5
    $zeph.register 'bind', 'mash+d' do |args|
      p args
    end
  end

  val = $zeph.send 'set_title', 'woot'
  p val
end
