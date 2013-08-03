from twisted.internet import reactor, protocol
from Queue import Queue
from threading import Thread
import json

setupper = None
# ONEYEAR = 365 * 24 * 60 * 60

def interruptable_get(q):
    return q.get()
    # while True:
    #     try:
    #         return q.get(timeout=1000)
    #     except Queue.Empty:
            # pass

class API:
    def __init__(self, zeph):
        self.zeph = zeph

    def alert(self, msg, dur = 1):
        self.zeph.sendMsg([0, 'alert', msg, dur])

    def bind(self, key, mods, f):
        def tempFn(arg):
            f()
        self.zeph.sendMsg([0, 'bind', key, mods], fn = tempFn)


class ZephClient(protocol.Protocol):
    def connectionMade(self):
        self.reifiedMsgIdGen = self.msgIdGen()
        self.buf = ''
        self.readingSize = None
        self.api = API(self)
        self.queues = {}
        def tmpFn():
            setupper(self.api)
        t = Thread(target = tmpFn)
        t.start()

    def msgIdGen(self):
        i = 0
        while True:
            i += 1
            yield i

    def sendMsg(self, msg, infinite=True, fn=None):
        msgid = self.reifiedMsgIdGen.next()
        self.queues[msgid] = Queue(10)

        msg.insert(0, msgid)

        print "sending", msg

        msgStr = json.dumps(msg)
        self.transport.write(str(len(msgStr)) + '\n' + msgStr)

        if fn is not None:
            def tmpFn():
                val = self.queues[msgid].interruptable_get()
                if infinite:
                    while True:
                        val = self.queues[msgid].interruptable_get()
                        fn(val)
                else:
                    val = self.queues[msgid].interruptable_get()
                    fn(val)
                del self.queues[msgid]
            t = Thread(target = tmpFn)
            t.start()
        else:
            q = self.queues[msgid]
            print "about to get from", q
            # val = q.get()
            print "just got"
            # del self.queues[msgid]
            # return val

    def dataReceived(self, data):
        print "data recv"
        self.buf += data
        while self.processIncomingData():
            pass

    def handleMessage(self, msg):
        print "about to send"
        def tmpFn():
            msgid = msg[0]
            obj = msg[1]
            q = self.queues[msgid]
            print "sending", q, msgid, obj
            q.put(obj)
        t = Thread(target = tmpFn)
        t.start()
        # print 'msg:', msgid, msg

    def processIncomingData(self):
        print "read data", self.buf
        if self.readingSize:
            l = len(self.buf)
            if l >= self.readingSize:
                msg, self.buf = self.buf[:l], self.buf[l:]
                obj = json.loads(msg)
                self.readingSize = None
                self.handleMessage(obj)
                return True
        else:
            idx = self.buf.find('\n')
            if idx != -1:
                self.readingSize, self.buf = int(self.buf[:idx]), self.buf[idx+1:]
                return True

        return False

class ZephClientFactory(protocol.ClientFactory):
    protocol = ZephClient


def zephyros(f):
    global setupper
    setupper = f
    factory = ZephClientFactory()
    reactor.connectTCP("localhost", 1235, factory)
    reactor.run()
