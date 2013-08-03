from twisted.internet import reactor, protocol
import json

setupper = None


class API:
    def __init__(self, zeph):
        self.zeph = zeph
        self.reifiedMsgIdGen = self.msgIdGen()

    def msgIdGen(self):
        i = 0
        while True:
            i += 1
            yield i

    def bind(self, key, mods, f):
        msgid = self.reifiedMsgIdGen.next()
        self.zeph.sendMsg([msgid, 0, 'bind', key, mods])


class ZephClient(protocol.Protocol):
    def connectionMade(self):
        self.buf = ''
        self.readingSize = None
        self.api = API(self)
        setupper(self.api)

    def sendMsg(self, msg):
        msgStr = json.dumps(msg)
        self.transport.write(str(len(msgStr)) + '\n' + msgStr)

    def dataReceived(self, data):
        self.buf += data
        while self.processIncomingData():
            pass

    def handleMessage(self, msg):
        print 'msg:', msg

    def processIncomingData(self):
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

    def startedConnecting(self, connector):
        print connector


def zephyros(f):
    global setupper
    setupper = f
    factory = ZephClientFactory()
    reactor.connectTCP("localhost", 1235, factory)
    reactor.run()
