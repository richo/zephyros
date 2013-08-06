import socket
import threading
import json
import Queue
import sys
import atexit
import itertools






sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect(('127.0.0.1', 1235))

raw_message_queue = Queue.Queue(10)
reified_msg_id_gen = itertools.count()
send_data_queue = Queue.Queue(10)
individual_message_queues = {}


def run_in_background(fn):
    t = threading.Thread(target=fn)
    t.daemon = True
    t.start()

@run_in_background
def read_forever():
    while True:
        len_str = ''
        while True:
            in_str = sock.recv(1)
            if in_str == '\n':
                 break
            len_str += in_str

        len_num = int(len_str)
        data = ''
        while len(data) < len_num:
            new_data = sock.recv(len_num)
            len_num -= len(new_data)
            data += new_data

        obj = json.loads(data)
        raw_message_queue.put(obj)

@run_in_background
def send_data_fully():
    while True:
        data = send_data_queue.get()
        while len(data) > 0:
            num_wrote = sock.send(data)
            data = data[num_wrote:]


def send_message(msg, infinite=True, callback=None):
    msgId = reified_msg_id_gen.next()
    temp_send_queue = Queue.Queue(10)
    individual_message_queues[msgId] = temp_send_queue

    msg.insert(0, msgId)
    msg_str = json.dumps(msg)
    send_data_queue.put(str(len(msg_str)) + '\n' + msg_str)

    if callback is not None:
        def temp_fn():
            temp_send_queue.get() # ignore first
            if infinite:
                while True:
                    obj = temp_send_queue.get()
                    callback(obj[1])
            else:
                obj = temp_send_queue.get()
                callback(obj[1])
        run_in_background(temp_fn)
        return None
    else:
        return temp_send_queue.get()[1]


@run_in_background
def dispatch_individual_messages_forever():
    while True:
        msg = raw_message_queue.get()
        msg_id = msg[0]
        this_queue = individual_message_queues[msg_id]
        this_queue.put(msg)

def zephyros(fn):
    run_in_background(fn)
    try:
        while True: pass
    except KeyboardInterrupt:
        pass


class Proxy:
    def __init__(self, id): self.id = id
    def _send_sync(self, *args): return send_message([self.id] + list(args))

class Window(Proxy):
    def title(self): return self._send_sync('title')
    def frame(self): return self._send_sync('frame')
    def set_frame(self, f): self._send_sync('set_frame', f)

class Screen(Proxy):
    pass

class App(Proxy):
    pass

class Api(Proxy):
    def alert(self, msg, duration=1): self._send_sync('alert', msg, duration)
    def log(self, msg): self._send_sync('log', msg)
    def relaunch_config(self): self._send_sync('relaunch_config')
    def clipboard_contents(self): return self._send_sync('clipboard_contents')
    def focused_window(self): return Window(self._send_sync('focused_window'))
    def visible_windows(self): return map(Window, self._send_sync('visible_windows'))
    def all_windows(self): return map(Window, self._send_sync('all_windows'))
    def main_screen(self): return Screen(self._send_sync('main_screen'))
    def all_screens(self): return map(Screen, self._send_sync('all_screens'))
    def running_apps(self): return map(App, self._send_sync('running_apps'))
    def bind(self, key, mods, fn):
        def tmp_fn(obj): fn()
        send_message([0, 'bind', key, mods], callback=tmp_fn)
    def choose_from(self, lst, title, lines, chars, fn):
        send_message([0, 'choose_from', lst, title, lines, chars], callback=fn, infinite=False)
    def listen(self, event, fn):
        def tmp_fn(obj):
            if event == "window_created":       fn(Window(obj))
            elif event == "window_minimized":   fn(Window(obj))
            elif event == "window_unminimized": fn(Window(obj))
            elif event == "window_moved":       fn(Window(obj))
            elif event == "window_resized":     fn(Window(obj))
            elif event == "app_launched":       fn(App(obj))
            elif event == "app_died":           fn(App(obj))
            elif event == "app_hidden":         fn(App(obj))
            elif event == "app_shown":          fn(App(obj))
            elif event == "screens_changed":    fn()
        send_message([0, 'listen', event], callback=tmp_fn)

api = Api(0)
