import sys
sys.path.insert(0, '/Users/sdegutis/projects/zephyros/libs/not-ready')
import zeph


@zeph.zephyros
def stuff(zeph):
    zeph.sendMsg([0, 0, 'bind', 'd', ['cmd', 'shift']])
