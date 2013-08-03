import sys
sys.path.insert(0, '/Users/sdegutis/projects/zephyros/libs/not-ready')
import zeph


@zeph.zephyros
def stuff(zeph):
    def it():
        pass
    zeph.bind('d', ['cmd', 'shift'], it)
