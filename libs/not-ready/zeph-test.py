import sys
sys.path.insert(0, '/Users/sdegutis/projects/zephyros/libs/not-ready')
import zeph


@zeph.zephyros
def stuff(zeph):
    # def it():
    #     print "here i am d"
    #     pass
    # def it2():
    #     print "here i am f"
    #     pass
    # zeph.bind('d', ['cmd', 'shift'], it)
    # zeph.bind('f', ['cmd', 'shift'], it2)
    zeph.alert('hello me')
