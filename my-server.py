from epc.server import EPCServer
import webbrowser
server = EPCServer(('localhost', 8081))

webbrowser.register('chrome', None,
                webbrowser.BackgroundBrowser("/usr/bin/chromium-browser"))


@server.register_function
def echo(*a):
    return a

@server.register_function
def pow(*x):
    return [i**10 for i in x]

@server.register_function
def browser_open_url(*url):
    assert(len(url) == 1)
    webbrowser.get('chrome').open(url[0])


server.print_port()
server.serve_forever()
