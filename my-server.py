from epc.server import EPCServer
import logging
import webbrowser
server = EPCServer(('localhost', 8081))


@server.register_function
def echo(*a):
    return a


@server.register_function
def pow(*x):
    return [i**10 for i in x]


@server.register_function
def browser_open_url(*url):
    assert (len(url) == 1)
    webbrowser.get('chrome').open(url[0])


@server.register_function
def set_browser_bin(*bin):
    assert len(bin) == 1
    bin = bin[0]
    webbrowser.register('chrome', None, webbrowser.BackgroundBrowser(bin))
    logging.warning("Set browser to %s", bin)


server.print_port()
server.serve_forever()
