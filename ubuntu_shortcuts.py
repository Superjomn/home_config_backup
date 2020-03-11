#!/usr/bin/python3
import fire
import subprocess
import logging


def run_shell_command(args):
    logging.warning('running command: ' + ' '.join(args))
    process = subprocess.Popen(args,
                               stdout=subprocess.PIPE,
                               universal_newlines=True)

    while True:
        output = process.stdout.readline()
        print(output.strip())
        # Do something else
        return_code = process.poll()
        if return_code is not None:
            print('RETURN CODE', return_code)
            # Process has finished, read rest of the output
            for output in process.stdout.readlines():
                print(output.strip())
            break


class Ubuntu(object):
    apt_ipv4 = '-o Acquire::ForceIPv4=true'

    def hello(self, name="World"):
        return "Hello %s" % name

    def install(self, pkg):
        args = ['sudo', 'apt', 'install', pkg, '-y', Ubuntu.apt_ipv4]
        run_shell_command(args)

    def update(self):
        args = ['sudo', 'apt', 'update', Ubuntu.apt_ipv4]
        run_shell_command(args)


if __name__ == '__main__':
    fire.Fire(Ubuntu)
