#!/usr/bin/env python2

"""Build lib.mli from build.mllib"""

def generate(mllib, mli):
    mllib = open(mllib, 'r')
    mli = open(mli, 'w')
    for line in mllib.xreadlines():
        line2 = 'module {0} : module type of {0}\n'.format(line.strip())
        mli.write(line2)
    mli.close()
    mllib.close()


if __name__ == '__main__':
    mllib = 'src/lib.mllib'
    mli = 'src/lib.mli'
    print "generate", mli, "from", mllib
    generate(mllib, mli)
