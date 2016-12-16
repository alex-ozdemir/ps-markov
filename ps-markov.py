import os.path
import os
import random
import string
import sys
import itertools
from flask import Flask, render_template
def windows(it, n):
    its = tuple(it[i:] for i in range(n))
    for t in zip(*its):
        yield t

def load(dirname, n):
    d = {}
    B = []
    for fn in os.listdir(dirname):
        fnn = "%s/%s" % (dirname, fn)
        with open(fnn) as f:
            l = ['']
            for c in f.read().replace('-\n', '').replace('\n',' ').strip():
                if c in string.ascii_letters or c in '\'+-' or c in string.digits:
                    l[-1] = l[-1] + c
                elif c in string.whitespace:
                    l.append('')
                elif c not in string.printable:
                    l.append('')
                else:
                    l.append('')
                    l.append(c)
                    l.append('')
            l = list(filter(lambda s: len(s) > 0, l))
            B.extend(windows(l, n))
            for t in windows(l, n+1):
                if t[:-1] not in d:
                    d[t[:-1]] = {}
                if t[-1] not in d[t[:-1]]:
                    d[t[:-1]][t[-1]] = 0
                d[t[:-1]][t[-1]] += 1
    d2 = {}
    for k in d:
        d2[k] = cdfinv(d[k])
    return (d2, B)


def cdfinv(m):
    s = sum(m.values())
    a = 0
    cdf = ([], [])
    for k in m:
        c = m[k]
        a += c / s
        cdf[0].append(a)
        cdf[1].append(k)
    return cdf

def cdflookup(m, real):
    cdf = m[0]
    outs = m[1]
    i = binsearch(cdf, real)
    return outs[i]

def binsearch(l, real):
    s = 0
    e = len(l)-1
    while e - s > 0:
        if real > l[e]:
            return e
        if real < l[s]:
            return s
        h = (s + e) // 2
        if real == l[h]:
            return h
        elif real < l[h]:
            e = h - 1
        else:
            s = h + 1
    return -1


def fmtpunctuation(s):
    rules = {
            ' .': '.',
            ' ,': ',',
            ' ?': '?',
            ' !': '!',
            ' )': ')',
            '( ': '(',
            ' ;': ';',
            'Ph. D.': 'PhD.'
            }
    for r in rules:
        s = s.replace(r, rules[r])
    return s

def gen(ngrams, d, l):
    seed = random.choice(ngrams)
    out = list(seed)
    while len(out) < l:
        lastk = tuple(out[-n:])
        #while out[-1] != '.':
        if lastk in d:
            out.append(cdflookup(d[lastk], random.random()))
        else:
            reseed = random.choice(ngrams)
            out.extend(reseed)
    if out[-1] != '.':
        out.append('.')
    return fmtpunctuation(' '.join(out))

n = int(sys.argv[1])
l = int(sys.argv[2])
_dir = sys.argv[3]
(d, ngrams) = load(_dir, n)
ngrams = ngrams[:-1]
n = 0
OUTPUT = 'output'

try:
    n = 1 + max(map(int, os.listdir(OUTPUT)))
except:
    pass

app = Flask(__name__)

@app.route("/raw")
def raw():
    global ngrams, d, l, n
    out = gen(ngrams, d, l)
    print('Generated')
    with open('%s/%05d' % (OUTPUT, n), 'w') as f:
        f.write(out)
    print('Written')
    n += 1
    return out

@app.route("/")
def formatted():
    global ngrams, d, l, n
    out = gen(ngrams, d, l)
    print('Generated')
    with open('%s/%05d' % (OUTPUT, n), 'w') as f:
        f.write(out)
    print('Written')
    n += 1
    return render_template('statement.html', statement=out)

app.run(host='134.173.42.100', port=9000)

def bw(d, s):
    D = {}
    for k in d:
        if s in k:
            D[k] = d[k]
    return D
