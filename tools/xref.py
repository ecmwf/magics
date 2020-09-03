#!/usr/bin/env python3
import os
import xmltodict
import json
from collections import defaultdict

DEFS = {}


def load(n):
    with open(n) as f:
        x = xmltodict.parse(f.read())

    klass = x['magics']['class']
    klass['PATH'] = n

    assert klass['@name'] not in DEFS, (klass['@name'], n, DEFS[klass['@name']])
    DEFS[klass['@name']] = klass

    # if '@inherits' in klass:
    #     return

    # try:
    #     assert False
    #     print(klass['@name'])
    # except:
    #     print(json.dumps(x, indent=4))
    #     exit(1)


for n in sorted(os.listdir('.')):
    if n.endswith('.xml'):  # and n.startswith("SimpleP"):
        load(n)


def action(klass):
    if '@action' in klass:
        return klass['@action']
    if '@inherits' in klass:
        if '/' in klass['@inherits']:
            return [action(x) for x in klass['@inherits'].split('/')]
        return action(DEFS[klass['@inherits']])


PARAMS = defaultdict(set)
for n, klass in DEFS.items():
    parms = klass.get('parameter', [])
    if not isinstance(parms, list):
        parms = [parms]
    for p in parms:

        try:
            PARAMS[p['@name']].add(action(klass))
        except:
            print(klass)
            raise
    # print(n, action(klass))
    # if '@action' in klass:
    #     print(n, json.dumps(klass, indent=4))
# load("Contour.xml")

for p, v in sorted(PARAMS.items()):
    print(p, v)
