#!/usr/bin/env python3
import json
import sys
from collections import defaultdict
import yaml

P = defaultdict(list)

for n in sys.argv[1:]:
    with open(n) as f:

        x = json.loads(f.read())
        for p in x:
            if "preferred_units" in p and "styles" not in p and "eccharts_layer" not in p:
                pu = p["preferred_units"]
                pm = p["match"]
                P[pu].extend(pm)


for k, v in P.items():
    for m in v:
        l = ["units=%s" % (k.replace("/", "|"),)]
        for p, w in m.items():
            if isinstance(w, list):
                l.append("%s=%s" % (p, "/".join(w)))
            else:
                l.append("%s=%s" % (p, w))
        print(",".join(l))
