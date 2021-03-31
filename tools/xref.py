#!/usr/bin/env python3
import os
import xmltodict
import yaml
from collections import defaultdict, OrderedDict

yaml.Dumper.ignore_aliases = lambda *args: True

DEFS = {}

T = {"on": True, "off": False, "no": False, "stringarray()": [], "floatarray()": []}


def tidy(x):

    if isinstance(x, (list, tuple)):
        return [tidy(y) for y in x]

    if isinstance(x, (dict, OrderedDict)):
        d = {}
        for k, v in x.items():
            d[tidy(k)] = tidy(v)

        return d

    if isinstance(x, str):
        if x.startswith("@"):
            return x[1:]

    try:
        return float(x)
    except:
        pass

    try:
        return int(x)
    except:
        pass

    x = T.get(x, x)

    if isinstance(x, str):
        return x.strip().replace("\n", " ").replace("\t", " ").replace("  ", " ")

    return x


def load(n):
    with open(n) as f:
        x = xmltodict.parse(f.read())

    print(
        yaml.dump(
            tidy(x)["magics"],
            default_flow_style=False,
            default_style=None,
            canonical=False,
            explicit_start=True,
        )
    )

    klass = x["magics"]["class"]
    klass["PATH"] = n

    assert klass["@name"] not in DEFS, (klass["@name"], n, DEFS[klass["@name"]])
    DEFS[klass["@name"]] = klass

    # if '@inherits' in klass:
    #     return

    # try:
    #     assert False
    #     print(klass['@name'])
    # except:
    #     print(json.dumps(x, indent=4))
    #     exit(1)


root = os.path.join(os.path.dirname(os.path.dirname(__file__)), "src", "params")

for n in sorted(os.listdir(root)):
    if n.endswith(".xml"):  # and n.startswith("SimpleP"):
        load(os.path.join(root, n))


def action(klass):
    if "@action" in klass:
        return klass["@action"]
    if "@inherits" in klass:
        if "/" in klass["@inherits"]:
            return [action(x) for x in klass["@inherits"].split("/")]
        return action(DEFS[klass["@inherits"]])


ACTIONS = defaultdict(set)

PARAMS = defaultdict(set)
for n, klass in DEFS.items():
    parms = klass.get("parameter", [])
    if not isinstance(parms, list):
        parms = [parms]

    a = action(klass)
    if a:
        for p in parms:

            try:
                PARAMS[p["@name"]].add(a)
            except:
                print(klass)
                raise

            ACTIONS[a].add(p["@name"])
    # print(n, action(klass))
    # if '@action' in klass:
    #     print(n, json.dumps(klass, indent=4))
# load("Contour.xml")

# for p, v in sorted(PARAMS.items()):
#     print(p, v)

# for p, v in sorted(ACTIONS.items()):
#     print("def", p,"(")
#     for x in sorted(v):
#         print("   ", x, "=None,")
#     print("    ):")
#     print("         pass")
#     print()
