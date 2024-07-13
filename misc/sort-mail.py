#!/usr/bin/python3

from mailbox import mbox

targets = [
    "28 Ellesmere Road Purchase",
    "3-mobile",
    "Anglian Windows",
    "Clojure",
    "Conveyancing Quotes",
    "CTCCambridge",
    "CTCCambridgeRoutes",
    "CTCOxford",
    "Dad's Estate",
    "Dad's Memorial",
    "Dad's Memorial Service",
    "Facebook",
    "Golang",
    "GreenMetropolis",
    "LibDems",
    "Nationwide",
    "OkCupid",
    "Pseudospam",
    "Riverford",
    "RussianDatingScam",
    "Sanger",
    "SmileBanking",
    "UKUUG",
    "Virgin Wines",
    "Personal",
    "Sent",
    "Inbox",
    "Archived",
    "Spam",
    "Bin",
]

def target(m):
    if "X-Gmail-Labels" in m:
        labels = m["X-Gmail-Labels"].split(",")
        for t in targets:
            if t in labels:
                return t
    return "Uncategorized"    


incoming = mbox("/home/ray/Mail/Gmail.mbox", create=False)

destinations = {}

n = 0
for m in incoming:
    t = target(m)
    if t not in destinations:
        destinations[t] = mbox(f"/home/ray/Mail/GMail/{t}", create=True)
    destinations[t].add(m)

for d in destinations:
    d.flush()

