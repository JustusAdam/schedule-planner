#!/usr/bin/env python3

from urllib.request import *
import re
import sys
import os
import json

grab_table_regex = re.compile('<h1>4\. Semester</h1>.*?<table>(.*?)</table>', flags=re.DOTALL)
tr_regex = re.compile('<tr>(.*?)</tr>', flags=re.DOTALL)
td_regex = re.compile('<td>(.*?)</td>', flags=re.DOTALL)
a_regex = re.compile('<a .*?">(.*?)</a>', flags=re.DOTALL)
br_regex = re.compile(' (.*?)<br ?/>')


days = {
    "Montag": 1,
    "Dienstag": 2,
    "Mittwoch": 3,
    "Donnerstag": 4,
    "Freitag": 5,
    "Samstag": 6,
    "Sonntag": 7
}


def apply_each(func, vals):
    return tuple(func(v) for v in vals)


def main():
    file = sys.argv[1]

    content = urlopen('http://web.inf.tu-dresden.de/Fak/ss/15/studiengang/studiengang_inf_bach.html').read().decode()

    table = grab_table_regex.search(content).group(1)

    _, *lessons = [a.group(1) for a in list(re.finditer(tr_regex, table))]

    lessons = list(
        list(
            a.group(1)
            for a in list(re.finditer(td_regex, l))
        )
        for l in lessons
    )

    def splitter(s, a, b):
        for s1 in s.split(a):
            for s2 in s1.split(b):
                s2 = s2.replace(' ', '')
                if s2:
                    yield s2


    def l():
        for a in lessons:
            nmatch = re.search(a_regex, a[0])
            if nmatch is None:
                nmatch = br_regex.match(a[0])
            name = nmatch.group(1)

            s = apply_each(lambda b: list(splitter(b, '<br />', '<br/>')), a[6:9])

            for kind, day, slot in zip(*s):
                if kind == "U":
                    yield dict(
                        subject=name,
                        day=days[day.replace(' ', '')],
                        slot=int(slot.replace('.', '').replace(' ', ''))
                    )

    lessons = list(l())

    if os.path.exists(file):
        mode = 'w'
        with open(file, mode='r') as f:
            content = json.load(f)
    else:
        mode = 'w+'
        content = {
            'rules': [],
            'lessons': []
        }
    content['lessons'] = lessons
    with open(file, mode=mode) as f:
        json.dump(content, fp=f, indent=4)

if __name__ == '__main__':
    main()
