#!/usr/bin/env python3

from urllib.request import *
import re
import sys
import os
import json
from itertools import count

grab_table_regex = lambda semester: re.compile('<h1>{}\. Semester</h1>.*?<table>(.*?)</table>'.format(semester), flags=re.DOTALL)
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


def splitter(s, a, b):
    for s1 in s.split(a):
        for s2 in s1.split(b):
            s2 = s2.replace(' ', '')
            if s2:
                yield s2


def extract_data(string, semester):
    table = grab_table_regex(semester).search(string).group(1)

    _, *raw_lessons = [a.group(1) for a in re.finditer(tr_regex, table)]

    lessons = (
        a.group(1)
        for a in re.finditer(td_regex, l)        
        for l in raw_lessons
    )

    def l():
        for a in lessons:
            nmatch = re.search(a_regex, a[0])
            if nmatch is None:
                nmatch = br_regex.match(a[0])
            name = nmatch.group(1)

            vl_numbers = count(1)

            s = list(map(lambda b: list(splitter(b, '<br />', '<br/>')), a[6:9]))

            for kind, day, slot in zip(*s):    
                
                yield dict(
                    subject=name + (" VL{}".format(next(vl_numbers)) if kind == "V" else " UE"),
                    day=days[day.replace(' ', '')],
                    slot=int(slot.replace('.', '').replace(' ', ''))
                )

    return list(l())


def main():
    import argparse

    parser = argparse.ArgumentParser()

    parser.add_argument(
        '-o', '--output', type=str, default='out.json'
    )
    parser.add_argument(
        'semester', type=int
    )

    args = parser.parse_args()

    file = args.output

    content = urlopen('http://web.inf.tu-dresden.de/Fak/ss/15/studiengang/studiengang_inf_bach.html').read().decode()

    lessons = extract_data(content, args.semester)

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
