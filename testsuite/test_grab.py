#! /usr/bin/env python3

from urllib.request import urlopen
from os.path import dirname, join, abspath
import sys
import json
import unittest

sys.path.append(join(dirname(__file__), '../util'))

from grab import extract_data

parpath = abspath(join(dirname(__file__), '..'))

testfile = join(parpath, 'source.html')

resfile = join(parpath, 'result.json')


def get_testdata():
	with open(testfile, 'r', encoding="utf-8") as f:
		return f.read()


def get_resultdata():
	with open(resfile, 'r', encoding='utf-8') as f:
		return json.reads(fp=f)


class TestGrab:

	def test_grab(self):
		content = get_testdata()

		processed = extract_data(content, 4)

		expected = get_resultdata()


		self.assertEqual(expected, processed)


if __name__ == '__main__':
	unittest.main()
