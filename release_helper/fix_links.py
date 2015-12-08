#!/usr/bin/python

import sys

from bs4 import BeautifulSoup

doc = open( sys.argv[1], 'r')
soup = BeautifulSoup( doc , 'lxml')
for a in soup.findAll('a'):
    linkstr = a['href']
    ind = linkstr.find('#')

    if ind == -1:
       ind = len(linkstr) 

    # don't do it for web-links
    # or for pure hashtags
    if linkstr[:4] != "http" and linkstr[0] != "#":
        a['href'] = linkstr[:ind] + '.html' + linkstr[ind:]

doc.close()

#html = soup.prettify(formatter="html")
html = str(soup)
with open( sys.argv[1], 'wb' ) as file:
    file.write(html)
