#-*- encoding: utf-8 -*-

import os
import re

import requests
from BeautifulSoup import BeautifulSoup as BS

URL_51VOA = "http://www.51voa.com"
URL_51VOA_STANDARD = "/VOA_Standard_English/"
URL_51VOA_SPECIAL  = "/VOA_Special_English/"

FILE_ROOT = "/home/kdr2/Resources/VOA"

def file_exists(f):
    try:
        os.stat(f)
        return True
    except:
        return False

def item_info(item):
    links = item.findAll("a")
    if(len(links) == 1): # Standard
        a = links[-1]
        return (a.attrMap["href"], a.text, None)
    a = links[-1]
    return (a.attrMap["href"], a.text, links[0].text)

def parse_text(content):
    date = content.findAll(attrs={"class":"datetime"})
    if len(date)>0:
        date = date[0]
    else:
        date = content.findAll(attrs={"class":"byline"})
        date = date[0]

    ret = ""
    n = date.nextSibling

    while n:
        name = getattr(n, 'name', '').lower()
        #if name == "div":
        #    break
        if name == "br":
            ret += "\n"
        elif name == "p":
            ret += "\n%s\n" % n.text
        elif name == "a":
            ret += n.text
        else:
            try:
                ret += n
            except:
                ret += n.text
        #endif
        n = n.nextSibling
    return ret.encode("utf-8")

def get_text(url):
     r = requests.get(url)
     if r.ok:
         return r.text[4:].encode("iso-8859-1")
     raise Exception("Bad Request")

def get_list(text):
    bs0 = BS(text)
    ul = bs0.findAll(id="list")[0].ul
    lst = ul.findAll("li")
    ret = [item_info(l) for l in lst]
    return ret


def download_item(item):

    #0. prepare info
    url = URL_51VOA + item[0]
    title = item[1]
    r0=re.compile("(.*?)\s+\((\d+)-(\d+)-(\d+)\)")
    m = r0.match(title)
    if not m: return
    m = m.groups()
    cat = item[2].strip(" []") if item[2] else "Standard"
    catname = "VOA-Special" if item[2] else "VOA-Standard"
    print(m[0])
    title = u"[%04d%02d%02d][%s]%s" % (
        int(m[1]), int(m[2]), int(m[3]),
        cat, m[0]
    )

    dirname = "%s/%04d%02d" % (catname, int(m[1]), int(m[2]))
    dirname = os.path.join(FILE_ROOT, dirname)
    if not file_exists(dirname): os.makedirs(dirname, 0755)

    filename = os.path.join(dirname, "%s.txt" % title)
    mp3file =  os.path.join(dirname, "%s.mp3" % title).replace('"',"'")

    if(file_exists(filename) and file_exists(mp3file)):
        return

    #1. get page text
    r = requests.get(url)
    if not r.ok: return None

    bs0 = BS(r.text[4:].encode("iso-8859-1"))

    mp3 = bs0.findAll(id="mp3")[0]
    mp3= mp3.attrMap["href"]
    print "start download  %s " % mp3
    os.system((u"curl %s --output \"%s\"" % (mp3, mp3file)).encode("utf-8"))
    f = file(filename, 'w')
    title = bs0.findAll('title')[0].text
    byline = bs0.findAll(attrs={"class":"byline"})
    author = byline[0].text if byline else "Unknow Author"
    datetime = bs0.findAll(attrs={"class":"datetime"})
    date = datetime[0].text if datetime else "Unknow Date"
    f.write((u"%s\n%s, %s\n" % (title, author, date)).encode("utf-8"))

    content = bs0.findAll(attrs={"id":"content"})[0]
    f.write(parse_text(content))
    f.close()
    return 0

a = get_list(get_text(URL_51VOA + URL_51VOA_STANDARD))
for x in a:download_item(x)

a = get_list(get_text(URL_51VOA + URL_51VOA_SPECIAL))
for x in a:download_item(x)
