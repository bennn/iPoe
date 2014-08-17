# Word processing utilities. Shared between scraper and poem views.
import os

from scrapy.selector import HtmlXPathSelector


TMPFILE  = "./rhymebrain.html"

def normalize_word(word):
    # Remove punctuation from the string [word].
    return "".join((c for c in word.strip().lower() if 'a' <= c <= 'z'))

def scrape_rhymebrain(word):
    url = "http://rhymebrain.com/en/What_rhymes_with_%s.html" % word
    os.system("curl -s '%s' > %s" % (url, TMPFILE))
    hxs = HtmlXPathSelector(text=open(TMPFILE, 'r').read())
    hdr = hxs.select("//div[@id = 'results']/h4")
    rhymes = [normalize_word(x) for x in hdr.select("./preceding-sibling::span[@class = 'wordpanel']/text()").extract()]
    almost = [normalize_word(x) for x in hdr.select("./following-sibling::span[@class = 'wordpanel']/text()").extract()]
    os.system("rm %s" % TMPFILE)
    if almost:
        return rhymes, almost
    else:
        return [normalize_word(x) for x in hxs.select("//div[@id = 'results']/span/text()").extract()], []

