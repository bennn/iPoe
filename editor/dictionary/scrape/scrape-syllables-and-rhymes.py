import os
import re
import sys
import urllib

from dictionary.scrape import util
# from scrapy.selector import HtmlXPathSelector

def parse_hyphenated(hyphen):
    # Make sure there are no spaces in these words! Else we just ignore them.
    chars, num_syllables = [], 0
    for c in hyphen:
        if ord(c) == 165:
            # That's a hyphen!
            num_syllables += 1
        elif 'A' <= c <= 'Z':
            chars.append(c.lower())
        elif 'a' <= c <= 'z':
            chars.append(c)
    return "".join(chars) , num_syllables

def run(fname):
    """
        2014-08-16:
            Treat each line of the file as a hyphenated word.
            Count the number of hypens as syllables and use rhymebrain.com
            to get the rhymes and almost-rhymes.
    """
    with open(fname, 'r') as f:
        with open(out1, 'w') as out_syll:
            with open(out2, 'w') as out_rhyme:
                for line in f:
                    hyphenated_words = line.rstrip().split(" ")
                    for hyphen in hyphenated_words:
                        print("Processing hyphenated word '%s'..." % hyphen)
                        word, syllables       = parse_hyphenated(hyphen)
                        rhymes, almost_rhymes = util.scrape_rhymebrain(word)
                        print>>out_syll,  ("%s\t%s" % (word, syllables))
                        print>>out_rhyme, ("%s\t%s\t%s" % (word, ",".join(rhymes), ",".join(almost_rhymes)))
                        print("Scraped word '%s' with '%s' syllables '%s' rhymes, and '%s' almost rhymes." % (word, syllables, len(rhymes), len(almost_rhymes)))


if __name__ == "__main__":
    if len(sys.argv) == 2:
        run(sys.argv[1])
    else:
        print("usage: scrape-syllables-and-rhymes <filename>")
