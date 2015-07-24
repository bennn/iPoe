import glob

# Collect scraped syllable information into a single, large, SQL file

def run():
  seen = {}
  with open("syllables.sql", "w") as f:
    for filename in glob.glob("../../scrape/output*/word-syllable*"):
      print("Processing '%s'" % filename)
      with open(filename, "r") as g:
        for line in g:
          word, syll = line.strip().split("\t")
          if word not in seen:
            seen[word] = syll
            print("('%s', %s)," % (word, syll), file=f)
          elif syll != seen[word]:
            print("WARNING: conflict at '%s', have %s syllables or %s syllables" % (word, syll, seen[word]))

if __name__ == "__main__":
  run()
