from dictionary.models import Word
from dictionary.scrape.util import normalize_word

ENGLISH_VOWELS = ['a', 'e', 'i', 'o', 'u', 'y']

class Rhyme:
    SOUND = None
    def __init__(self, word=""):
        try:
            w = Word.objects.get(name=word)
            self.SOUND = None # w.phonic_set.related. TODO
        except Word.DoesNotExist:
            self.SOUND = None

    def __str__(self):
        return self.SOUND
        
    def match(self, words):
        # Check if argument sound matches this rhyme
        sound = self.sound_of_words(words)
        if self.SOUND is None:
            # Set sound
            self.SOUND = sound
        # Check sound
        return self.SOUND == sound

    def sound_of_words(self, words):
        last_word = [x for x in (normalize_word(w) for w in words.split(" ")) if x][-1]
        try:
            word_obj = Word.objects.get(name=last_word)
            sound = word_obj.phonic_set.related.first().name
        except:
            sound = last_word[-3:]
        return sound
        

class Refrain:
    WORDS = None
    RHYME = None

    def __init__(self, rhyme):
        self.RHYME = rhyme

    def match(self, words):
        # Check if that line matches this line. Set if not already.
        if self.WORDS is None:
            return self.set(words)
        else:
            # Need exact match
            return self.WORDS == words

    def set(self, words):
        if self.RHYME.match(words):
            self.WORDS = words
            return True
        else:
            return False

class PoemError(Exception):
    def __init__(self, msg):
        self.msg = msg

    def __str__(self):
        return repr(self.msg)

class Poem:
    NAME = "???"

    def __init__(self, content=""):        
        self.NUM_LINES          = None
        self.NUM_STANZAS        = None
        self.LINES_PER_STANZA   = []
        self.SYLLABLES_PER_LINE = []
        self.RHYME_SCHEME       = []
        self.content = content

    def as_text_area(self):
        # render the widget to html
        fmt = {
            'content'  : self.content,
            'num_rows' : (self.NUM_LINES or 35) + (self.NUM_STANZAS or 0),
            'num_cols' : 80,
        }
        return """<textarea id='poem_content' name='poem_content' rows='{num_rows}' cols='{num_cols}'>{content}</textarea>""".format(**fmt)

    def check_stanzas(self):
        if len(self.get_stanzas()) == self.NUM_STANZAS:
            return
        else:
            raise PoemError("A %s should have one stanza." % self.NAME)

    def check_lines(self):
        if len(self.get_lines()) == self.NUM_LINES:
            return
        else:
            raise PoemError("A %s should have %s lines." % (self.NAME, self.NUM_LINES))

    def check_syllables(self):
        all_lines = self.get_lines()
        errors    = []
        for i in xrange(0, self.NUM_LINES):
            expected_syllables = self.syllables_of_int(i)
            actual_syllables   = self.syllables_of_line(all_lines[i])
            if (expected_syllables is not None) and (actual_syllables != expected_syllables):
                errors.append("Line %s should have %s syllables, got %s syllables." % (i+1, expected_syllables, actual_syllables))
        if errors:
            raise PoemError("\n".join(errors))
        else:
            return

    def check_rhyme_scheme(self):
        all_lines = self.get_lines()
        errors = []
        for i in xrange(0, self.NUM_LINES):
            expected_rhyme = self.rhyme_of_int(i)
            if (expected_rhyme is not None) and (not expected_rhyme.match(all_lines[i])):
                errors.append("Line %s does not match rhyme scheme '%s'." % (i+1, expected_rhyme))
        if errors:
            raise PoemError("\n".join(errors))
        else:
            return

    def check_other(self):
        # Miscellaneous check. Nothing here by default.
        return

    def check_style(self):
        errors = []
        for check in [self.check_syllables, self.check_rhyme_scheme, self.check_other]:
            try:
                check()
            except PoemError as err:
                errors.append(err.msg)
        if errors:
            raise PoemError("\n".join(errors))

    def check_words(self):
        # filter puncutation
        # get all words from all lines
        # lookup each in db
        not_found = set([])
        for word in self.get_filtered_words(self.content):
            try:
                Word.objects.get(name=word)
            except Word.DoesNotExist:
                not_found.add(word)
        return list(not_found)

    def compile(self):
        # Compare 'self.content' to an expected poem of this format. Default is 'PASS!'
        try:
            self.check_stanzas()
            self.check_lines()
            self.check_style()
        except PoemError as err:
            return err.msg

    def get_description(self):
        return "There's no description."

    def get_filtered_words(self, text):
        # Return a generator of each word of the content, punctuation removed.
        for line in text.split("\n"):
            for word in line.split(" "):
                clean_word = normalize_word(word)
                if clean_word:
                    yield clean_word

    def get_lines(self):
        return [line for stanza in self.get_stanzas() for line in stanza]

    def get_name(self):
        return self.NAME

    def get_numlines(self):
        return self.NUM_LINES

    def get_numstanzas(self):
        # Return the expected number of stanzas
        return self.NUM_STANZAS

    def get_stanzas(self):
        content_by_stanza = []
        curr_stanza = []
        for line in (x.strip() for x in self.content.split("\n")):
            if line:
                curr_stanza.append(line)
            elif curr_stanza:
                content_by_stanza.append(curr_stanza)
                curr_stanza = []
        if curr_stanza:
            content_by_stanza.append(curr_stanza)
        return content_by_stanza

    def lines_of_stanza(self, stanza_index):
        if stanza_index < len(self.LINES_PER_STANZA):
            return self.LINES_PER_STANZA[stanza_index]
        else:
            return None

    def rhyme_of_int(self, line_index):
        if line_index < len(self.RHYME_SCHEME):
            return self.RHYME_SCHEME[line_index]
        else:
            return None

    def syllables_of_int(self, line_index):
        if line_index < len(self.SYLLABLES_PER_LINE):
            return self.SYLLABLES_PER_LINE[line_index]
        else:
            return None

    def syllables_of_line(self, line):
        num_syllables = 0
        for word in self.get_filtered_words(line):
            try:
                word_obj = Word.objects.get(name=word)
                num_syllables += word_obj.num_syllables
            except Word.DoesNotExist:
                num_syllables += sum((1 for c in word if c in ENGLISH_VOWELS))
        return num_syllables

class FreeVerse(Poem):
    NAME = "Free Verse"
    SLUG = "free"

    def get_description(self):
        return "Free verse has no rules. Go ham."

class Haiku(Poem):
    NAME = "Haiku"
    SLUG = "haiku"
    
    def __init__(self, *args, **kwargs):
        Poem.__init__(self, *args, **kwargs)
        self.NUM_LINES          = 3
        self.NUM_STANZAS        = 1
        self.LINES_PER_STANZA   = [3]
        self.SYLLABLES_PER_LINE = [5,7,5]

    def get_description(self):
        return " ".join(("Traditional haikus consist of 3 lines with 5 syllables on the first line, 7 syllables on the second, and 5 syllables on the third.",
            "Juxtaposition is nice for haikus.",
            "Combine two very different ideas or feelings with a key 'cutting' word."))

class Cinquian(Poem):
    NAME = "Cinquain"
    SLUG = "cinquain"

    def __init__(self, *args, **kwargs):
        Poem.__init__(self, *args, **kwargs)
        self.NUM_LINES          = 5
        self.NUM_STANZAS        = 1
        self.LINES_PER_STANZA   = [self.NUM_LINES]
        self.SYLLABLES_PER_LINE = [2, 4, 6, 8, 2]
        (a,b) = Rhyme(), Rhyme()
        self.RHYME_SCHEME       = [a, b, a, b, b]

    def get_description(self):
        return "The American cinquain is a 5-line poem with rhyme scheme 'ababb' and 2, 4, 6, 8, and 2 syllables on each line."
    
class Clerihew(Poem):
    NAME = "Clerihew"
    SLUG = "clerihew"

    def __init__(self, *args, **kwargs):
        Poem.__init__(self, *args, **kwargs)
        self.NUM_LINES          = 4
        self.NUM_STANZAS        = 1
        self.LINES_PER_STANZA   = [self.NUM_LINES]
        self.SYLLABLES_PER_LINE = []
        (a,b) = Rhyme(), Rhyme()
        self.RHYME_SCHEME       = [a, a, b, b]

    def get_description(self):
        return " ".join(("The first line of a clerihew should be a famous person's name and the rest of the poem should put that person in an amusing or uncommon situation.",
                         "Rhyme scheme is 'aabb', but length and meter are free and should be varied for effect."))
    
class Couplet(Poem):
    NAME = "Couplet"
    SLUG = "couplet"
    
    def __init__(self, *args, **kwargs):
        Poem.__init__(self, *args, **kwargs)
        self.NUM_LINES          = 2
        self.NUM_STANZAS        = 1
        self.LINES_PER_STANZA   = [self.NUM_LINES]
        self.SYLLABLES_PER_LINE = [10] * self.NUM_LINES
        a = Rhyme()
        self.RHYME_SCHEME       = [a, a]

    def get_description(self):
        return "Two rhyming lines of 10 syllables."

class Tercet(Poem):
    NAME = "Tercet"
    SLUG = "tercet"
    
    def __init__(self, *args, **kwargs):
        Poem.__init__(self, *args, **kwargs)
        self.NUM_LINES          = 3
        self.NUM_STANZAS        = 1
        self.LINES_PER_STANZA   = [self.NUM_LINES]
        self.SYLLABLES_PER_LINE = [10] * self.NUM_LINES
        a = Rhyme()
        self.RHYME_SCHEME       = [a, a, a]

    def get_description(self):
        return "Three rhyming lines of 10 syllables."

class Tanaga(Poem):
    NAME = "Tanaga"
    SLUG = "tanaga"

    def __init__(self, *args, **kwargs):
        Poem.__init__(self, *args, **kwargs)
        self.NUM_LINES          = 4
        self.NUM_STANZAS        = 4
        self.LINES_PER_STANZA   = self.NUM_LINES
        self.SYLLABLES_PER_LINE = [7] * self.NUM_LINES
        (a,b) = Rhyme(), Rhyme()
        self.RHYME_SCHEME       = [a, a, b, b]

    def get_description(self):
        return "The Tanaga is a Filipino style. It has 4 lines of 7 syllables and rhyme scheme 'aabb'."
    
class Limerick(Poem):
    NAME = "Limerick"
    SLUG = "limerick"
    
    def __init__(self, *args, **kwargs):
        Poem.__init__(self, *args, **kwargs)
        self.NUM_LINES          = 5
        self.NUM_STANZAS        = 1
        self.LINES_PER_STANZA   = [5]
        self.SYLLABLES_PER_LINE = [] # roughly 8, 8, 6, 6, 8, but too rough to call
        a  = Rhyme()
        b  = Rhyme()
        self.RHYME_SCHEME = [a, a, b, b, a]
        
    def check_other(self):
        # Coreectness = right rhyme. It should taste like a song, but not sure how to check that
        all_lines = self.get_lines()
        shortest_long_line = min([self.syllables_of_line(l) for l in [all_lines[0], all_lines[1], all_lines[4]]])
        longest_short_line = max([self.syllables_of_line(l) for l in [all_lines[2], all_lines[3]]])
        if shortest_long_line < longest_short_line:
            raise PoemError("Lines 3 and 4 should probably have fewer syllables than lines 1,2, and 5.")
        else:
            return
    
    def get_description(self):
        return "Limericks are fun. Write 5 musical lines with rhyme scheme 'aabba'."

class Villanelle(Poem):
    NAME = "Villanelle"
    SLUG = "villanelle"
    
    def __init__(self, *args, **kwargs):
        Poem.__init__(self, *args, **kwargs)
        self.NUM_LINES          = 19
        self.NUM_STANZAS        = 6
        self.LINES_PER_STANZA   = [3, 3, 3, 3, 3, 4]
        self.SYLLABLES_PER_LINE = [10] * self.NUM_LINES
        (a,b)  = Rhyme(), Rhyme()
        r1 = Refrain(a)
        r2 = Refrain(a)
        self.RHYME_SCHEME = [r1, b, r2,
                             a,  b, r1,
                             a,  b, r2,
                             a,  b, r1,
                             a,  b, r2,
                             a,  b, r1, r2]
            
    def get_description(self):
        return " ".join(("A villanelle is a 19-line, iambic pentameter poem broken into 6 stanzas.",
            "The first stanza sets the refrain.",
            "Its first line must be the last line of stanzas 2 and 4, and the second-to-last line of the final stanza.",
            "Its last line must be the last line of stanzas 3, 5, and 6.",
            "These first and last lines must rhyme both with each other and with the first line of every proceeding stanza.",
            "Additionally, the second line of every stanza must rhyme."))
            # "There seems to be a connection between villanelles and obsession."
            # "See Dylan Thomas or Sylvia Plath."

class Rondelet(Poem):
    NAME = "Rondelet"
    SLUG = "rondelet"
    
    def __init__(self, *args, **kwargs):
        Poem.__init__(self, *args, **kwargs)
        self.NUM_LINES          = 7
        self.NUM_STANZAS        = 1
        self.LINES_PER_STANZA   = self.NUM_LINES
        self.SYLLABLES_PER_LINE = [4, 8, 4, 8, 8, 8, 4]
        (a,b)  = Rhyme(), Rhyme()
        r1 = Refrain(a)
        self.RHYME_SCHEME = [r1, b, r1, a, b, b, r1]
            
    def get_description(self):
        return " ".join(("A rondelet is a 7-line French form with rhyme scheme 'AbAabbA'.",
                         "The refrain 'A' has 4 syllables and the rest have 8 syllables."))

class ItalianSonnet(Poem):
    NAME = "Italian Sonnet"
    SLUG = "italian-sonnet"
    
    def __init__(self, *args, **kwargs):
        Poem.__init__(self, *args, **kwargs)
        self.NUM_LINES          = 14
        self.NUM_STANZAS        = 1
        self.LINES_PER_STANZA   = [self.NUM_LINES]
        self.SYLLABLES_PER_LINE = [10] * self.NUM_LINES

        (a,b,c,d,e)  = (Rhyme(), Rhyme(), Rhyme(), Rhyme(), Rhyme())
        self.RHYME_SCHEME = [a, b, b, a,
                             a, b, b, a,
                             c, d, e, c, d, e]

    def get_description(self):
        return " ".join(("The Italian sonnet, invented by Giacomo de Lentini in the 1200s, consists of 14-lines of iambic pentameter.",
            "The first eight lines are usually the introduction and the last six are usually the resolution.",
            "Rhyme scheme is 'abba abba cde cde'."))

class EnglishSonnet(Poem):
    NAME = "English Sonnet"
    SLUG = "english-sonnet"
    
    def __init__(self, *args, **kwargs):
        Poem.__init__(self, *args, **kwargs)
        self.NUM_LINES          = 14
        self.NUM_STANZAS        = 1
        self.LINES_PER_STANZA   = [self.NUM_LINES]
        self.SYLLABLES_PER_LINE = [10] * self.NUM_LINES

        (a,b,c,d,e,f,g)  = (Rhyme(), Rhyme(), Rhyme(), Rhyme(), Rhyme(), Rhyme(), Rhyme())
        self.RHYME_SCHEME = [a, b, a, b,
                             c, d, c, d,
                             e, f, e, f,
                             g, g]

    def get_description(self):
        return " ".join(("English sonnets are 14 lines in iambic pentameter.",
            "The rhyme scheme is 'abab cdcd efef gg'.",
            "Notice that couplet at the end."))

# See 'http://en.wikipedia.org/wiki/Rhyme_scheme' for more
