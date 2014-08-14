from dictionary.models import Word

ENGLISH_VOWELS = ['a', 'e', 'i', 'o', 'u', 'y']

class Rhyme:
    SOUND = None
    def __init__(self, word=""):
        try:
            w = Word.objects.get(name=word)
            self.SOUND = None # w.phonic_set.related.
        except Word.DoesNotExist:
            self.SOUND = None
        
    def match(self, sound):
        # Check if argument sound matches this rhyme
        if self.SOUND is None:
            # Set sound
            self.SOUND = sound
        # Check sound
        return self.SOUND == sound

class Refrain:
    WORDS = None
    RHYME = None

    def __init__(self, rhyme):
        self.RHYME = rhyme

    def match(self, words):
        # Check if that line matches this line. Same words.
        return self.WORDS == words

    def set(self, words):
        last_word = words.rsplit(" ", 1)[-1]
        if self.RHYME == Rhyme(last_word):
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

    NAME               = "???"
    NUM_LINES          = None
    NUM_STANZAS        = None
    LINES_PER_STANZA   = []
    SYLLABLES_PER_LINE = []
    RHYME_SCHEME       = []

    def __init__(self, content=""):
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
        return

    def check_other(self):
        return

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
            self.check_syllables()
            self.check_rhyme_scheme()
            self.check_other()
            return "" # We are OK
        except PoemError as err:
            return err.msg

    def normalize_word(self, word):
        # Remove punctuation from the string [word].
        return "".join((c for c in word.lower() if 'a' <= c <= 'z'))

    def get_description(self):
        return "There's no description."

    def get_filtered_words(self, text):
        # Return a generator of each word of the content, punctuation removed.
        for line in text.split("\n"):
            for word in line.split(" "):
                clean_word = self.normalize_word(word)
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

    def get_description(self):
        return "Free verse has no rules. Go ham."

class Haiku(Poem):
    NAME               = "Haiku"
    NUM_LINES          = 3
    NUM_STANZAS        = 1
    LINES_PER_STANZA   = [3]
    SYLLABLES_PER_LINE = [5,7,5]

    def get_description(self):
        return " ".join(("Traditional haikus consist of 3 lines with 5 syllables on the first line, 7 syllables on the second, and 5 syllables on the third.",
            "Juxtaposition is nice for haikus.",
            "Combine two very different ideas or feelings with a key 'cutting' word."))
    
class Limerick(Poem):
    NAME               = "Limerick"
    NUM_LINES          = 5
    NUM_STANZAS        = 1
    LINES_PER_STANZA   = [5]
    SYLLABLES_PER_LINE = [] # roughly 8, 8, 6, 6, 8, but too rough to call

    a  = Rhyme()
    b  = Rhyme()
    RHYME_SCHEME = [a, a, b, b, a]

    def check_other(self):
        # Coreectness = right rhyme. It should taste like a song, but not sure how to check that
        return True
    
    def get_description(self):
        return "Limericks are fun."

class Villanelle(Poem):
    NAME               = "Villanelle"
    NUM_LINES          = 19
    NUM_STANZAS        = 6
    LINES_PER_STANZA   = [3, 3, 3, 3, 3, 4]
    SYLLABLES_PER_LINE = [10] * NUM_LINES

    (a,b)  = Rhyme(), Rhyme()
    r1 = Refrain(a)
    r2 = Refrain(a)
    RHYME_SCHEME = [r1, b, r2,
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

class ItalianSonnet(Poem):
    NAME               = "Italian (Petrarchan) Sonnet"
    NUM_LINES          = 14
    NUM_STANZAS        = 1
    LINES_PER_STANZA   = [NUM_LINES]
    SYLLABLES_PER_LINE = [10] * NUM_LINES

    (a,b,c,d, e)  = (Rhyme(), Rhyme(), Rhyme(), Rhyme(), Rhyme())
    RHYME_SCHEME = [a, b, b, a,
                    a, b, b, a,
                    c, d, e, c, d, e]

    def get_description(self):
        return " ".join(("The Italian sonnet, invented by Giacomo de Lentini in the 1200s, consists of 14-lines of iambic pentameter.",
            "The first eight lines are usually the introduction and the last six are usually the resolution.",
            "Rhyme scheme is 'abba abba cde cde'."))

class EnglishSonnet(Poem):
    NAME               = "English (Shakespearian) Sonnet"
    NUM_LINES          = 14
    NUM_STANZAS        = 1
    LINES_PER_STANZA   = [14]
    SYLLABLES_PER_LINE = [10] * NUM_LINES

    (a,b,c,d,e,f,g)  = (Rhyme(), Rhyme(), Rhyme(), Rhyme(), Rhyme(), Rhyme(), Rhyme())
    RHYME_SCHEME = [a, b, a, b,
                    c, d, c, d,
                    e, f, e, f,
                    g, g]

    def get_description(self):
        return " ".join(("English sonnets are 14 lines in iambic pentameter.",
            "The rhyme scheme is 'abab cdcd efef gg'.",
            "Notice that couplet at the end."))


def of_string(poem_type):
    if poem_type == "free":
        return FreeVerse
    elif poem_type == "haiku":
        return Haiku
    elif poem_type == "villanelle":
        return Villanelle
    elif poem_type == "limerick":
        return Limerick
    elif poem_type == "italian-sonnet":
        return ItalianSonnet
    elif poem_type == "english-sonnet":
        return EnglishSonnet
    else: # default / not-implemented
        return Poem
