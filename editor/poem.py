class Poem:

    NAME               = "???"
    NUM_LINES          = None
    NUM_STANZAS        = None
    LINES_PER_STANZA   = []
    SYLLABLES_PER_LINE = []

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

    def compile(self):
        # Compare 'self.content' to an expected poem of this format. Default is 'PASS!'
        # TODO break into methods. What does it mean for a poem to be correct?
        # right number of lines
        # right number of stanzas
        # right number of lines per stanza
        # right number of syllables per line
        # right rhyme scheme (can I do schemes as lists? [a, b, a, b, c, c])
        return None

    def get_name(self):
        return self.NAME

    def get_numlines(self):
        return self.NUM_LINES

    def get_numstanzas(self):
        return self.NUM_STANZAS

    def lines_of_stanza(self, stanza_index):
        if stanza_index < len(self.LINES_PER_STANZA):
            return self.LINES_PER_STANZA[stanza_index]
        else:
            return None

    def syllables_of_line(self, line_index):
        if line_index < len(self.SYLLABLES_PER_LINE):
            return self.SYLLABLES_PER_LINE[line_index]
        else:
            return None

class FreeVerse(Poem):
    NAME = "Free Verse"

class Haiku(Poem):
    NAME               = "Haiku"
    NUM_LINES          = 3
    NUM_STANZAS        = 1
    LINES_PER_STANZA   = [3]
    SYLLABLES_PER_LINE = [5,7,5]

    def compile(self):
        return "Errors everywhere"

class Rhyme:
    SOUND = None
    def __init__(self, word):
        # TODO get the ending sound from the word
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
        last_word = Word(words.rsplit(" ", 1)[-1])
        if self.RHYME == Rhyme(last_word):
            self.WORDS = words
            return True
        else:
            return False

class Villanelle(Poem):
    NAME               = "Villanelle"
    NUM_LINES          = 19
    NUM_STANZAS        = 6
    LINES_PER_STANZA   = [3, 3, 3, 3, 3, 4]
    SYLLABLES_PER_LINE = [10] * 19

    a  = Rhyme()
    b  = Rhyme()
    r1 = Refrain(a)
    r2 = Refrain(a)
    RHYME_SCHEME = [r1, b, r2,
                    a,  b, r1,
                    a,  b, r2,
                    a,  b, r1,
                    a,  b, r2,
                    a,  b, r1, r2]
                        


def of_string(poem_type):
    if poem_type == "free":
        return FreeVerse
    elif poem_type == "haiku":
        return Haiku
    else: # default / not-implemented
        return Poem
