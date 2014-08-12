class Poem:

    NAME               = "???"
    NUM_LINES          = None
    NUM_STANZAS        = None
    LINES_PER_STANZA   = []
    SYLLABLES_PER_LINE = []

    def as_text_area(self):
        # render the widget to html
        fmt = {
            'num_rows' : (self.NUM_LINES or 35) + (self.NUM_STANZAS or 0),
            'num_cols' : 80,
        }
        return """<textarea id='poem_content' rows='{num_rows}' cols='{num_cols}'></textarea>""".format(**fmt)

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
    NUM_STANZA         = 1
    LINES_PER_STANZA   = [3]
    SYLLABLES_PER_LINE = [5,7,5]

def of_string(poem_type):
    if poem_type == "free":
        return FreeVerse
    elif poem_type == "haiku":
        return Haiku
    else: # default / not-implemented
        return Poem
