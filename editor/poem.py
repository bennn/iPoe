class Poem:

    NUM_LINES          = None
    NUM_STANZAS        = None
    LINES_PER_STANZA   = []
    SYLLABLES_PER_LINE = []

    def get_numlines():
        return self.NUM_LINES

    def get_numstanzas():
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

class Free(Poem):
    pass

class Haiku(Poem):
    NUM_LINES          = 3
    NUM_STANZA         = 1
    LINES_PER_STANZA   = [3]
    SYLLABLES_PER_LINE = [5,7,5]
