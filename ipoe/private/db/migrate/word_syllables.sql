CREATE TABLE word_syllables (
  word integer REFERENCES word(id) ON DELETE CASCADE,
  syllables integer NOT NULL,
  PRIMARY KEY (word, syllables)
);
