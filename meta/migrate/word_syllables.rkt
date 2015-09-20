CREATE TABLE word_syllables (
  word integer REFERENCES word(id),
  syllables integer NOT NULL,
  PRIMARY KEY (word, syllables)
);
