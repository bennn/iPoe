CREATE TABLE word_rhymes (
  word integer REFERENCES word(id),
  rhyme integer REFERENCES word(id),
  PRIMARY KEY (word, rhyme)
);
