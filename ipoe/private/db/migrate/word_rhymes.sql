CREATE TABLE word_rhymes (
  word integer REFERENCES word(id) ON DELETE CASCADE,
  rhyme integer REFERENCES word(id) ON DELETE CASCADE,
  PRIMARY KEY (word, rhyme)
);
