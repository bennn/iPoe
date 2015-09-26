CREATE TABLE word_almost_rhymes (
  word integer REFERENCES word(id) ON DELETE CASCADE,
  almost_rhyme integer REFERENCES word(id) ON DELETE CASCADE,
  PRIMARY KEY (word, almost_rhyme)
);
