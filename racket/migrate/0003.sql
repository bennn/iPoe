CREATE TABLE word_almost_rhymes (
  word integer REFERENCES word(id),
  almost_rhyme integer REFERENCES word(id),
  PRIMARY KEY (word, almost_rhyme)
);
