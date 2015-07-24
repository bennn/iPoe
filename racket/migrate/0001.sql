CREATE TABLE word (
 id serial PRIMARY KEY,
 word text UNIQUE NOT NULL,
 num_syllables integer NOT NULL
);
