from django.db import models

class Word(models.Model):
    name               = models.CharField(max_length=50)
    num_syllables      = models.IntegerField()
    rhymes_with        = models.ManyToManyField(Word)
    almost_rhymes_with = models.ManyToManyField(Word)

    def __unicode__(self):
        return self.name
