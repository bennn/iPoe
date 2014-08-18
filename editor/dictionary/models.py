from django.db import models

class Word(models.Model):
    name               = models.CharField(max_length=50)
    num_syllables      = models.IntegerField()
    rhymes_with        = models.CommaSeparatedIntegerField(max_length=999)
    almost_rhymes_with = models.CommaSeparatedIntegerField(max_length=999)

    def __unicode__(self):
        return self.name
