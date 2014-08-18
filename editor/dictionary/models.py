from django.db import models

class Word(models.Model):
    name = models.CharField(max_length=50)
    num_syllables = models.IntegerField()

    def __unicode__(self):
        return self.name
