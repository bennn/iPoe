from django.conf.urls import patterns, include, url

urlpatterns = patterns('',
    # Examples:
    url(r'^$', 'views.home'),
    url(r'^editor$', 'views.editor'),
    url(r'^editor/(?P<poemtype>[a-z\-]+)/$', 'views.editor'),
)
