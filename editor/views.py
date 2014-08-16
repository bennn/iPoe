import inspect
import sys
import poem

from django.core.context_processors import csrf
from django.shortcuts import render_to_response

POEM_STYLES= dict(((c.SLUG, c)
                   for (_,c) in inspect.getmembers(sys.modules[poem.__name__], (lambda c: inspect.isclass(c) and hasattr(c, 'SLUG')))))

def create(request):
    cookie = {}
    cookie.update(csrf(request))
    return render_to_response('create.html', cookie)

def home(request):
    cookie = {'poem_names_and_slugs': sorted(((c.NAME,slug) for (slug, c) in POEM_STYLES.iteritems()))}
    return render_to_response('index.html', cookie)

def editor(request, poem_slug=None):
    cookie = {}
    cookie.update(csrf(request)) # Required for csrf form protection
    cookie['slug'] = poem_slug
    poem_class = POEM_STYLES.get(poem_slug, poem.Poem) # poem.of_string(poem_slug)
    if request.method == "POST":
        # Compile poem
        cookie['poem'] = poem_class(request.POST.get('poem_content', ''))
        if 'compile' in request.POST:
            cookie['compiled'] = True
            cookie['unknown_words'] = cookie['poem'].check_words()
            cookie['error_message'] = cookie['poem'].compile()
        return render_to_response('editor.html', cookie)
    else:
        # Prepare to create poem
        cookie['poem'] = poem_class()
        return render_to_response('editor.html', cookie)
