import poem

from django.core.context_processors import csrf
from django.shortcuts import render_to_response


def create(request):
    cookie = {}
    cookie.update(csrf(request))
    return render_to_response('create.html', cookie)

def home(request):
    return render_to_response('index.html', {})

def editor(request, poem_slug=None):
    cookie = {}
    cookie.update(csrf(request)) # Required for csrf form protection
    cookie['slug'] = poem_slug
    poem_class = poem.of_string(poem_slug)
    if request.method == "POST":
        print("POST is %s" % request.POST)
        # Compile poem
        cookie['poem'] = poem_class(request.POST.get('poem_content', ''))
        if 'compile' in request.POST:
            cookie['compiled'] = True
            cookie['unknown_words'] = cookie['poem'].check_words()
            cookie['error_message'] = None # cookie['poem'].compile()
        return render_to_response('editor.html', cookie)
    else:
        # Prepare to create poem
        cookie['poem'] = poem_class()
        return render_to_response('editor.html', cookie)
