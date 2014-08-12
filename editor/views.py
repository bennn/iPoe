import poem

from django.shortcuts import render_to_response


def home(request):
    return render_to_response('index.html', {})

def editor(request, poem_type=None):
    poem_class = poem.of_string(poem_type)
    cookie = {
        'poem': poem_class()
    }
    return render_to_response('editor.html', cookie)
