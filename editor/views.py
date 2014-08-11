from django.shortcuts import render_to_response

def home(request):
    return render_to_response('index.html', {})

def editor(request, poemtype=""):
    return render_to_response('index.html', {})
