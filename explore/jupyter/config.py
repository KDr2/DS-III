import os
import sys

def path(sub=None):
    here = os.path.dirname(__file__)
    if sub:
        return os.path.join(here, sub)
    return here

#c.NotebookApp.extra_template_paths = ""
c.NotebookApp.extra_static_paths = [path("static")]

c.NotebookApp.notebook_dir = path("notebooks")
