% title: hello kfold
% date: 2021-12-11
% desc: some potentially interesting information.

static site generators are fairly popular for blogging these days, which is generally a good thing
considering that most small blogs don't need a large backend to function. however, most of the tools
available for generating static sites are too bloated for my liking. to that end, i wrote a simple
site generator in python that converts a directory structure of markdown files into a proper blog.
this page and others on this site were produced by my generator!

# how it works
the tool scans a content directory containing a set of subdirectories, each containing an `index.md`
file alongside any other files to be pushed to the public directory. using the name of each subdirectory as a (kinda lousy) primary key to refer to posts, the tool then looks at a manifest table to
determine which posts are new, have been deleted, and have been updated. updates are further
determined by hashing markdown files and comparing them to hashes also stored in the manifest.

once the new and updated posts have been found, each markdown file is lightly parsed for metadata
indicated by lines beginning with `%` starting at the top of the file. then, the rest of the
Markdown is converted to HTML using [Python-Markdown](https://github.com/python-markdown/markdown).
this HTML and the metadata previously extracted are then injected into template HTML using Jinja2
in order to generate proper pages for each post. each post's final HTML page is saved into its own
subdirectory under the site's public directory. deleted posts are then removed from the public
directory.

finally, a table of contents is generated (again using Jinja2) to tie everything together, then placed into the site's public directory as the index.
