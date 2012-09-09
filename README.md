`tumblesocks-mode` - Tumblr Support for Emacs
=============================================
<!-- ![http://i.imgur.com/WW6Qo.png](http://i.imgur.com/WW6Qo.png) -->
![http://i.imgur.com/9wroS.png](http://i.imgur.com/9wroS.png)

Tumblesocks is an Emacs tumblr client. With it, you can write posts,
check your dashboard, and view blogs and notes.

Viewing blogs
-------------
The main entry point is `tumblesocks-view-dashboard`, which will
open up a list of posts from your dashboard.

Keys:

* Move between next and previous posts quickly with **n** and **p**.
* **q: Quit**; go back to what you were doing before.

Visiting each post:
* **l: Like** the post underneath the cursor. Use `C-u l` to **unlike** it.
* **f: Follow** whichever blog wrote the post underneath the cursor. Use `C-u f` to **unfollow** the blog.
* **o: Open** the post underneath the cursor in your web browser.
* **y: Yank** the URL of the post underneath the cursor to the kill ring/clipboard.

Switching views:
* **s: Search** for posts with a certain tag.
* **b: Visit Blog**: Show a list of posts from a certain blog.
* **Enter: Open** the post underneath the cursor in a new page, showing its notes.
* **g: Refresh** the current view (download new posts)

Managing your posts:
* **r: Reblog** the post under the cursor. This function will prompt you for extra comments to add.
* **c: Compose new post**. Once you are finished writing your post, use `C-c C-c` to **submit** it. Markdown syntax is supported.
* **d: Delete** the post under the cursor. (This only works if you made that post.)
* **e: Edit** the post under the cursor. (This only works if you made that post.)

How to connect Tumblesocks to your tumblr account
=================================================

The first time you use Tumblesocks, you mist connect Emacs to your
tumblr account. This process is a bit convoluted, but you only have to
do it once.

First, add the following to your .emacs:

    (setq tumblesocks-blog "YourBlogName.tumblr.com")

Apply the changes, either by running `M-x eval-buffer` or by
restarting Emacs.

Then, run `M-x tumblesocks-api-test-auth` to connect Emacs to your
Tumblr account.

Tumblr will open a webpage asking for access. Once you click
**Accept**, you will receive a very long code. Copy+paste that back
into your Emacs prompt (you can also use middle-click to do this).

If everything goes right, you will see a message like this in your
minibuffer:

    Hello, sneakygcr! Tumblesocks is working properly.

Troubleshooting
---------------
If anything goes wrong (typically "401 Not Authorized" errors), you
can ask Tumblesocks to forget your key by issuing
`M-x tumblesocks-api-forget-authentication`. Then use
`M-x tumblesocks-api-test-auth` again to reconnect your Emacs to your
Tumblr account.

Other "advanced" commands
-------------------------

If you have gumption, try these out:

* `tumblesocks-text-post-from-region`: Instantly create a post with
  the contents of region.
* `tumblesocks-text-post-from-buffer`: Instantly create a post from
  the entire buffer
* `tumblesocks-compose-new-from-region`: Open a buffer and start
  writing a new post. The contents of region will be copied over.
* `tumblesocks-compose-new-from-highlighted-region`: Open a buffer and
  start writing a new post. The contents of region will be
  syntax-highlighted and copied into the post as formatted HTML. This
  is super-useful for including source code into your tumbles.
* `tumblesocks-compose-insert-highlighted-region`: Insert the
  syntax-highlighted region at the end of post you're currently writing.
