---
layout: post
title:  "Generate Daily Messages With Org Journal"
date:   2013-11-17 15:38
categories: emacs
---

One of the requirements of my job is to send out a daily message. This message
should summarize what I did the day before, and what I hope to accomplish for
the current day. At first, I would spend my day jumping from task to task, and
then the next morning, I would rack my brain to make sure I included all the
things I worked on.

It didn't take long to realize that this was no way to live. I definitely
needed a better solution. So I started looking around.

## Org Journal

After hunting around for a bit, I came across Org Journal. It is a fantastic
package for, as the name implies, writing journal entries. And since it's built
on top of Org Mode, I knew I had to give it a try.

Once installed and configured, I can press, in my case, C-x j, and this will
either start a new journal entry if one doesn't already exist, or open an
existing one. Each time a new entry is added, org-journal inserts a second
level heading in the document, and stamps it with the time the entry was added.

This was definitely a productivity win for me, and so I settled on it as my
tool of choice.

## Scratching An Itch

Even though org-journal was assisting me in remembering everything I did on the
previous day, I still had to format the entries into a suitable format and send
them off to my boss. Accomplishing this was a multi-step process which I
quickly grew tired of. I had to:

1. Call M-x calendar and navigate to the previous day
2. Press j to open that day's journal entry
3. C-x h to copy the entire entry
4. C-x b *scratch* to switch over to my scratch buffer
5. C-y to paste the journal contents in
6. Remove any additional notes I had, strip out the top level headers and time stamps, and then add my plan for the current day.

What a bunch of nonsense! I certainly wasn't going to do that 5 days a week for the rest of my career.

## Elisp to the Rescue

So naturally, I did what any Emacs user would do: I wrote some lisp. Before we
get into the code, I should warn you about something. I'm still pretty new to
lisp. If you're an old hat at this stuff, I'm sure there are better ways to
accomplish what I'm doing. And I would LOVE to hear about them. Please feel
free to comment!

Disclaimer aside, let's get into the code.

### What Was Yesterday

The first thing I needed to accomplish was determining the date of the prior
day. Like most people (I hope), I only work 5 days a week. So simply grabbing
yesterday's date wasn't going to work. Instead, I need to keep subtracting a
day's worth of time from the current day, until I reached some time period that
fell between Monday and Friday.

That lead me to:

{% highlight lisp %}
(defun my/org-find-previous-workday ()
  (let* ((time (time-subtract (current-time) (seconds-to-time 86400)))
         (workdays '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday")))
    (while (not (member (format-time-string "%A" time) workdays))
      (setq time (time-subtract time (seconds-to-time 86400))))
    time))
{% endhighlight %}

### Grab The Tasks

Once I knew the previous working day's date, I needed to grab the journal
entry's content. Since I include notes and other references for each entry, and
this email is supposed to be succinct, using the entire journal entry wasn't
sufficient. Instead, I needed to strip out just the second level headings,
which include short summaries of what I'm working on.

I wrote three functions to handle this, which are included below. The results
of these three functions is to return a list of all the headers in the org file
with a level I specified. If I don't specify a level, it will return all header
levels.

{% highlight lisp %}
(defun my/org-should-include-current (level)
  (progn
    (if (and
         (outline-on-heading-p)
         (or
          (eq nil level) (eq level (org-outline-level))))
        t
      nil)))

(defun my/org-get-current-header ()
  (progn
    (setq beg (point))
    (outline-end-of-heading)
    (buffer-substring-no-properties beg (point))))

(defun my/org-outline-headings-to-list (level)
  (setq headings '())
  (show-all)
  (goto-char (point-min))

  (if (my/org-should-include-current level)
      (add-to-list 'headings (my/org-get-current-header) t))

  (while (outline-next-heading)
    (if (my/org-should-include-current level)
        (add-to-list 'headings (my/org-get-current-header) t)))
  headings)
{% endhighlight %}

### Generating The Message

At this point, I had all the data I needed to quickly generate my scrum
message. All that was left was to hook it together, open a temp buffer with the
formatted results, and then start filling in the current day's goals.

{% highlight lisp %}
(defun my/org-outline-create-scrum-message ()
  (interactive)
  (let* ((previous (my/org-find-previous-workday)))
    (setq journal-file (concat org-journal-dir (format-time-string "%Y%m%d" previous)))
    (if (file-exists-p journal-file)
        (progn
          (switch-to-buffer (find-file-noselect journal-file))
          (setq headings (my/org-outline-headings-to-list 2))
          (switch-to-buffer "*daily scrum*")
          (erase-buffer)
          (insert "* Yesterday\n")
          (mapcar (lambda(element)
                    (insert (replace-regexp-in-string "^** ..... " "  - " element) "\n")) headings)
          (insert "\n* Today\n  - ")
          (org-mode)
          (show-all)))))
{% endhighlight %}

## Future Expansion

I have been using this method for the last couple of weeks to much success. It
has saved me a ton of time, and has helped ensure that I account for every task
I worked on during the previous day.

There are a few things I would like to improve. The most obvious next step is
to hook up gnus to my work email and generate this message in an actual email
buffer. Then, once I'm finished typing, I can just send it off directly from
Emacs instead of copy and pasting it into my mail client.

The other thing I need to handle is skipped dates. If I take a vacation day, or
there is a holiday, this code will just fail to generate a message template for
me. It only goes back one day and then calls it quits after that. It wouldn't
take much to modify it to keep searching until it finds a day with an entry. I
just haven't gotten around to it yet.

As I mentioned before, I'm sure the code could be improved. I would love to
hear feedback from the community on what I could do better. Lisp is still
pretty new to me, but I've been having a blast learning it.

For those interested in seeing the code all in one place, or checking out the
rest of my configuration, you can see it on GitHub.

Thanks, and happy coding!
