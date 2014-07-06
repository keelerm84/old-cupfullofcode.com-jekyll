---
layout: post
title:  "Dancing Code Monkey"
date:   2013-01-05 23:09
categories: emacs
---

A few months ago, I started playing around with Emacs.  For roughly six years
prior to that, I was a die-hard Vim fan.  Vim will always hold a special place
in my heart, but I have grown to love Emacs. I'll undoubtedly cover Emacs in
more detail, but I'll leave that for other posts.

One cannot use Emacs for long without dabbling in Elisp, the language upon
which Emacs is built.  Having only briefly touched on Lisp languages in
college, I figured it was high time to start the learning process.

Recently, a friend of mine stumbled upon this cute little code monkey on
someone's Stackoverflow profile.

          __
     w  c(..)o  (
      \__(-)   __)
          /\  (
         /( )__)
        m /|
         | \
         m  m

Quite naturally I thought, "I should make him dance."  As this seemed
relatively simple, I decided to tackle this with Elisp.

I first created four slightly altered copies of the little guy, and with the
code shown below, had him dancing in no time.  You can make him dance
interactively a set number of times by issuing the standard prefix arguments
prior to invoking the function ( e.g. C-u 100 M-x dance-monkey ) or watch him
shake his groove thing on YouTube.

{% highlight lisp %}
(defun insert-monkey (num)
  (interactive "P")
  (erase-buffer)
  (insert-file-contents 
    (concat "/home/keelerm/Projects/emacs/code-monkey/" (int-to-string num))))

(defun dance-monkey (repeat)
  (interactive "P")
  (let ((times 0))
    (while (< times repeat)
      (insert-monkey (mod times 4))
      (setq times (1+ times))
      (sit-for 0.2)
      )))
{% endhighlight %}

As stated above, I know this is extremely simple, but I suppose everyone has to
start somewhere.  Besides, who doesn't love a dancing monkey?
