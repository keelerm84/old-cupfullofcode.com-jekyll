---
layout: post
title:  "Snippet Expansion with YASnippet"
date:   2013-02-26 11:35
categories: emacs
---

Any editor worth its salt has some provision for text snippet
expansion. TextMate, Vim, and Sublime Text 2 all have this capability and Emacs
is certainly no exception. If you're not familiar with the concept of snippets,
the basic idea involves defining a keyword, which when followed with some
trigger (keyboard shortcut or menu option), replaces that keyword with some
predefined text. This functionality is a great boost to productivity as it
prevents the developer from having to manually type potentially hundreds or
thousands of lines of relatively boilerplate code.

## YASnippet

Like nearly all things in Emacs, there are a number of available packages that
provide this feature. The one I'll be discussing today is
[YASnippet (Yet Another Snippet)](https://github.com/capitaomorte/yasnippet). The
installation instructions are straightforward and minimal, so I won't bother
covering them here. Instead, I'll get right into the meat of the extension.

## Plain Text Expansion

The most basic functionality any snippet expansion package can offer is
straight text replacement. At first blush, this doesn't seem terribly helpful,
but let's consider an example. Suppose I want to include a copy of the GNU
Public License in my project. Copying and pasting such a lengthy license would
quickly grow tiresome for each project you work on. But with the help of
YASnippet, it's as painless as defining the below snippet and then typing
`gpl3` and pressing `TAB` to expand.

{% highlight text %}
# name : GPLv3
# key : gpl3
# --
                    GNU GENERAL PUBLIC LICENSE
                       Version 3, 29 June 2007

 Copyright (C) 2007 Free Software Foundation, Inc. http://fsf.org/
 Everyone is permitted to copy and distribute verbatim copies
 of this license document, but changing it is not allowed.

                            Preamble

  The GNU General Public License is a free, copyleft license for
software and other kinds of works.
[...]
{% endhighlight %}

You will notice defining this snippet was incredibly easy. The `#name : GPLv3`
line defines a common name for the snippet. The keyword we will use to trigger
the expansion is defined by `# key: gpl3`. Anything that follows the line `#
--` will be the replacement text that is inserted when our snippet is
expanded. Phew! That sure saved us a ton of typing. But that's barely
scratching the surface.

## Tab Stops

While simple text replacement does have its place, it would be more beneficial
if we could add a level of interactivity to the expansion process. This is
where tab stop fields enter the scene. A lot of the boilerplate code a
developer writes is similar, but not quite identical. For example, for loops
follow the same structure, but the initial and terminating conditions,
increment values, and variable names are likely to differ in some regard, so
simple text expansion isn't quite good enough. So let's define the following
snippet.

{% highlight text %}
# name : for
# key : for
# --
for($1; $2; $3) {
  $0
}
{% endhighlight %}

The `$N` values will act as tab stops for your cursor when the snippet is
expanded. This means, after expansion, my cursor will stop at `$1`, allowing me
to specify a value of my choosing. As I successively hit `TAB`, I will move
through the other tab stops, in numerical order. Upon exiting the expansion, my
cursor will end at `$0`, which is a special `$N` type marker. It should be
noted that each of the tab stops can also be defined with default values, using
the syntax `${N:default value}`. If the defined default value is sufficient,
you can simply tab past it and continue on your way.

## Mirrored Fields

While that is pretty awesome, there is a bit of a downside there. In each of
the three stops, if I was using the variable `i`, I would have to type that in
3 times. `i` isn't so bad, but if my variable is
`anExtremelyLongAndOverlyVerboseVariable`, that is tedious and error
prone. Luckily for us, YASnippet has the answer, and it is mirrored
fields. Mirrored fields allow you to type something in once, and have it
repeated throughout the snippet at other marked placeholders. The initial tab
stop should be defined as `${N:enumerate}` and each place you want that
information mirrored should use the standard tab notation. Let's see that in
action!

A common style you'll see in code is to define an if / while / for block, and
then include a comment at the bottom that reminds the reader of the condition
we're closing. YASnippet can certainly help us with this. Check out the snippet
below. Type in the condition once, have it included twice. Sweet. Converting
the `for` snippet is left as an exercise to the reader.

{% highlight text %}
if (${1:enumerate})
{
$0
} // $1
{% endhighlight %}

## Embedded Lisp

While the above examples have been awesome, the real power of YASnippet has yet
to be revealed. Not only can you include straight text, simple tab stops and
mirrored fields, but you can even include elisp code! The realm of possibility
just got ridiculous.

Let's look at an example from the C++ [QT](http://qt-project.org/) world. When
defining QT UI classes, there is some standard code that must be included each
time. Combining the power of mirrored fields with elisp, we can define the
following snippets. You can see them in action on
[YouTube](http://youtu.be/dlDvDNnsYr4).

{% highlight text %}
# name: QT UI class ... { ... }
# key: uiclass
# --

#include <$3>

namespace Ui {
    class $1;
}

class ${1:Name} : public ${3:QWidget}
{
    Q_OBJECT

public:
    $1(${2:QWidget * parent = 0});
    ~$1();

private:
    Ui::$1 * ui;
};
{% endhighlight %}

{% highlight text %}
# name: QT UI source
# key: outsource
# --
#include "ui_${1:$(downcase yas/text)}.h"
#include "${1:$(downcase yas/text)}.h"

${1:Name}::$1(QWidget * parent) : ${2:QWidget}(parent), ui(new Ui::$1) {
  ui->setupUi(this);
  $0
}

$1::~$1() {
}
{% endhighlight %}

As you can see, the syntax for including elisp is similar to providing default
values for tab stop fields, except all the power of elisp is unleashed!
YASnippet provides a handy placeholder, `yas/text` which represents the text
that is being mirrored in the field. Using this, you can easily camel case
certain words, upper or lowercase sections of code, and more. But you're not
limited to just built-in functionality. You can define entirely new functions
of code and reference them just like anything else.

## Putting It All Together

What follows are a few elisp functions I have defined and a snippet that makes
use of them. This snippet will generate the skeleton for a PHP class, including
the namespace at the top. As all of the building blocks have been covered
earlier in this post, I'll just briefly touch on each portion.

{% highlight lisp %}
(defun find-git-repo (dir)
  (if (string= "/" dir)
      nil
    (if (file-exists-p (expand-file-name ".git/" dir))
        dir
      (find-git-repo (expand-file-name "../" dir)))))
{% endhighlight %}

This function starts looking within a specified directory for the existing of a
.git directory, which would signify we are at the top level of a git
repository. If it doesn't find it, it keeps looking up the directory structure
until it either succeeds, or reaches the root. If it finds it, we return that
directory. Otherwise, we'll return nil.

{% highlight lisp %}
(defun find-project-root ()
  (interactive)
  (if (ignore-errors (eproject-root))
      (eproject-root)
    (or (find-git-repo (buffer-file-name)) (file-name-directory (buffer-file-name)))))
{% endhighlight %}

Using our find-git-repo as a spring board, this function will determine any
project's root directory. As I use [eproject](https://github.com/jrockway/eproject) for a lot of my work,
I first check to see if the `eproject-root` variable is defined. If it isn't,
I'll fall back to looking for a git repo.

{% highlight lisp %}
(defun file-path-to-namespace ()
  (interactive)
  (let (
        (root (find-project-root))
        (base (file-name-nondirectory buffer-file-name))
        )
    (substring (replace-regexp-in-string "/" "\\" (substring buffer-file-name (length root) (* -1 (length base))) t t) 0 -1)
    )
  )
{% endhighlight %}

This function will take the current buffer path, find the project root it is
included in, and convert it into a PHP namespace with the portion of the path
containing the project root removed.

{% highlight text %}
# name : php-namespaced-class
# key : class
# --
<?php

/**
 * `(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`
 *
 * @author `(user-full-name)` `(if user-mail-address (concat "<" user-mail-address ">") "")`
 */

namespace `(file-path-to-namespace)`;

/**
 * ${1:Description}
 */
class `(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`
{
    $0
}
{% endhighlight %}

This final snippet hooks everything up. If I have a project at
`/var/www/html/`, and I create a file NewUtil.php in say `lib/utils/`, this
snippet will generate

{% highlight text %}
<?php

/**
 * NewUtil
 *
 * @author Matthew M. Keeler <keelerm@tortugas-llc.com>
 */

namespace lib\utils;

/**
 * New Util description goes here
 */
class NewUtil
{
}
{% endhighlight %}

As you can see, the power of YASnippet is incredible. Be sure to check out the
YASnippet repo for more examples of snippets that are included in the default
installation. If any of you are currently using it, I'd love to see some of the
snippets you use. And of course, if you know of ways to improve my examples,
please let me know! Happy coding.

 
