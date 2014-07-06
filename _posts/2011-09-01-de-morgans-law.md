---
layout: post
title:  "Negation and De Morgan's Law"
date:   2011-09-01 00:00
categories: programming
---

Undoubtedly, your grade school grammar teacher has scolded you at one point or
another for your inadvertent use of a double negative.  Statements such as “I
don’t know nothing about physics”, while grammatically incorrect, are often
semantically inaccurate as well.  While the previous statement would commonly
be understood to mean the person posses no knowledge of physics, the actual
statement implies the opposite, that the preposition “I know nothing about
physics” is incorrect.  Certainly your teacher offered sound advice and one
would be wise to heed such instruction.

Unfortunately, when programming such double negation is quite common and often
leads to disastrous ends.  For example, who would qualify for the below
condition?

{% highlight php %}
<?php
if( !($student->age != 18 && !$student->enrolled('Physics')) ) { ... }
{% endhighlight %}

We want someone who is not both not 18 and not enrolled in physics. Simple,
right?

Granted, at the time this snippet was written, it might have been well
understood by the author.  But 6 months later when this line needs altered by a
different developer, precious time is wasted deciphering this twisted logic.

This is where [De Morgan’s Laws](http://en.wikipedia.org/wiki/De_Morgan) come
in handy. From Wikipedia:

>“The negation of a conjunction is the disjunction of the negations.” \\
>“The negation of a disjunction is the conjunction of the negations.“

Great, so what does that mean? Basically, it means that if you have two
conditions — `p` and `q` — and you apply the negation to the statement `p and
q`, this is logically equivalent to `!p or !q`. Similarly, if we have the
statement `p or q` to which we apply the negation operation, this is logically
equivalent to `!p and !q`.

The easiest way to think about this mentally is that when we apply a negation
across a statement, we prepend each preposition ( either `p` or `q` ) with the
`not` operator, and every time we encounter the logical operators `and` or
`or`, we toggle them

Thus, `! ( ( p or q ) and ( r or s ) )` implies `( (!p and !q) or (!r and !s)
)`.

Using De Morgan’s law, let’s simplify the logic of our initial if condition.

Let us assume the preposition `p` represents the condition where the student’s
age is 18 and `q` represents the condition where the student is enrolled in
physics. Given this, our initial condition can be written in a more compact
form as

{% highlight c %}
if( !(!p && !q) ) {
{% endhighlight %}

After applying De Morgan’s Law, we have

{% highlight c %}
if( !!p || !!q ) {
{% endhighlight %}

The negation of the negation of `p` is `p`, so the above reduces to

{% highlight c %}
if( p || q ) {
{% endhighlight %}

Substituting `p` and `q` for the original conditions, we arrive at the greatly
simplified

{% highlight php %}
<?php
if ( $student->age == 18 || $student->enrolled('Physics') ) {
{% endhighlight %}

which is much more readily understood.

In order to convince you that the two statements are logically equivalent, a
[truth table](http://en.wikipedia.org/wiki/Truth_table) is provided below. Note
that the second to last column contains the results of the original condition,
and the last column is the reduced condition.

{% highlight text %}
| p | q | !p | !q | !p && !q | !(!p ^ !q) | p v q |
|---+---+----+----+----------+------------+-------|
| 1 | 1 |  0 |  0 |        0 |          1 |     1 |
| 1 | 0 |  0 |  1 |        0 |          1 |     1 |
| 0 | 1 |  1 |  0 |        0 |          1 |     1 |
| 0 | 0 |  1 |  1 |        1 |          0 |     0 |
{% endhighlight %}

As you can see, De Morgan’s Law is a powerful tool for managing conditional
complexity in code. I hope this hasn’t not helped.
