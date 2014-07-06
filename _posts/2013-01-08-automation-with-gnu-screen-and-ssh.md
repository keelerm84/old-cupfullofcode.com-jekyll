---
layout: post
title:  "Automation with GNU Screen and SSH"
date:   2013-01-08 13:38
categories: utilities
---

As a developer, I tend to prefer automation whenever possible. Hours of your
life are lost in the minutes spent doing that which could easily be
scripted. The classic rule of thumb I try to follow is "if you have to do it
twice, automate it."

At my job, I have to maintain multiple versions of the product, each of which
are hosted on different virtual machines. When I am ready to start work on a
new feature to an existing version, I have to fire up my code editor, connect
multiple terminals to the system so I can tail various log files, and open a
shell for git. As one can imagine, this is rather time consuming. There is
simply no way I'm doing this by hand every day. To address this, I use a
combination of SSH keys and configs, GNU screen and a small alias.

There are tons of documentation on the web about the use of SSH keys and config
files. If you are unfamiliar with these topics, you can read about SSH keys at
[http://paulkeck.com/ssh/](http://paulkeck.com/ssh/) and config files at
[http://nerderati.com/2011/03/simplify-your-life-with-an-ssh-config-file/](http://nerderati.com/2011/03/simplify-your-life-with-an-ssh-config-file/). Since
the main purpose is to show how my tools fit together, I'll just state that
every virtual server I connect to has a copy of my SSH key, and that I have a
config file defined with an example snippet shown below:

{% highlight sh %}
Host 19
HostName 19.virtualserver
Port 12345
User developer
{% endhighlight %}

Each host corresponds to a server hosting a specific version, so instead of
issuing ssh -p12345 developer@19.virtualserver, I can simply type ssh 19. With
the added bonus SSH keys bring to the party, connecting is quick and
simple. But what about all those connections I mentioned earlier?

While it is true I could open multiple terminal tabs, I actually prefer to work
within [GNU Screen](http://www.gnu.org/software/screen/). Once again, the
details of using screen are outside the scope of this entry, but check out this
post
[http://www.kuro5hin.org/story/2004/3/9/16838/14935](http://www.kuro5hin.org/story/2004/3/9/16838/14935)
for some general information.

One thing you don't hear a lot about is scripting with screen. With the
powerful options provided, you can easily start a screen, launch multiple named
tabs, and execute any command of your choosing. I have a screen config for each
product version I manage. Version 1.9's config is shown below:

{% highlight sh %}
# Start screen in detached mode (-d -m), and give it a session name ('1-9') to avoid
# confusion when communicating with this session later as we add commands
screen -d -m -S 1-9

# Run a command in the current screen window (edit some file for example)
screen -S 1-9 -p 0 -X title Git

# Create a new window, and run a command in that window:
screen -S 1-9 -p 0 -X screen -t Code emacsclient -nw -c
screen -S 1-9 -p 0 -X screen -t Application ssh 19 'tail -F /var/www/product/logs/error.log'
screen -S 1-9 -p 0 -X screen -t Apache ssh 19 'tail -F /var/log/httpd/error.log'
screen -S 1-9 -p 0 -X screen -t Remote ssh 19
{% endhighlight %}

Executing this script will create a new screen session with five tabs, giving
me a tab for git, one for my editor of choice, two actively tailing various log
files, and a third connected to the remote system so I can easily navigate and
check out other bits of the system as needed. This script relies heavily on my
aforementioned use of SSH configs and keys. With the keys copied over, I don't
have to provide a password for the three tabs that connect to the development
box. And if any of the connection details for any of the virtual machines
change, I don't have to touch this script, and can instead make the
modification in one place.

And finally, just to prevent a few more keystrokes, I also have defined aliases
in my .alias file like the following:

{% highlight sh %}
alias '19'='cd ~/Projects/version19; ~/.screen.d/1.9 && screen -r 1-9'
{% endhighlight %}

Now, when I'm ready to go to work, I simply open a terminal, type 19, and I'm
automatically thrown into screen, running the applications and viewing the logs
that I almost always need, ready to hit the ground coding with absolute minimal
effort.
