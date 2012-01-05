gwist
=====

gwist is a client to post your code to gist.github.com and tweet with the URL.

Installation
------------

For Ubuntu:

Copy http://shugo.net/tmp/gwist to your PATH.

For other platforms:

Build from the source on github.

    $ git clone git://github.com/shugo/gwist.git
    $ cd gwist
    $ vi Gwist/Twitter.hs
    # Remove the line importing Gwist.Secret, and replace
    # Secret.twitterConsumer{Key,Secret} with your values
    $ ghc --make -O gwist.hs

Usage
-----

First, you have to get PIN from Twitter:

    $ gwist 'hello world in haskell' hello.hs
    visit http://twitter.com/oauth/authorize?oauth_token=...
    enter PIN: <PIN>
    created a gist at https://gist.github.com/...
    posted a tweet

The Twitter credential is saved in ~/.gwistrc.

Use the -u option to authenticate as a Github user:

    $ gwist -u shugo 'hello and bye in haskell' hello.hs bye.hs
    Github password: <password>
    ...

The Github credential is also saved in ~/.gwistrc.

You can post multiple files:

    $ gwist 'hello and bye in haskell' hello.hs bye.hs

Ask gwist --help for other options.
