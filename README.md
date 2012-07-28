## What «Loh» is

[![Build Status](https://secure.travis-ci.org/dmalikov/loh.png?branch=master)](http://travis-ci.org/dmalikov/loh)

*Loh* is LastFM scrobbler based on [liblastfm](https://github.com/supki/haskell-liblastfm).
*Loh* is available to scrobble track info from many players in parallel.

## How to start
At first, you need to allow access from Loh to your LastFM account. *Loh* suggest to do it at first time.

    $> loh
    ~/.lohrc seems to be missing
    You need correct session key to use loh properly.
    Authorize your token: http://www.last.fm/api/auth/?api_key=34a538d1ce307a257c695bcc7e031392&token=b6994737fe0dc4657b22632746b1769e
    And then press any key.

Then you should configure Loh by adding some player names in `~/.lohrc`.

    $> loh
    loh: There is nothing to scrobble!
    Add some players to your config file.

After that *loh* is ready to use.

## Loh-db
Loh-db is an utility to manage tracks that failed scrobbling. For example, you are offline, and Loh said:

    2012-06-30 [22:17:10]: [Mpd] now playing "The Legendary Pink Dots - King of a Small World" failed
    2012-06-30 [22:18:40]: [Mpd] scrobbling "The Legendary Pink Dots - King of a Small World" failed 
    2012-06-30 [22:18:40]: [Mpd] stored in DB "The Legendary Pink Dots - King of a Small World"

Loh store unscrobbled track:

    $> loh-db list
    1 tracks stored:
    "The Legendary Pink Dots - King of a Small World" played June 30, 22:18

So when you get internet connection you can scrobble it with `loh-db push`:

    $> loh-db push
    (1/1) scrobbled "The Legendary Pink Dots - King of a Small World", done
    Total: DB pushing is done

And db is empty now:

    $> loh-db list
    0 tracks stored:
