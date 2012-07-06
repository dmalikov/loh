## What «Loh» is

[![Build Status](https://secure.travis-ci.org/dmalikov/loh.png?branch=master)](http://travis-ci.org/dmalikov/loh)

Loh is LastFM scrobbler based on [liblastfm](https://github.com/supki/haskell-liblastfm).
Loh is available to scrobble track info from many players in parallel.

## How to start
At first, you need to allow access from Loh to your LastFM account:

    $> loh-get-session 
    Authorize your token: http://www.last.fm/api/auth/?api_key=34a538d1ce307a257c695bcc7e031392&token=98df79b300d2afeb0ff77f0f21e6a980
    # press «Yes, allow access» button on the lastFM page
    # press <Enter>
    Session key is successfuly written to /home/user/.lastfm.conf

Then you should configure Loh. Add name of the players that you wanna Loh looking for:

    $> echo "Mpd" >> ~/.lohrc
    $> echo "Mocp" >> ~/.lohrc

Start Loh:

    $> loh

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
