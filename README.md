## What «Loh» is

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
