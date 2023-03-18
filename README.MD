Audiobook Toolbox
===============
This repo contains some simple scripts for helping to deal with m4b audiobooks. This is mostly for personal use, but it is possible others might find something of value here. Included are some script files I've found handy, as well as some ramblings to help me remember how things work. Most likely anyone finding this will be better served by the far more extensive [m4b-Tool](https://github.com/sandreas/m4b-tool). This is really a wrapper for that wrapper that mostly suits my own purposes.

> 🛈 **Note:** m4b and m4a are the same format, with m4b being used for audiobooks by convention only. You can freely rename an m4a file to m4b and visa versa.

Requirements
-----
This tool box is just a fairly basic wrapper to for some other much more powerful tools that are required. All the scripts here can be used on Windows using the Linux Subsystem.
 * ffmpeg
 * mp4V2-Utils
 * gpac
 * mp3val

The requirements call be installed as follows:
 ```
 sudo apt install ffmpeg mp4v2-utils gpac
 ```

Audiobook
===============
Central up for all the tools.

```
audiobooks ACTION [ACTION PARAMETERS]

Actions:
  -t, --trim              [audiobook.m4b] [Length]        Trim the [audiobook.m4b] to [Length] (HH:MM:SS.000). This will copy to a new file as some meta data will be lost.
  -m, --merge                                             Merge all files in this folder into a single file with the same name as the folder.
  -c, --chapters                                          Process audiobook chapters - use 'audiobooks -c -h' for details.
```

Chapters
===============
Handling the adding and removing of chapters to m4b files.

```
chapters ACTION [ACTION PARAMETERS]

Actions:
  -a, --add               [chapters.txt] [audiobook.m4b]  Add the chapters in [chapters.txt] to the audiobook in quicktime format.
  -d, --dump              [audiobook.m4B]                 Dump the chapters from the audiobook to [audiobook.chapters.txt]

  -ac, --audacity-clean   [labels.txt]                    Remove unedited Audacity labels (ones with a "S" Title) and align start and end points.
  -ad, --audacity-dump    [labels.txt]                    Covert Audacity labels into quicktime format.
```

chapterFiles
-----
Some shops, such as the Google Play Store, provide m4a/m4b files without chapters for their audiobooks.

Locating the chapter points is manual process unfortunately. Audacity has a silence detector which can be a good starting point (I recommend searching for silence of 3 seconds for longer, and placing the label 1 second before the end of the silence). It is possible to export the labels generated as a tab separated text file which I have included tools for processing (it can also be easily fiddled with in excel or similar). Once timestamps for each chapter are located you can use them to generate a chapter file in the quicktime format:

```
00:00:00 Track 1 Title
00:10:00 Track 2 Title
```

Once you have your chapter file you can use `chapters` to import the chapters.

I've also found even audible often has very poor chaptering, and I spend a disproportionate amount of time correctly chaptering things.

I hope some of these might prove useful to other people, but they kind of rely on your have an identical export to me, and the more I use audible and such the more I realise how often these things are modified over time. They may still prove useful as a starting point. I have many manually chapters books before I started backing up to github, I may start to upload them at some point if anyone other than me ever uses this repo.