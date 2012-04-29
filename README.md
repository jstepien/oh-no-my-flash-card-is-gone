# Oh no, my flash card is gone

> _TL;DR_: The filesystem on my camera's memory card turned into a bowl of
> [bigos][bigos]. I got my JPEGs back, though.

What you're reading is more of a blog entry than a code repository, but since I
haven't set up a blog yet this seems to be a quite appropriate medium to share
this story.

## Fear, uncertainty and doubt

Earlier today I turned my photo camera on only to get informed that there's
something wrong with the memory card. _Would you like to get it formatted?_
asked the device politely. _Over my dead body!_, I heartily replied. The card
was full of newly taken pictures which would be difficult if not next to
impossible to retake. Losing them was not an option.

I plugged the camera into my laptop and found out that `mount` refuses to
cooperate with the filesystem on the card. I ran `fsck`. The result wasn't
promising.

    $ fsck.vfat /dev/sdb1
    dosfsck 3.0.12, 29 Oct 2011, FAT32, LFN
    Logical sector size (1792 bytes) is not a multiple of the physical sector size.

I compared it with results for a valid filesystem.

    $ dd if=/dev/zero of=valid bs=1M count=10
    $ mkfs.vfat valid
    $ fsck.vfat valid
    dosfsck 3.0.12, 29 Oct 2011, FAT32, LFN
    fs: 0 files, 0/5101 clusters

Alright, so there was definitely something wrong with the FAT32 on the camera.
I copied contents of the card to the laptop and let the `file` tool do the job
of determining contents of copied data.

    $ dd if=/dev/sdb1 of=card
    $ file card
    card: data
    $ file valid
    valid: x86 boot sector, mkdosfs boot message display, code offset 0x3c, ...

As you can see, unlike the byte stream from the flash card, the valid filesystem
was correctly identified. In case you're not aware, `data` in terms of the
`file` tool is a synonym for _no clue_.

In order to take a closer look I printed a handful of leading bytes in a
human-readable format and compared them with the valid filesystem's header.

    $ xxd card | head
    0000000: f8ff ff7f ffff 4f63 0500 0600 0700 0800  ......Oc........
    0000010: 0900 0a00 0b00 0c00 0d00 0e00 0f00 1000  ................
    0000020: 1100 1200 1300 1400 1500 1600 1700 1800  ................
    0000030: 1900 1a00 1b00 1c00 1d00 1e00 1f00 2000  .............. .
    0000040: 2100 2200 2300 2400 2500 2600 2700 2800  !.".#.$.%.&.'.(.
    0000050: 2900 2a00 2b00 ffff 2d00 2e00 2f00 3000  ).*.+...-.../.0.
    0000060: 3100 3200 3300 3400 3500 3600 3700 3800  1.2.3.4.5.6.7.8.
    0000070: 3900 3a00 3b00 3c00 3d00 3e00 3f00 4000  9.:.;.<.=.>.?.@.
    0000080: 4100 4200 4300 4400 4500 4600 4700 4800  A.B.C.D.E.F.G.H.
    0000090: 4900 4a00 4b00 4c00 4d00 4e00 ffff 5000  I.J.K.L.M.N...P.
    $ xxd valid | head
    0000000: eb3c 906d 6b64 6f73 6673 0000 0204 0400  .<.mkdosfs......
    0000010: 0200 0200 50f8 1400 2000 4000 0000 0000  ....P... .@.....
    0000020: 0000 0000 0000 299a b7f4 5920 2020 2020  ......)...Y
    0000030: 2020 2020 2020 4641 5431 3620 2020 0e1f        FAT16   ..
    0000040: be5b 7cac 22c0 740b 56b4 0ebb 0700 cd10  .[|.".t.V.......
    0000050: 5eeb f032 e4cd 16cd 19eb fe54 6869 7320  ^..2.......This
    0000060: 6973 206e 6f74 2061 2062 6f6f 7461 626c  is not a bootabl
    0000070: 6520 6469 736b 2e20 2050 6c65 6173 6520  e disk.  Please
    0000080: 696e 7365 7274 2061 2062 6f6f 7461 626c  insert a bootabl
    0000090: 6520 666c 6f70 7079 2061 6e64 0d0a 7072  e floppy and..pr


Lovely. Apparently the header of the filesystem got corrupted.

## Challenge accepted

Having both a reasonable excuse to lower priorities of other tasks and an
appetite for a bit of hacking I rolled up my sleeves and dived into the problem.

I assumed that the header might be the only part that got corrupted and majority
of the data might be still intact. In order to increase my confidence in this
hypothesis I checked for EXIF date/time headers of photos I took in the morning
on April 24th. The results were promising.

    $ strings card | grep 2012.04.24 | head
    2012:04:24 08:55:20
    2012:04:24 08:55:20
    2012:04:24 08:55:20
    2012:04:24 08:57:48
    2012:04:24 08:57:48
    2012:04:24 08:57:48
    2012:04:24 09:03:48
    2012:04:24 09:03:48
    2012:04:24 09:03:48
    2012:04:24 09:03:58

On Wikipedia I've learned that [JPEG][jpeg] files start with a magic number
`0xffd8`. I thought that by scanning the dump of the card for this 2 byte
sequence I would be able to find exact offsets at which JPEG files start.
In order to achieve it I wrote a piece of Haskell which efficiently got the task
done. You can find the code in [`scan.hs`][scan].

    $ ./scan < card > offsets
    $ head -5 offsets
    7085
    7597
    71085
    71597
    169472

Did it work as expected? By using a [shamefully simple tool][chunk] for
extracting a fragment of a file I let `file` and `identify` programmes to
determine what could be found at a given offset.

    $ ./chunk 169472 10000000 < card | file -
    /dev/stdin: JPEG image data, EXIF standard 2.21
    $ ./chunk 169472 10000000 < card | identify -
    -=>/tmp/magick-hiyb0uK9 JPEG 2048x1536 2048x1536+0+0 8-bit DirectClass ...

A natural question which comes to mind at this point is presence of false
positives. Probability of random occurrence of `0xffd8` seems relatively low,
however given the magnitude of the input size we have to consider presence of
such misleading cases.

Having candidates for beginnings of JPEG files I combined a standard JPEG
decoder `djpeg`, [ImageMagick][im] and [ExifTool][et] in a third piece of
Haskell. The idea was following. Assuming 10MB as an upper bound for rescued
JPEG's file size, take such number of bytes from the card starting at an offset
determined by the `scan` tool. Pipe it into `djpeg`. If it chokes and returns
non-zero exit code it was either a false positive or a broken file. Otherwise
pipe the decoded data to `convert` to recode it back as a JPEG. Finally use
`exiftool` to extract back EXIF metadata which got lost in translation. Repeat
for all offsets.

The source is available in [`rescue.hs`][rescue].

    $ ./rescue offsets < card

After barely half an hour all my photos were waiting for me in the current
directory, with just a couple having minor glitches due to malformed input data.

## Conclusions and future work

The task wasn't as demanding as it initially appeared to me. I imagine that
given enough knowledge about format of potentially lost files and proper
decoding software it could be implemented for virtually any other data format.

There's still a lot of room for improvement, especially performance-wise but
also on the plane of quality. As the code attached to this file solved my
problem in a reasonable time frame I didn't find it worthwhile to work on
performance issues.

There are clearly some obvious areas to be explored though. The task performed
by `rescue.hs` shouldn't be difficult to parallelise.  Decoding JPEGs only to
get them encoded once again is a waste of both time and quality; there might be
a better way to achieve the same goal. ExifTool takes a quarter of a second to
start so choosing a more efficient way of extracting EXIF metadata might be a
good idea. `djpeg` and `exiftool` can run in parallel. Error handling is sloppy.
False positives and broken JPEGs might require a different treatment. `scan.hs`
could be optimised to take an advantage of more efficient pattern-matching
techniques. The list goes on.

On a final note, I suspect I could have simply searched the web for an existing,
mature and tested solution designed for the problem I encountered. It would take
less time and it would surely recover more files than my method did. However I
guess we can agree that it would be _easy_ and the easy way is never rewarding.

To wrap it up, before you'll find yourself in a similar situation remember to:

  - avoid panic,
  - get familiar with command line utilities your OS provides,
  - and learn Haskell (I simply couldn't resist).

--

As a non-native English speaker I always welcome remarks regarding my wording.

(C) 2012 Jan Stępień.
This file can be distributed under terms of [CC BY-NC-SA 3.0][cc].
Unless otherwise stated, all code in this repository is published under terms of
the [MIT License][mit].


  [bigos]: https://en.wikipedia.org/w/index.php?title=Bigos&oldid=489526950
  [jpeg]: https://en.wikipedia.org/w/index.php?title=JPEG&oldid=489286037
  [scan]: https://github.com/jstepien/oh-no-my-flash-card-is-gone/blob/master/scan.hs
  [chunk]: https://github.com/jstepien/oh-no-my-flash-card-is-gone/blob/master/chunk.hs
  [im]: http://www.imagemagick.org/
  [et]: http://www.sno.phy.queensu.ca/~phil/exiftool/
  [rescue]: https://github.com/jstepien/oh-no-my-flash-card-is-gone/blob/master/rescue.hs
  [cc]: http://creativecommons.org/licenses/by-nc-sa/3.0/
  [mit]: https://en.wikipedia.org/w/index.php?title=MIT_License&oldid=488497962#License_terms
