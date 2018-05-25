---
title: Errors and the workarounds frequently encountered when dealing with Haskell on Windows
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
subHeading: Quick-and-dirty checklist
author: Yuji Yamamoto
postedBy: <a href="http://the.igreque.info/">Yuji Yamamoto(@igrep)</a>
date: May 25, 2018
tags: Windows
language: en
...
---

This is the English version of [WindowsでHaskellを扱う時によく遭遇するエラーと対処法](https://haskell.jp/blog/posts/2017/windows-gotchas.html).  
The original article is the 4th article of [Haskell (その4) Advent Calendar 2017 (Japanese)](https://qiita.com/advent-calendar/2017/haskell4).


What I'm going to tell is summarized as [just one tweet (originally in Japanese)](https://twitter.com/igrep/status/938056578934042626):

> What I've learned:
>
> - chcp65001 if 'Invalid character'
> - rebuild if 'Permission Denied'
> - Don't mix Japanese characters in file paths.
> - Some libraries in C are available, and others are not.
>
> Perhaps they're helpful in other languages.

Let me add more details.

# chcp 65001 if "Invalid character"

You would have encountered this frequently, especially if you don't know how to avoid/fix this.  
Oh, it's caused again by building with hakyll!


```
> stack exec -- site rebuild
...
  [ERROR] preprocessed-site\posts/2017/01-first.md: hGetContents: invalid argument (invalid byte sequence)
```

The object called [`Handle`](https://www.stackage.org/haddock/lts-10.0/base-4.10.1.0/System-IO.html#t:Handle), used by GHC to read and write a file, knows its character encoding.


This resembles Ruby's [`IO`](https://ruby-doc.org/core-2.5.0/IO.html) and Perl's file handler.  
Both of them represent the "gateway" of data, and assigning character encoding to them enables us to handle the only, consistently encoded strings by converting the incoming data.  
In Haskell's type `Char`, the only default encoding is UTF-32 (is this the right name in this case?).


The character encoding assigned to a `Handle` by default depends on the locale settings of the OS: in Japanese Windows, Windows-31J (a.k.a CP932).  
But it's now soon becoming 2018 (when writing the original article). Most files you create should be in UTF-8 unless you write programs in notepad.exe[^notepad].  
It doesn't work to read a UTF-8 file as a Windows-31J file because they're very different encoding system.  
The `invalid byte sequence` error, shown at the head of this section, is caused by that inconsistency.  
Remember this kind of errors are often caused when reading or writing stdout/stdin, as well as plain files.


[^notepad]: Translator's note: In Japanese locale, notepad.exe saves the file in Windows-31J. This will be changed (into UTF-8) in the future release of Windows 10.

## Workaround

### If you encounter as a user

In many cases you can avoid these kind of errors by running the below command in advance.


```
> chcp 65001
> stack exec -- site rebuild
... Should work!
```

This command temporarily changes the character encoding in the current Command Prompt session.  
The number `65001` seems to stand for UTF-8.  
To roll it back, run `chcp 932`.


```
> chcp 932
```

It seems that the "932" of "CP932" is the same "932" entered here!


The `chcp` command is available in MSYS2's bash (Surprises me a little. How it works?).  
But you should know that `chcp` exists at `C:\Windows\System32\`, which MSYS2 users usually don't want to include in the `PATH`.  
The directory contains many incompatible commands whose names conflict with the tools loved by Unix people (e.g. `find.exe`)!


So I've dropped `C:\Windows\System32\` from `PATH` when using MSYS2.  
If you've done like me, run by full path:


```
/c/Windows/System32/chcp.com 932
```

### If it still doesn't work, or you're the developer of the libraries etc.

Unfortunately, the error can often persist even after running `chcp 65001`[^eta-20127].  
According to my guess, the `chcp 65001` command doesn't affect the grandchild processes of the Command Prompt (or bash etc.) on which the `chcp` is run (i.e. the child processes of the command you enter).

[^eta-20127]: By the way, when I once tried to build the compiler of [Eta](http://eta-lang.org/), (as far as I remember) `chcp 65001` didn't fix the problem, but `chcp 20127` did.  
As `chcp 20127` switches into US-ASCII, I suspect the local environment of the developer of Eta is US-ASCII...

If the error still happens you can either report to the developer, or fix it yourself!  
When reporting; asking the developer to run after doing `chcp 932` could help him/her reproduce the bug (Sorry, I've never tried it).  
When fixing by yourself, perhaps the best and most certain way would be to switch the character encoding of the `Handle` object.


This problem is caused by the inconsistency between the `Handle`\'s character encoding and the encoding of the bytes that are actually transferred. So switching into the proper encoding should fix it.  
If the error happens when reading/writing a common UTF-8 file via the `Handle`, writing like below can avoid it:


```haskell
import System.IO (hSetEncoding)
import GHC.IO.Encoding (utf8)

hSetEncoding handle utf8
```

As a bonus, I'll show you an example of how [I myself addressed a problem caused by the standard output (or standard error output), and fixed a bug in haddock](https://github.com/haskell/haddock/pull/566).
In short, it can at least suppress the error to paste the code below before your program uses the `Handle` (Copied from [this commit](https://github.com/haskell/haddock/pull/566/commits/855118ee45e323fd9b2ee32103c7ba3eb1fbe4f2)).


```haskell
{-# LANGUAGE CPP #-}

import System.IO (hSetEncoding, stdout)

#if defined(mingw32_HOST_OS)
import GHC.IO.Encoding.CodePage (mkLocaleEncoding)
import GHC.IO.Encoding.Failure (CodingFailureMode(TransliterateCodingFailure))
#endif

...

#if defined(mingw32_HOST_OS)
  liftIO $ hSetEncoding stdout $ mkLocaleEncoding TransliterateCodingFailure
#endif
```

CPP macros to `import` modules only available on Windows makes this code hard to read, so let's cut out the verbose part:


```
hSetEncoding stdout $ mkLocaleEncoding TransliterateCodingFailure
```


Here're the details:  
First of all, `hSetEncoding` is the function to change the `Handle`'s character encoding, as I referred before.  
Then `stdout` is the `Handle` for the standard output as its name.  
The last function call `mkLocaleEncoding TransliterateCodingFailure` returns a character encoding object for the current Windows' character encoding (i.e. `chcp`ed character encoding), configured as "Even if the `Handle` detects any characters which can't be converted into/from a Unicode character, don't raise an error, convert it into some likable character instead.".  

As the result of the `hSetEncoding` above, and the current character encoding is Windows-31J, the character used in the compilation error of GHC:

```
↓This character
• No instance for (Transformation Nagisa CardCommune_Mepple)
↑
```

is converted into


```
? No instance for (Transformation Nagisa CardCommune_Mepple)
```

the question mark. Yeah, this is the "?" I bet most users of GHC on Japanese Windows have seen at least once 😅  
This makes me guess GHC executes `hSetEncoding stderr $ mkLocaleEncoding TransliterateCodingFailure` by default before printing out the compilation error.  
Anyway, it's good that the program doesn't abort due to the error!


As the last note of this section: Read [the document of GHC.IO.Encoding](https://hackage.haskell.org/package/base-4.10.1.0/docs/GHC-IO-Encoding.html) for the details of how GHC handles various character encodings.  

# Rebuild if "Permission Denied"

I've made the first section too long for "Quick-and-dirty checklist", but I'll tell you in short from this section.  
We often encounter some errors like "Permission Denied", "Directory not empty" and similar ones when running `stack build`, `ghc`, `elm-make`, and any other commands written in Haskell.  
To tell the truth, I'm completely not sure of the cause, but those errors disappear by running the same command several times.  
The key is to repeat many times. Never give up only by once or twice 😅  
Turning off your antivirus software's scanning of the problematic directory, Dropbox's synchronisation, etc. might also fix such errors.


# Try hard to build libraries in C...

On Windows, it frequently troubles us to install libraries which depend on libraries written in C (registered as `lib***` in your OS's package manager).  
But this is not the case only for Haskell.


The way to fix depends on the case, so let me give you some examples as external links (Sorry, all pages are written in Japanese!).


- HDBC-sqlite3:
    - [Windows版stackでもHDBC-sqlite3をビルドする - Qiita](https://qiita.com/igrep/items/d947ab871eb5b20b57e4)
    - [MSYS2でHDBC-sqlite3をコンパイル - 北海道苫小牧市出身の初老PGが書くブログ](http://hiratara.hatenadiary.jp/entry/2017/01/29/110100)
- [Haskell - Haskellにてstackでiconvパッケージを利用する方法【Windows環境】(102462)｜teratail](https://teratail.com/questions/102462)

That's all!  
Then, Happy Hacking in Haskell on Windows 10!! I don't know WSL!🏁🏁🏁
